# refs: van Walraven paper: https://pubmed.ncbi.nlm.nih.gov/19433995/

require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)
library(comorbidity)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=xsw-000-sp09;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)


report_start <- '2021-06-01'
report_end <- '2024-06-01'
pre_window_days <- 60

days_outcome_window <- 30

string_ecds<-glue::glue("select 
                        NHS_Number,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Departure_Date,
                        Departure_Time,
                        Conclusion_Date,
                        Conclusion_Time,
                        Site_Name,
                        Acuity_Desc,
                        Index_Of_Multiple_Deprivation,
                        Stated_Gender,
                        HES_Age_At_Arrival,
                        Arrival_Mode_Desc,
                        Arrival_Category_Desc,
                        Attendance_Source_Desc,
                        destination_desc,
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date >= '{ymd(report_start) - ddays(pre_window_days)}'
      and destination_desc not in ('Emergency department discharge to emergency department short stay ward',
      'Emergency department discharge to same day emergency care service',
      'Emergency department discharge to ambulatory emergency care service',
      'Admission to the mortuary')
      ")

dat_ecds_raw<-sqlQuery(con,string_ecds)
saveRDS(dat_ecds_raw, "data/dat_ecds_raw_new.RDS")
dat_ecds_raw <- readRDS("data/dat_ecds_raw_new.RDS")

dat_ecds<-dat_ecds_raw %>%
  mutate(NHS_Number = as.character(NHS_Number)) %>%
  mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
  mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  select(
    nhs_number = NHS_Number,
    imd = Index_Of_Multiple_Deprivation,
    sex = Stated_Gender,
    age = HES_Age_At_Arrival,
    arrival_dt,
    initial_dt,
    treatment_dt,
    dta_dt,
    departure_dt,
    conclusion_dt,
    site = Site_Name,
    Acuity_Desc,
    Arrival_Category_Desc,
    Arrival_Mode_Desc
  ) %>%
  rowwise() %>%
  mutate(departure_dt=max(dta_dt,departure_dt, conclusion_dt, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(dta_time = interval(dta_dt, departure_dt)/dminutes(1)) %>%
  mutate(ed_time = interval(arrival_dt, departure_dt)/dminutes(1)) %>%
  mutate(breach = (interval(arrival_dt, departure_dt)/dminutes(1)) > 4*60) %>%
  mutate(date=floor_date(departure_dt,"day")) %>%
  mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                               TRUE~"major")) %>%
  group_by(nhs_number, arrival_dt) %>%
  mutate(arr_id = cur_group_id()) %>%
  ungroup()

dat_ecds_perf <- dat_ecds %>%
  group_by(site, arrival_hour = floor_date(arrival_dt, unit = "hour")) %>%
  summarise(perf = 1-mean(breach))


# compute the number of previous ed attendances/admissions from index event
# index here is the FIRST ed attendance with admission in the study period
dat_ecds_n_ed <- 
  dat_ecds %>%
  select(nhs_number, arrival_dt, dta_dt) %>%
  mutate(
    arrival_day = floor_date(arrival_dt, "days"),
    dta_day = floor_date(dta_dt, "days")
    
  ) %>%
  distinct() %>%
  filter(!is.na(nhs_number)) %>%
  arrange(nhs_number, arrival_day) %>%
  group_by(nhs_number, arrival_day) %>%
  filter(arrival_dt == max(arrival_dt, na.rm = TRUE)) %>%
  group_by(nhs_number) %>%
  # remove patients that never DTA
  filter(!all(is.na(dta_dt))) %>%
  # earliest arrival with dta
  mutate(min_arr = arrival_day[which.min(dta_dt)]) %>%
  # remove ed attendances after the min DTA date
  filter(arrival_day <= min_arr) %>%
  mutate(arr_diff = (min_arr - lag(arrival_day))/ddays(1)) %>%
  mutate(n_ed = sum(na.omit(arr_diff) < pre_window_days)) %>%
  mutate(n_adm = sum(na.omit(arr_diff[!is.na(dta_dt)]) < pre_window_days)) %>%
  mutate(n_ed = ifelse(n_ed > 0, n_ed -1, 0)) %>%
  mutate(n_adm = ifelse(n_adm > 0, n_adm -1, 0)) %>%
  select(nhs_number, n_ed, n_adm) %>%
  distinct()


# get bnssg nhs nums for filtering
string_attr <- glue::glue("	select
	nhs_number
	,max(attribute_period) as max_date
	,count(attribute_period) as date_count
	from
	MODELLING_SQL_AREA.dbo.primary_care_attributes
	where attribute_period between '{report_start}' and '{report_end}'
	group by nhs_number")


dat_attr <- sqlQuery(con,string_attr) %>%
  mutate(nhs_number = as.character(nhs_number))

# saveRDS(dat_attr, "data/dat_attr.RDS")
dat_attr <- readRDS("data/dat_attr.RDS")

# arrivals for joining to sus
arrivals <- 
  dat_ecds %>%
  filter(!is.na(dta_time), nhs_number %in% unique(dat_attr$nhs_number)) %>%
  group_by(nhs_number, arr_id) %>%
  summarise(departure_dt = as.Date(departure_dt)[1]
  )

# SUS query (takes a long time, should prune the variables it is returning)
string_apc <- {glue::glue("SELECT 
                          [Process_ID]
      ,[GeneratedRecordIdentifier]
      ,[PBRspellID]
      ,[ReasonForAccess]
      ,[CDStype]
      ,[ProtocolIdentifier]
      ,[UpdateType]
      ,[BulkReplacementCDSgroup]
      ,[TestIndicator]
      ,[ApplicableDateTime]
      ,[CensusDate]
      ,[ExtractDateTime]
      ,[ReportPeriodStartDate]
      ,[ReportPeriodEndDate]
      ,[OrganisationCode_SenderOfTransaction]
      ,[OrganisationCodeTypeOfSender]
      ,[SubmissionDate]
      ,[CDSInterchangeID]
      ,[UniqueCDSidentifier]
      ,[LocalPatientIdentifier]
      ,[OrganisationCode_LocalPatientIdentifier]
      ,[OrganisationCodeType_LocalPatientIdentifier]
      ,[NHSnumber]
      ,[DateOfBirth]
      ,[BirthWeight]
      ,[LiveOrStillBirth]
      ,[CarerSupportIndicator]
      ,[LegalStatusClassificationOnAdmission_PsychiatricCensusOnly]
      ,[EthnicGroup]
      ,[MaritalStatus_PsychiatricCensusOnly]
      ,[NHSnumberTraceStatus]
      ,[WithheldIdentityReason]
      ,[Sex]
      ,[PregnancyTotalPreviousPregnancies]
      ,[NameFormatCode]
      ,[PatientName]
      ,[PersonTitle]
      ,[PersonNameSuffix]
      ,[AddressFormatCode]
      ,[PatientsAddress]
      ,[Postcode]
      ,[OrganisationCode_ResidenceResponsibility]
      ,[PCTofResidence]
      ,[OrganisationCodeType_PCTofResidence]
      ,[OSVClassificationatCDSActivityDate]
      ,[OverseasVisitorStatusClassification1]
      ,[OverseasVisitorStatusStartDate1]
      ,[OverseasVisitorStatusEndDate1]
      ,[OverseasVisitorStatusClassification2]
      ,[OverseasVisitorStatusStartDate2]
      ,[OverseasVisitorStatusEndDate2]
      ,[OverseasVisitorStatusClassification3]
      ,[OverseasVisitorStatusStartDate3]
      ,[OverseasVisitorStatusEndDate3]
      ,[OverseasVisitorStatusClassification4]
      ,[OverseasVisitorStatusStartDate4]
      ,[OverseasVisitorStatusEndDate4]
      ,[OverseasVisitorStatusClassification5]
      ,[OverseasVisitorStatusStartDate5]
      ,[OverseasVisitorStatusEndDate5]
      ,[AdministrativeCategory]
      ,[PatientClassification]
      ,[AdmissionMethod_HospitalProviderSpell]
      ,[DischargeDestination_HospitalProviderSpell]
      ,[DischargeMethod_HospitalProviderSpell]
      ,[SourceOfAdmission_HospitalProviderSpell]
      ,[DischargeToHospitalAtHomeServiceIndicator]
      ,[HospitalProviderSpellNumber]
      ,[StartDate_HospitalProviderSpell]
      ,[StartTime_HospitalProviderSpell]
      ,[DischargeDate_FromHospitalProviderSpell]
      ,[DischargeTime_HospitalProviderSpell]
      ,[EpisodeNumber]
      ,[FirstRegularDayNightAdmission]
      ,[LastEpisodeInSpellIndicator]
      ,[NeonatalLevelOfCare]
      ,[OperationStatus]
      ,[PsychiatricPatientStatus]
      ,[StartDate_ConsultantEpisode]
      ,[StartTime_Episode]
      ,[EndDate_ConsultantEpisode]
      ,[EndTime_Episode]
      ,[LengthOfStayAdjustment_Rehabilitation]
      ,[LengthOfStayAdjustment_SpecialistPalliativeCare]
      ,[CommissioningSerialNumber]
      ,[NHSserviceAgreementLineNumber]
      ,[ProviderReferenceNumber]
      ,[CommissionerReferenceNumber]
      ,[OrganisationCode_CodeOfProvider]
      ,[OrganisationCodeType_OfProvider]
      ,[OrganisationCode_CodeOfCommissioner]
      ,[OrganisationCodeType_OfCommissioner]
      ,[ConsultantCode]
      ,[MainSpecialtyCode]
      ,[TreatmentFunctionCode]
      ,[LocalSubSpecialtyCode]
      ,[Multi_ProfessionalOrMultidisciplinaryIndCode]
      ,[RehabilitationAssessmentTeamType]
      ,[DiagnosisSchemeInUse_ICD]
      ,[DiagnosisPrimary_ICD]
      ,[PresentOnAdmissionIndicatorDiag1]
      ,[Diagnosis1stSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag2]
      ,[Diagnosis2ndSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag3]
      ,[Diagnosis3rdSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag4]
      ,[Diagnosis4thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag5]
      ,[Diagnosis5thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag6]
      ,[Diagnosis6thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag7]
      ,[Diagnosis7thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag8]
      ,[Diagnosis8thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag9]
      ,[Diagnosis9thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag10]
      ,[Diagnosis10thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag11]
      ,[Diagnosis11thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag12]
      ,[Diagnosis12thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag13]
      ,[Diagnosis13thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag14]
      ,[Diagnosis14thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag15]
      ,[Diagnosis15thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag16]
      ,[Diagnosis16thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag17]
      ,[Diagnosis17thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag18]
      ,[Diagnosis18thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag19]
      ,[Diagnosis19thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag20]
      ,[Diagnosis20thSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag21]
      ,[Diagnosis21stSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag22]
      ,[Diagnosis22ndSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag23]
      ,[Diagnosis23rdSecondary_ICD]
      ,[PresentOnAdmissionIndicatorDiag24]
      ,[DiagnosisSchemeInUse_Read]
      ,[DiagnosisPrimary_Read]
      ,[Diagnosis1stSecondary_Read]
      ,[Diagnosis2ndSecondary_Read]
      ,[Diagnosis3rdSecondary_Read]
      ,[Diagnosis4thSecondary_Read]
      ,[Diagnosis5thSecondary_Read]
      ,[Diagnosis6thSecondary_Read]
      ,[Diagnosis7thSecondary_Read]
      ,[Diagnosis8thSecondary_Read]
      ,[Diagnosis9thSecondary_Read]
      ,[Diagnosis10thSecondary_Read]
      ,[Diagnosis11thSecondary_Read]
      ,[Diagnosis12thSecondary_Read]
      ,[Diagnosis13thSecondary_Read]
      ,[Diagnosis14thSecondary_Read]
      ,[Diagnosis15thSecondary_Read]
      ,[Diagnosis16thSecondary_Read]
      ,[Diagnosis17thSecondary_Read]
      ,[Diagnosis18thSecondary_Read]
      ,[Diagnosis19thSecondary_Read]
      ,[Diagnosis20thSecondary_Read]
      ,[Diagnosis21stSecondary_Read]
      ,[Diagnosis22ndSecondary_Read]
      ,[Diagnosis23rdSecondary_Read]
      ,[ProcedureSchemeInUse_OPCS]
      ,[PrimaryProcedure_OPCS]
      ,[PrimaryProcedureDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs1]
      ,[ProfessionalRegistrationIssuerCodeOpcs1]
      ,[ResponsibleAnaesthetistCodeOpcs1]
      ,[ResponsibleAnaesthetistRegBodyOpcs1]
      ,[Procedure2nd_OPCS]
      ,[Procedure2ndDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs2]
      ,[ProfessionalRegistrationIssuerCodeOpcs2]
      ,[ResponsibleAnaesthetistCodeOpcs2]
      ,[ResponsibleAnaesthetistRegBodyOpcs2]
      ,[Procedure3rd_OPCS]
      ,[Procedure3rdDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs3]
      ,[ProfessionalRegistrationIssuerCodeOpcs3]
      ,[ResponsibleAnaesthetistCodeOpcs3]
      ,[ResponsibleAnaesthetistRegBodyOpcs3]
      ,[Procedure4th_OPCS]
      ,[Procedure4thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs4]
      ,[ProfessionalRegistrationIssuerCodeOpcs4]
      ,[ResponsibleAnaesthetistCodeOpcs4]
      ,[ResponsibleAnaesthetistRegBodyOpcs4]
      ,[Procedure5th_OPCS]
      ,[Procedure5thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs5]
      ,[ProfessionalRegistrationIssuerCodeOpcs5]
      ,[ResponsibleAnaesthetistCodeOpcs5]
      ,[ResponsibleAnaesthetistRegBodyOpcs5]
      ,[Procedure6th_OPCS]
      ,[Procedure6thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs6]
      ,[ProfessionalRegistrationIssuerCodeOpcs6]
      ,[ResponsibleAnaesthetistCodeOpcs6]
      ,[ResponsibleAnaesthetistRegBodyOpcs6]
      ,[Procedure7th_OPCS]
      ,[Procedure7thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs7]
      ,[ProfessionalRegistrationIssuerCodeOpcs7]
      ,[ResponsibleAnaesthetistCodeOpcs7]
      ,[ResponsibleAnaesthetistRegBodyOpcs7]
      ,[Procedure8th_OPCS]
      ,[Procedure8thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs8]
      ,[ProfessionalRegistrationIssuerCodeOpcs8]
      ,[ResponsibleAnaesthetistCodeOpcs8]
      ,[ResponsibleAnaesthetistRegBodyOpcs8]
      ,[Procedure9th_OPCS]
      ,[Procedure9thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs9]
      ,[ProfessionalRegistrationIssuerCodeOpcs9]
      ,[ResponsibleAnaesthetistCodeOpcs9]
      ,[ResponsibleAnaesthetistRegBodyOpcs9]
      ,[Procedure10th_OPCS]
      ,[Procedure10thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs10]
      ,[ProfessionalRegistrationIssuerCodeOpcs10]
      ,[ResponsibleAnaesthetistCodeOpcs10]
      ,[ResponsibleAnaesthetistRegBodyOpcs10]
      ,[Procedure11th_OPCS]
      ,[Procedure11thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs11]
      ,[ProfessionalRegistrationIssuerCodeOpcs11]
      ,[ResponsibleAnaesthetistCodeOpcs11]
      ,[ResponsibleAnaesthetistRegBodyOpcs11]
      ,[Procedure12th_OPCS]
      ,[Procedure12thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs12]
      ,[ProfessionalRegistrationIssuerCodeOpcs12]
      ,[ResponsibleAnaesthetistCodeOpcs12]
      ,[ResponsibleAnaesthetistRegBodyOpcs12]
      ,[Procedure13th_OPCS]
      ,[Procedure13thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs13]
      ,[ProfessionalRegistrationIssuerCodeOpcs13]
      ,[ResponsibleAnaesthetistCodeOpcs13]
      ,[ResponsibleAnaesthetistRegBodyOpcs13]
      ,[Procedure14th_OPCS]
      ,[Procedure14thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs14]
      ,[ProfessionalRegistrationIssuerCodeOpcs14]
      ,[ResponsibleAnaesthetistCodeOpcs14]
      ,[ResponsibleAnaesthetistRegBodyOpcs14]
      ,[Procedure15th_OPCS]
      ,[Procedure15thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs15]
      ,[ProfessionalRegistrationIssuerCodeOpcs15]
      ,[ResponsibleAnaesthetistCodeOpcs15]
      ,[ResponsibleAnaesthetistRegBodyOpcs15]
      ,[Procedure16th_OPCS]
      ,[Procedure16thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs16]
      ,[ProfessionalRegistrationIssuerCodeOpcs16]
      ,[ResponsibleAnaesthetistCodeOpcs16]
      ,[ResponsibleAnaesthetistRegBodyOpcs16]
      ,[Procedure17th_OPCS]
      ,[Procedure17thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs17]
      ,[ProfessionalRegistrationIssuerCodeOpcs17]
      ,[ResponsibleAnaesthetistCodeOpcs17]
      ,[ResponsibleAnaesthetistRegBodyOpcs17]
      ,[Procedure18th_OPCS]
      ,[Procedure18thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs18]
      ,[ProfessionalRegistrationIssuerCodeOpcs18]
      ,[ResponsibleAnaesthetistCodeOpcs18]
      ,[ResponsibleAnaesthetistRegBodyOpcs18]
      ,[Procedure19th_OPCS]
      ,[Procedure19thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs19]
      ,[ProfessionalRegistrationIssuerCodeOpcs19]
      ,[ResponsibleAnaesthetistCodeOpcs19]
      ,[ResponsibleAnaesthetistRegBodyOpcs19]
      ,[Procedure20th_OPCS]
      ,[Procedure20thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs20]
      ,[ProfessionalRegistrationIssuerCodeOpcs20]
      ,[ResponsibleAnaesthetistCodeOpcs20]
      ,[ResponsibleAnaesthetistRegBodyOpcs20]
      ,[Procedure21st_OPCS]
      ,[Procedure21stDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs21]
      ,[ProfessionalRegistrationIssuerCodeOpcs21]
      ,[ResponsibleAnaesthetistCodeOpcs21]
      ,[ResponsibleAnaesthetistRegBodyOpcs21]
      ,[Procedure22nd_OPCS]
      ,[Procedure22ndDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs22]
      ,[ProfessionalRegistrationIssuerCodeOpcs22]
      ,[ResponsibleAnaesthetistCodeOpcs22]
      ,[ResponsibleAnaesthetistRegBodyOpcs22]
      ,[Procedure23rd_OPCS]
      ,[Procedure23rdDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs23]
      ,[ProfessionalRegistrationIssuerCodeOpcs23]
      ,[ResponsibleAnaesthetistCodeOpcs23]
      ,[ResponsibleAnaesthetistRegBodyOpcs23]
      ,[Procedure24th_OPCS]
      ,[Procedure24thDate_OPCS]
      ,[MainOperatingHealthcareProfessionalCodeOpcs24]
      ,[ProfessionalRegistrationIssuerCodeOpcs24]
      ,[ResponsibleAnaesthetistCodeOpcs24]
      ,[ResponsibleAnaesthetistRegBodyOpcs24]
      ,[ProcedureSchemeInUse_Read]
      ,[PrimaryProcedure_Read]
      ,[PrimaryProcedureDate_Read]
      ,[Procedure2nd_Read]
      ,[Procedure2ndDate_Read]
      ,[Procedure3rd_Read]
      ,[Procedure3rdDate_Read]
      ,[Procedure4th_Read]
      ,[Procedure4thDate_Read]
      ,[Procedure5th_Read]
      ,[Procedure5thDate_Read]
      ,[Procedure6th_Read]
      ,[Procedure6thDate_Read]
      ,[Procedure7th_Read]
      ,[Procedure7thDate_Read]
      ,[Procedure8th_Read]
      ,[Procedure8thDate_Read]
      ,[Procedure9th_Read]
      ,[Procedure9thDate_Read]
      ,[Procedure10th_Read]
      ,[Procedure10thDate_Read]
      ,[Procedure11th_Read]
      ,[Procedure11thDate_Read]
      ,[Procedure12th_Read]
      ,[Procedure12thDate_Read]
      ,[Procedure13th_Read]
      ,[Procedure13thDate_Read]
      ,[Procedure14th_Read]
      ,[Procedure14thDate_Read]
      ,[Procedure15th_Read]
      ,[Procedure15thDate_Read]
      ,[Procedure16th_Read]
      ,[Procedure16thDate_Read]
      ,[Procedure17th_Read]
      ,[Procedure17thDate_Read]
      ,[Procedure18th_Read]
      ,[Procedure18thDate_Read]
      ,[Procedure19th_Read]
      ,[Procedure19thDate_Read]
      ,[Procedure20th_Read]
      ,[Procedure20thDate_Read]
      ,[Procedure21st_Read]
      ,[Procedure21stDate_Read]
      ,[Procedure22nd_Read]
      ,[Procedure22ndDate_Read]
      ,[Procedure23rd_Read]
      ,[Procedure23rdDate_Read]
      ,[Procedure24th_Read]
      ,[Procedure24thDate_Read]
      ,[WardCodeatEpisodeStartDate]
      ,[WardSecurityLevelatEpisodeStartDate]
      ,[LocationClassAtEpisodeStartDate]
      ,[SiteCode_ofTreatmentAtEpisodeStartDate]
      ,[OrganisationCodeType_SiteCodeOfTreatment_AtStartOfEpisode]
      ,[IntendedClinicalCareIntensity_AtStartOfEpisode]
      ,[AgeGroupIntended_AtStartOfEpisode]
      ,[SexOfPatients_AtStartOfEpisode]
      ,[WardDayPeriodAvailability]
      ,[WardNightPeriodAvailability]
      ,[WardCode1]
      ,[WardSecurityLevel1]
      ,[LocationClass1]
      ,[SiteCode_ofTreatment1]
      ,[OrganisationCodeType_SiteCodeOfTreatment1]
      ,[IntendedClinicalCareIntensity1]
      ,[AgeGroupIntended1]
      ,[SexOfPatients1]
      ,[WardDayPeriodAvailability1]
      ,[WardNightPeriodAvailability1]
      ,[StartDate1]
      ,[StartTime_WardStay1]
      ,[EndDate1]
      ,[EndTime_WardStay1]
      ,[WardCode2]
      ,[WardSecurityLevel2]
      ,[StartTime_WardStay2]
      ,[EndTime_WardStay2]
      ,[WardCode3]
      ,[WardSecurityLevel3]
      ,[StartTime_WardStay3]
      ,[EndTime_WardStay3]
      ,[WardCode4]
      ,[WardSecurityLevel4]
      ,[StartTime_WardStay4]
      ,[EndTime_WardStay4]
      ,[WardCode5]
      ,[WardSecurityLevel5]
      ,[StartTime_WardStay5]
      ,[EndTime_WardStay5]
      ,[WardCode6]
      ,[WardSecurityLevel6]
      ,[StartTime_WardStay6]
      ,[EndTime_WardStay6]
      ,[WardCode7]
      ,[WardSecurityLevel7]
      ,[StartTime_WardStay7]
      ,[EndTime_WardStay7]
      ,[WardCode8]
      ,[WardSecurityLevel8]
      ,[StartTime_WardStay8]
      ,[EndTime_WardStay8]
      ,[WardCode9]
      ,[WardSecurityLevel9]
      ,[StartTime_WardStay9]
      ,[EndTime_WardStay9]
      ,[WardCode10]
      ,[WardSecurityLevel10]
      ,[StartTime_WardStay10]
      ,[EndTime_WardStay10]
      ,[WardCode11]
      ,[WardSecurityLevel11]
      ,[StartTime_WardStay11]
      ,[EndTime_WardStay11]
      ,[WardCode12]
      ,[WardSecurityLevel12]
      ,[StartTime_WardStay12]
      ,[EndTime_WardStay12]
      ,[WardCode13]
      ,[WardSecurityLevel13]
      ,[StartTime_WardStay13]
      ,[EndTime_WardStay13]
      ,[WardCode14]
      ,[WardSecurityLevel14]
      ,[StartTime_WardStay14]
      ,[EndTime_WardStay14]
      ,[WardCode15]
      ,[WardSecurityLevel15]
      ,[StartTime_WardStay15]
      ,[EndTime_WardStay15]
      ,[WardCode16]
      ,[WardSecurityLevel16]
      ,[StartTime_WardStay16]
      ,[EndTime_WardStay16]
      ,[WardCode17]
      ,[WardSecurityLevel17]
      ,[StartTime_WardStay17]
      ,[EndTime_WardStay17]
      ,[WardCode18]
      ,[WardSecurityLevel18]
      ,[StartTime_WardStay18]
      ,[EndTime_WardStay18]
      ,[WardCode19]
      ,[WardSecurityLevel19]
      ,[StartTime_WardStay19]
      ,[EndTime_WardStay19]
      ,[WardCode20]
      ,[WardSecurityLevel20]
      ,[StartTime_WardStay20]
      ,[EndTime_WardStay20]
      ,[WardCode21]
      ,[WardSecurityLevel21]
      ,[StartTime_WardStay21]
      ,[EndTime_WardStay21]
      ,[WardCode22]
      ,[WardSecurityLevel22]
      ,[StartTime_WardStay22]
      ,[EndTime_WardStay22]
      ,[WardCode23]
      ,[WardSecurityLevel23]
      ,[StartTime_WardStay23]
      ,[EndTime_WardStay23]
      ,[WardCode24]
      ,[WardSecurityLevel24]
      ,[StartTime_WardStay24]
      ,[EndTime_WardStay24]
      ,[WardCodeatEpisodeEndDate]
      ,[WardSecurityLevelatEpisodeEndDate]
      ,[LocationClassAtEpisodeEndDate]
      ,[SiteCode_ofTreatmentAtEpisodeEndDate]
      ,[OrganisationCodeTypeSiteCode_ofTreatmentAtEpisodeEndDate]
      ,[IntendedClinicalCareIntensityAtEpisodeEndDate]
      ,[AgeGroupIntendedAtEpisodeEndDate]
      ,[SexOfPatientsAtEpisodeEndDate]
      ,[WardDayPeriodAvailabilityAtEpisodeEndDate]
      ,[WardNightPeriodAvailabilityAtEpisodeEndDate]
      ,[GeneralMedicalPractitionerCodeOfRegisteredGMP]
      ,[PracticeCodeOfRegisteredGP]
      ,[OrganisationCodeTypeOfRegisteredGP]
      ,[ReferrerCode]
      ,[ReferringOrganisationCode]
      ,[OrganisationCodeTypeOfReferrer]
      ,[DirectAccessReferralIndicator]
      ,[AmbulanceIncidentNumber]
      ,[OrganisationCode_ConveyingAmbulanceTrust]
      ,[DurationOfElectiveWait]
      ,[IntendedManagement]
      ,[DecidedToAdmitDate_forThisProvider]
      ,[WaitingTimeMeasurementType]
      ,[LocationTypeCodeatStartofEpisode]
      ,[HRGcode]
      ,[HRGversionNumber]
      ,[ProcedureSchemeInUse]
      ,[DominantGroupingVariable_Procedure]
      ,[FCE_HRG]
      ,[EpisodeHRGVersionNumber]
      ,[SpellCOREHRG]
      ,[SpellHRGVersionNumber]
      ,[NumberOfBabies]
      ,[FirstAntenatalAssessmentDate]
      ,[GMP_CodeOfGMPresponsibleForAntenatalCare]
      ,[CodeOfGPpractice_RegisteredGMP_AntenatalCare]
      ,[OrganisationCodeTypeGPpractice_RegisteredGMP_AntenatalCare]
      ,[LocationClassOfDeliveryPlace_Intended]
      ,[LocationTypeofDeliveryPlaceIntended]
      ,[DeliveryPlaceChangeReason]
      ,[DeliveryPlaceType_Intended]
      ,[AnaestheticGivenDuringLabour_Delivery]
      ,[AnaestheticGivenPostDelivery]
      ,[GestationLength_LabourOnset]
      ,[Labour_DeliveryOnsetMethod]
      ,[DeliveryDate]
      ,[BirthOrderBaby1]
      ,[DeliveryMethodBaby1]
      ,[GestationLength_AssessmentBaby1]
      ,[ResuscitationMethodBaby1]
      ,[StatusOfPersonConductingDeliveryBaby1]
      ,[LocalPatientIdentifierBaby1]
      ,[OrganisationCodeLocalPatientIDbaby1]
      ,[OrganisationCodeTypeLocalPatientIDbaby1]
      ,[NHSnumberBaby1]
      ,[NHSnumberStatusIndicatorBaby1]
      ,[BirthDate_BabyBaby1]
      ,[BirthWeightBaby1]
      ,[LiveOrStillBirthBaby1]
      ,[SexBaby1]
      ,[BirthLocationType1]
      ,[BirthLocationClass1]
      ,[DeliveryPlaceType_Actual1]
      ,[BirthOrderBaby2]
      ,[DeliveryMethodBaby2]
      ,[GestationLength_AssessmentBaby2]
      ,[ResuscitationMethodBaby2]
      ,[StatusOfPersonConductingDeliveryBaby2]
      ,[LocalPatientIdentifierBaby2]
      ,[OrganisationCodeLocalPatientIDbaby2]
      ,[OrganisationCodeTypeLocalPatientIDbaby2]
      ,[NHSnumberBaby2]
      ,[NHSnumberStatusIndicatorBaby2]
      ,[BirthDate_BabyBaby2]
      ,[BirthWeightBaby2]
      ,[LiveOrStillBirthBaby2]
      ,[SexBaby2]
      ,[BirthLocationType2]
      ,[BirthLocationClass2]
      ,[DeliveryPlaceType_Actual2]
      ,[BirthOrderBaby3]
      ,[DeliveryMethodBaby3]
      ,[GestationLength_AssessmentBaby3]
      ,[ResuscitationMethodBaby3]
      ,[StatusOfPersonConductingDeliveryBaby3]
      ,[LocalPatientIdentifierBaby3]
      ,[OrganisationCodeLocalPatientIDbaby3]
      ,[OrganisationCodeTypeLocalPatientIDbaby3]
      ,[NHSnumberBaby3]
      ,[NHSnumberStatusIndicatorBaby3]
      ,[BirthDate_BabyBaby3]
      ,[BirthWeightBaby3]
      ,[LiveOrStillBirthBaby3]
      ,[SexBaby3]
      ,[BirthLocationType3]
      ,[BirthLocationClass3]
      ,[DeliveryPlaceType_Actual3]
      ,[BirthOrderBaby4]
      ,[DeliveryMethodBaby4]
      ,[GestationLength_AssessmentBaby4]
      ,[ResuscitationMethodBaby4]
      ,[StatusOfPersonConductingDeliveryBaby4]
      ,[LocalPatientIdentifierBaby4]
      ,[OrganisationCodeLocalPatientIDbaby4]
      ,[OrganisationCodeTypeLocalPatientIDbaby4]
      ,[NHSnumberBaby4]
      ,[NHSnumberStatusIndicatorBaby4]
      ,[BirthDate_BabyBaby4]
      ,[BirthWeightBaby4]
      ,[LiveOrStillBirthBaby4]
      ,[SexBaby4]
      ,[BirthLocationType4]
      ,[BirthLocationClass4]
      ,[DeliveryPlaceType_Actual4]
      ,[BirthOrderBaby5]
      ,[DeliveryMethodBaby5]
      ,[GestationLength_AssessmentBaby5]
      ,[ResuscitationMethodBaby5]
      ,[StatusOfPersonConductingDeliveryBaby5]
      ,[LocalPatientIdentifierBaby5]
      ,[OrganisationCodeLocalPatientIDbaby5]
      ,[OrganisationCodeTypeLocalPatientIDbaby5]
      ,[NHSnumberBaby5]
      ,[NHSnumberStatusIndicatorBaby5]
      ,[BirthDate_BabyBaby5]
      ,[BirthWeightBaby5]
      ,[LiveOrStillBirthBaby5]
      ,[SexBaby5]
      ,[BirthLocationType5]
      ,[BirthLocationClass5]
      ,[DeliveryPlaceType_Actual5]
      ,[BirthOrderBaby6]
      ,[DeliveryMethodBaby6]
      ,[GestationLength_AssessmentBaby6]
      ,[ResuscitationMethodBaby6]
      ,[StatusOfPersonConductingDeliveryBaby6]
      ,[LocalPatientIdentifierBaby6]
      ,[OrganisationCodeLocalPatientIDbaby6]
      ,[OrganisationCodeTypeLocalPatientIDbaby6]
      ,[NHSnumberBaby6]
      ,[NHSnumberStatusIndicatorBaby6]
      ,[BirthDate_BabyBaby6]
      ,[BirthWeightBaby6]
      ,[LiveOrStillBirthBaby6]
      ,[SexBaby6]
      ,[BirthLocationType6]
      ,[BirthLocationClass6]
      ,[DeliveryPlaceType_Actual6]
      ,[BirthOrderBaby7]
      ,[DeliveryMethodBaby7]
      ,[GestationLength_AssessmentBaby7]
      ,[ResuscitationMethodBaby7]
      ,[StatusOfPersonConductingDeliveryBaby7]
      ,[LocalPatientIdentifierBaby7]
      ,[OrganisationCodeLocalPatientIDbaby7]
      ,[OrganisationCodeTypeLocalPatientIDbaby7]
      ,[NHSnumberBaby7]
      ,[NHSnumberStatusIndicatorBaby7]
      ,[BirthDate_BabyBaby7]
      ,[BirthWeightBaby7]
      ,[LiveOrStillBirthBaby7]
      ,[SexBaby7]
      ,[BirthLocationType7]
      ,[BirthLocationClass7]
      ,[DeliveryPlaceType_Actual7]
      ,[BirthOrderBaby8]
      ,[DeliveryMethodBaby8]
      ,[GestationLength_AssessmentBaby8]
      ,[ResuscitationMethodBaby8]
      ,[StatusOfPersonConductingDeliveryBaby8]
      ,[LocalPatientIdentifierBaby8]
      ,[OrganisationCodeLocalPatientIDbaby8]
      ,[OrganisationCodeTypeLocalPatientIDbaby8]
      ,[NHSnumberBaby8]
      ,[NHSnumberStatusIndicatorBaby8]
      ,[BirthDate_BabyBaby8]
      ,[BirthWeightBaby8]
      ,[LiveOrStillBirthBaby8]
      ,[SexBaby8]
      ,[BirthLocationType8]
      ,[BirthLocationClass8]
      ,[DeliveryPlaceType_Actual8]
      ,[BirthOrderBaby9]
      ,[DeliveryMethodBaby9]
      ,[GestationLength_AssessmentBaby9]
      ,[ResuscitationMethodBaby9]
      ,[StatusOfPersonConductingDeliveryBaby9]
      ,[LocalPatientIdentifierBaby9]
      ,[OrganisationCodeLocalPatientIDbaby9]
      ,[OrganisationCodeTypeLocalPatientIDbaby9]
      ,[NHSnumberBaby9]
      ,[NHSnumberStatusIndicatorBaby9]
      ,[BirthDate_BabyBaby9]
      ,[BirthWeightBaby9]
      ,[LiveOrStillBirthBaby9]
      ,[SexBaby9]
      ,[BirthLocationType9]
      ,[BirthLocationClass9]
      ,[DeliveryPlaceType_Actual9]
      ,[GestationLengthAssessmentBaby]
      ,[LocalPatientIdentifier_Mother]
      ,[OrganisationCode_LocalPatientIdentifier_Mother]
      ,[OrganisationCodeType_Mother]
      ,[NHSnumber_Mother]
      ,[NHSnumberStatusIndicator_Mother]
      ,[BirthDate_Mother]
      ,[AddressFormatCode_Mother]
      ,[PatientUsualAddress_Mother]
      ,[PostcodeOfUsualAddress_Mother]
      ,[OrganisationCode_PCTofResidence_Mother]
      ,[OrganisationCodeType_PCTofResidence_Mother]
      ,[UniqueBookingReferenceNumberConverted]
      ,[PatientPathwayIdentifier]
      ,[OrganisationCodePatientPathwayIdentifierIssuer]
      ,[ReferraltoTreatmentPeriodStatus]
      ,[ReferraltoTreatmentPeriodStartDate]
      ,[ReferraltoTreatmentPeriodEndDate]
      ,[LeadCareActivityIndicator]
      ,[AgeatCDSActivityDate]
      ,[NHSServiceAgreementChangeDate]
      ,[CDSActivityDate]
      ,[AgeasonAdmission]
      ,[AdminCategoryatStart]
      ,[HospitalProviderSpellDischargeReadyDate]
      ,[LocationType]
      ,[XMLVersion]
      ,[ConfidentialityCategory_Derived]
      ,[ReferraltoTreatmentLength_Derived]
      ,[AgerangepatientderivedfromDOB]
      ,[AgerangemotherderivedfromDOB]
      ,[Areacodederivedfrompostcode]
      ,[CDSGroup]
      ,[FinishedIndicator]
      ,[PCT_DerivedfromGP]
      ,[PCTType_DerivedfromGP]
      ,[GPPractice_Derived]
      ,[GPPracticeMother_Derived]
      ,[PCT_DerivedfromderivedGPPractice]
      ,[PCTMother_DerivedfromderivedGPPractice]
      ,[SHAfromGPPractice]
      ,[SHATypefromGPPractice]
      ,[HospitalSpellDuration]
      ,[MonthofBirth]
      ,[HomeBirthorDelivery]
      ,[ElectoralWardfrompostcode]
      ,[PCTfrompostcode]
      ,[PCTTypefromPostcode]
      ,[SHAfromPostcode]
      ,[SHATypefromPostcode]
      ,[AreacodefromProviderPostcode]
      ,[AgeatEndofEpisode]
      ,[AgeatStartofEpisode]
      ,[YearofBirth]
      ,[YearofBirthMother]
      ,[MonthofBirthMother]
      ,[2001CensusArea]
      ,[Country]
      ,[CountyCode]
      ,[1991CensusED]
      ,[EDDistrictCode]
      ,[ElectoralWardCode]
      ,[GORCode]
      ,[LocalUnitaryAuthority]
      ,[OldSHACode]
      ,[1998ElectoralArea]
      ,[NumberOfAugmentedPeriods]
      ,[DeliveryPlaceType_Actual]
      ,[LocationClassDeliveryPlace_Actual]
      ,[PBR_DerivedCommissioner]
      ,[PBR_DerivedCommissionerType]
      ,[File_ID]
      ,[Process_Key]
      ,[VGP_DetentionCategory]
      ,[VGP_TreatmentNo]
      ,[VGP_DobEstFlag]
      ,[VGP_OverseasVisitorStatus]
      ,[VGP_ActualWait]
      ,[VGP_ProviderSpellSOA]
      ,[VGP_ProvderSpellStartSite]
      ,[VGP_ConsultantEpisodeEndSite]
      ,[VGP_ConsultantEpisodeStartSite]
      ,[VGP_FCEStayId]
      ,[VGP_HealthResourceGroup]
      ,[VGP_HealthResourceGroupVerNo]
      ,[VGP_PricingCodeOfProcedure]
      ,[VGP_Price]
      ,[VGP_LocalSpecialtyCode]
      ,[VGP_RegionalSpecialtyCode]
      ,[VGP_WardCodeAtEndOfEpisode]
      ,[AIMTC_Pseudo_NHS]
      ,[AIMTC_PCTResidence]
      ,[AIMTC_PCTRegGP]
      ,[AIMTC_PracticeCodeOfRegisteredGP]
      ,[AIMTC_Age]
      ,[AIMTC_Type]
      ,[AIMTC_PCTCOMML]
      ,[AIMTC_PCTCodeofCommissioner]
      ,[AIMTC_Episode_Duration]
      ,[AIMTC_TreatmentFunctionCode]
      ,[AIMTC_SEQ]
      ,[AIMTC_SiteCode_ofTreatment1]
      ,[AIMTC_DischargeMethod_HospitalProviderSpell]
      ,[AIMTC_DischargeDestination_HospitalProviderSpell]
      ,[AIMTC_LastEpisodeInSpellIndicator]
      ,[AIMTC_PatientClassification]
      ,[AIMTC_OrganisationCode_CodeOfProvider]
      ,[AIMTC_AdmissionMethod_HospitalProviderSpell]
      ,[AIMTC_SourceOfAdmission_HospitalProviderSpell]
      ,[AIMTC_Child_CriticalCareDays]
      ,[AIMTC_Adult_CriticalCareDays]
      ,[AIMTC_CriticalCareDays]
      ,[AIMTC_OrganisationCode_Codeofcommissioner]
      ,[AIMTC_Specialty_Trend]
      ,[AIMTC_Postcode]
      ,[AIMTC_Postcode_Key]
      ,[AIMTC_OrganisationCode_CodeOfCommissionerFlag]
      ,[AIMTC_OrganisationCode_CodeOfCommissionerDescription]
      ,[AIMTC_CCG_NonCCG_Current_Flag]
      ,[AIMTC_CCG_NonCCG_Current_Description]
      ,[AIMTC_CCG_NonCCG_Previous_Flag]
      ,[AIMTC_CCG_NonCCG_Previous_Description]
      ,[AIMTC_CCG_NonCCG_ProviderSpell_Flag]
      ,[AIMTC_CCG_NonCCG_ProviderSpell_Description]
      ,[AIMTC_ProviderSpell_FCE_HRG]
      ,[AIMTC_ProviderSpell_GroupingMethodFlag]
      ,[AIMTC_ProviderSpell_DominantProcedure]
      ,[AIMTC_ProviderSpell_FCE_PBC]
      ,[AIMTC_ProviderSpell_CalcEpiDur]
      ,[AIMTC_ProviderSpell_Start_Date]
      ,[AIMTC_ProviderSpell_End_Date]
      ,[AIMTC_Current_FCE_HRG]
      ,[AIMTC_Current_GroupingMethodFlag]
      ,[AIMTC_Current_DominantProcedure]
      ,[AIMTC_Current_FCE_PBC]
      ,[AIMTC_Current_CalcEpiDur]
      ,[AIMTC_Current_PBR_State]
      ,[AIMTC_Current_PBR_Rules]
      ,[AIMTC_Current_PBR_Start_Date]
      ,[AIMTC_Current_PBR_End_Date]
      ,[AIMTC_Previous_FCE_HRG]
      ,[AIMTC_Previous_GroupingMethodFlag]
      ,[AIMTC_Previous_DominantProcedure]
      ,[AIMTC_Previous_FCE_PBC]
      ,[AIMTC_Previous_CalcEpiDur]
      ,[AIMTC_Previous_PBR_State]
      ,[AIMTC_Previous_PBR_Rules]
      ,[AIMTC_Previous_PBR_Start_Date]
      ,[AIMTC_Previous_PBR_End_Date]
      ,[AIMTC_Future_FCE_HRG]
      ,[AIMTC_Future_GroupingMethodFlag]
      ,[AIMTC_Future_DominantProcedure]
      ,[AIMTC_Future_FCE_PBC]
      ,[AIMTC_Future_CalcEpiDur]
      ,[AIMTC_Future_PBR_State]
      ,[AIMTC_Future_PBR_Rules]
      ,[AIMTC_Future_PBR_Start_Date]
      ,[AIMTC_Future_PBR_End_Date]
      ,[AIMTC_Future_Plus_FCE_HRG]
      ,[AIMTC_Future_Plus_GroupingMethodFlag]
      ,[AIMTC_Future_Plus_DominantProcedure]
      ,[AIMTC_Future_Plus_FCE_PBC]
      ,[AIMTC_Future_Plus_CalcEpiDur]
      ,[AIMTC_Future_Plus_PBR_State]
      ,[AIMTC_Future_Plus_PBR_Rules]
      ,[AIMTC_Future_Plus_PBR_Start_Date]
      ,[AIMTC_Future_Plus_PBR_End_Date]
      ,[AIMTC_Access_Level]
      ,[Derived_Previous_FCE_NPOC]
      ,[Derived_Previous_FCE_Serviceline]
      ,[Derived_Previous_ServiceLineList]
      ,[Derived_Previous_Spell_NPOC]
      ,[Derived_Previous_Spell_ServiceLine]
      ,[Derived_Current_FCE_NPOC]
      ,[Derived_Current_FCE_Serviceline]
      ,[Derived_Current_ServiceLineList]
      ,[Derived_Current_Spell_NPOC]
      ,[Derived_Current_Spell_ServiceLine]
      ,[Derived_Future_FCE_NPOC]
      ,[Derived_Future_FCE_Serviceline]
      ,[Derived_Future_ServiceLineList]
      ,[Derived_Future_Spell_NPOC]
      ,[Derived_Future_Spell_ServiceLine]
      ,[Derived_Previous_PSS2021_FCE_NPOC]
      ,[Derived_Previous_PSS2021_FCE_ServiceLine]
      ,[Derived_Previous_PSS2021_ServiceLineList]
      ,[Derived_Previous_PSS2021_Spell_NPOC]
      ,[Derived_Previous_PSS2021_Spell_ServiceLine]
      ,[Derived_Previous_PSS2122_FCE_NPOC]
      ,[Derived_Previous_PSS2122_FCE_ServiceLine]
      ,[Derived_Previous_PSS2122_ServiceLineList]
      ,[Derived_Previous_PSS2122_Spell_NPOC]
      ,[Derived_Previous_PSS2122_Spell_ServiceLine]
      ,[Derived_Previous_PSS2223_FCE_NPOC]
      ,[Derived_Previous_PSS2223_FCE_ServiceLine]
      ,[Derived_Previous_PSS2223_ServiceLineList]
      ,[Derived_Previous_PSS2223_Spell_NPOC]
      ,[Derived_Previous_PSS2223_Spell_ServiceLine]
      ,[Derived_Current_PSS2021_FCE_NPOC]
      ,[Derived_Current_PSS2021_FCE_ServiceLine]
      ,[Derived_Current_PSS2021_ServiceLineList]
      ,[Derived_Current_PSS2021_SpellNPOC]
      ,[Derived_Current_PSS2021_SpellServiceLine]
      ,[Derived_Current_PSS2122_FCE_NPOC]
      ,[Derived_Current_PSS2122_FCE_ServiceLine]
      ,[Derived_Current_PSS2122_ServiceLineList]
      ,[Derived_Current_PSS2122_SpellNPOC]
      ,[Derived_Current_PSS2122_SpellServiceLine]
      ,[Derived_Current_PSS2223_FCE_NPOC]
      ,[Derived_Current_PSS2223_FCE_ServiceLine]
      ,[Derived_Current_PSS2223_ServiceLineList]
      ,[Derived_Current_PSS2223_SpellNPOC]
      ,[Derived_Current_PSS2223_SpellServiceLine]
      ,[Derived_Future_PSS2021_FCE_NPOC]
      ,[Derived_Future_PSS2021_FCE_ServiceLine]
      ,[Derived_Future_PSS2021_ServiceLineList]
      ,[Derived_Future_PSS2021_SpellNPOC]
      ,[Derived_Future_PSS2021_SpellServiceLine]
      ,[Derived_Future_PSS2122_FCE_NPOC]
      ,[Derived_Future_PSS2122_FCE_ServiceLine]
      ,[Derived_Future_PSS2122_ServiceLineList]
      ,[Derived_Future_PSS2122_SpellNPOC]
      ,[Derived_Future_PSS2122_SpellServiceLine]
      ,[Derived_Future_PSS2223_FCE_NPOC]
      ,[Derived_Future_PSS2223_FCE_ServiceLine]
      ,[Derived_Future_PSS2223_ServiceLineList]
      ,[Derived_Future_PSS2223_SpellNPOC]
      ,[Derived_Future_PSS2223_SpellServiceLine]
  FROM [ABI].[dbo].[vw_APC_SEM_001]
  where cast(StartDate_HospitalProviderSpell as date) between '{report_start}' and '{report_end}'")
}

dat_apc <- sqlQuery(con,string_apc) %>%
  mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS))

saveRDS(dat_apc, "data/dat_apc.RDS")
dat_apc <- readRDS("data/dat_apc.RDS")

dat_scores <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, StartDate_HospitalProviderSpell, contains("ICD")) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(diff == min(diff), diff < 2)  %>%
  select(-DiagnosisSchemeInUse_ICD) %>%
  select(nhs_number, arr_id, matches("ICD")) %>%
  pivot_longer(-c(nhs_number, arr_id)) %>%
  filter(value != "") %>%
  select(-name) %>%
  ungroup() %>%
  distinct() %>%
  list(distinct(select(., nhs_number, arr_id)), 
       vw_score = comorbidity(., id = "arr_id", code = "value", "elixhauser_icd10_quan", assign0 = FALSE) %>%
         score(weights = "vw", assign0 = FALSE)) %>%
  `[`(-1) %>%
  bind_cols()

dat_spec <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  # filter(diff == min(diff), diff <= 1) %>%
  select(nhs_number, arr_id, MainSpecialtyCode)


dat_los <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  # filter(diff == min(diff), diff < 2) %>% 
  mutate(los = interval(StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell)/ddays(1)) %>%
  select(nhs_number, arr_id, los) %>%
  distinct()


# dat_adm <- dat_ecds %>%
#   mutate(nhs_number = as.character(nhs_number)) %>%
#   group_by(nhs_number) %>%
#   filter(dta_dt == min(dta_dt, na.rm = TRUE)) %>%
#   ungroup() %>%
#   select(site, nhs_number, imd, age, sex, arr_id, arrival_dt, dta_dt, departure_dt, dta_time, ed_time) %>%
#   mutate(arrival_hour = floor_date(arrival_dt, "hour")) %>%
#   left_join(dat_ecds_perf) %>%
#   left_join(dat_ecds_n_ed) %>% 
#   left_join(dat_scores) %>%
#   left_join(dat_los) %>%
#   select(-arr_id) %>%
#   na.omit()


dat_adm <- dat_ecds %>%
  # remove arrivals that dont have full outcome window
  filter(arrival_dt < ymd(report_end) - ddays(days_outcome_window)) %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  group_by(nhs_number) %>%
  slice_min(dta_dt, na_rm = TRUE) %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(age < 120, !is.na(age))   %>%
  # filter(dta_dt == min(dta_dt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_time = fct_recode(arrival_time,
                                   "00:00-06:00" = "00:00:00",
                                   "06:00-12:00" = "06:00:00",
                                   "12:00-18:00" = "12:00:00",
                                   "18:00-24:00" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    site,
    nhs_number,
    imd,
    age,
    sex,
    arr_id,
    arrival_dt,
    dta_dt,
    departure_dt,
    dta_time,
    ed_time,
    Acuity_Desc,
    arrival_mode = Arrival_Mode_Desc,
    arrival_cat = Arrival_Category_Desc,
    arrival_time,
    arrival_month,
    arrival_year
    # diagnoses,
    # investigation
  ) %>%
  mutate(arrival_hour = floor_date(arrival_dt, "hour")) %>%
  left_join(dat_ecds_perf) %>%
  left_join(dat_ecds_n_ed) %>% 
  left_join(dat_scores) %>%
  left_join(dat_spec)%>%
  mutate(perf_sq = perf^2, age_sq = age^2, n_ed_sq = n_ed^2, vw_score_sq = vw_score^2) %>%
  left_join(dat_los) %>%
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  na.omit()



(los_plot <- local({
    
  plot_df <- dat_adm %>%
    distinct() %>%
    filter(between(dta_time, 0, 60 * 24)) %>%
    filter(los < 75) 
  
  plot_df %>%
    mutate(dta_time = 30*dta_time%/%30) %>%
    group_by(dta_time) %>%
    summarise(los = quantile(los, 0.9)) %>%
    ggplot(aes(x = dta_time/60, y = los, col = "90% quantile")) +
    geom_point(data = plot_df, aes(col = "All data"),  alpha = 0.015) +
    geom_smooth(method = "lm", show.legend = FALSE) +
    geom_point(size = 2.5) +
    bnssgtheme() +
    scale_colour_manual(values = c(unname(bnssgcols[4]), "black"),
                        guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1))) +
    scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
    guides(alpha = "none") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text()) +
    labs(x = "Time waiting for admission (hour)",
         y = "LOS (day)",
         colour = "Total length of stay")
})
)


ggsave("materials/dta_los_quant.png",
       los_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9) 

dat_adm %>%
  distinct() %>%
  filter(between(dta_time, 0, 60 * 24)) %>%
  filter(los < 75) %>%
  mutate(dta_time = 30*dta_time%/%30) %>%
  group_by(dta_time) %>%
  summarise(los = quantile(los, 0.9)) %>%
  mutate(dta_time = dta_time/60) %>%
  lm(los ~ dta_time, data = .) %>%
  summary()


los_outputs <- local({

  plot_df <- dat_adm %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    distinct() %>%
    filter(between(dta_time, 0, 60 * 25)) 
  
  p_raw <- plot_df %>%
    mutate(dta_time = factor(dta_time %/% 60, levels = 0:24)) %>%
    ggplot(aes(
      x = dta_time,
      y = los
    )) +
    geom_boxplot(outliers = FALSE,
                 fatten = NULL) +
    stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
    theme_minimal() +
    scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE)]}) +
    # scale_y_continuous(limits = c(0, 25), breaks = seq(0, 45, 10)) +
    guides(alpha = "none") +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text()
    ) +
    labs(
      shape = "",
      x = "Time waiting for admission (hour)",
      y = "In-hospital length of stay (day)",
      colour = "Total length of stay"
    )
  
   mod_std <- lm(los ~ . 
                 -nhs_number
                 -site
                 -arrival_dt
                 -dta_dt
                 -departure_dt
                 -dta_time
                 -arrival_hour
                 -ed_time
                 -n_adm
                 , plot_df)
   
   reg_tab_std <- gtsummary::tbl_regression(mod_std,
                                        label = list(
     arrival_time = "Time of arrival",
     arrival_month = "Month of arrival",
     arrival_year = "Year of arrival",
     arrival_cat = "Arrival category",
     arrival_mode = "Arrival mode",
     perf = "4-hour ED performance",
     perf_sq = "4-hour ED performance (squared)",
     age = "Age",
     age_sq = "Age (squared)",
     n_ed = "Number of previous ED attendances",
     n_ed_sq = "Number of previous ED attendances (squared)",
     sex = "Sex",
     imd = "IMD",
     vw_score = "Van Walraven score",
     vw_score_sq = "Van Walraven score (squared)",
     Acuity_Desc = "Acuity description"#,
     # investigation = "Investigation description (encoded)",
     # diagnoses = "Diagnoses description (encoded)"
   )) %>%
     gtsummary::add_glance_table(include = c(nobs)) %>%
     gtsummary::as_gt() %>%
     gt::tab_header(
       title = gt::md("**In Hospital Length of Stay (days)**"))
   
   mod_full <- lm(los ~ .
                  -nhs_number
                  -site
                  -arrival_dt
                  -dta_dt
                  -departure_dt
                  -arrival_hour
                  -ed_time
                  -n_adm
                  , {plot_df %>% mutate(dta_time = dta_time/60)})
   
   reg_tab_full <- gtsummary::tbl_regression(
     mod_full,
                                        label = list(
     dta_time = "Hours waiting for admission",                                    
     arrival_time = "Time of arrival",
     arrival_month = "Month of arrival",
     arrival_year = "Year of arrival",
     arrival_cat = "Arrival category",
     arrival_mode = "Arrival mode",
     perf = "4-hour ED performance",
     perf_sq = "4-hour ED performance (squared)",
     age = "Age",
     age_sq = "Age (squared)",
     n_ed = "Number of previous ED attendances",
     n_ed_sq = "Number of previous ED attendances (squared)",
     sex = "Sex",
     imd = "IMD",
     vw_score = "Van Walraven score",
     vw_score_sq = "Van Walraven score (squared)",
     Acuity_Desc = "Acuity description"#,
     # investigation = "Investigation description (encoded)",
     # diagnoses = "Diagnoses description (encoded)"
   )) %>%
     gtsummary::add_glance_table(include = c(nobs)) %>%
     gtsummary::as_gt() %>%
     gt::tab_header(
       title = gt::md("**In Hospital Length of Stay (days)**"))
   
   los_pred <- predict(mod_std, plot_df)
   
   p_std <- plot_df %>%
     mutate(los_adj = los_pred, 
            dta_time = factor(dta_time %/% 60, levels = 0:24)) %>%
     ggplot(aes(
       x = dta_time,
       y = los - los_adj
     )) +
     geom_boxplot(outliers = FALSE,
                  fatten = NULL) +
     stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
     stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
     theme_minimal() +
     scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE)]}) +
     # scale_y_continuous(limits = c(0, 25), breaks = seq(0, 45, 10)) +
     guides(alpha = "none") +
     theme(
       legend.position = "bottom",
       legend.justification = "center",
       legend.title = element_text()
     ) +
     labs(
       shape = "",
       x = "Time waiting for admission (hour)",
       y = "In-hospital length of stay (day)",
       colour = "Total length of stay"
     )
   
   list(p_raw = p_raw, 
        p_std = p_std,
        mod_std = mod_std,
        mod_full = mod_full,
        reg_tab_std = reg_tab_std,
        reg_tab_full = reg_tab_full)

})

saveRDS(los_outputs, "data/los_outputs.RDS")

ggsave("materials/dta_los_plot_paper.png",
       last_plot(),
       bg = "white",
       height = 5,
       width = 7.5, 
       scale = 0.8) 

