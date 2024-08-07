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


report_start <- '2022-06-01'
report_end <- '2024-06-01'
pre_window_days <- 60


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
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date >= '{ymd(report_start) - ddays(pre_window_days)}'
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
    arrival_mode = Arrival_Mode_Desc,
    arrival_cat = Arrival_Category_Desc,
    diagnoses = Diagnoses_Code_1_Desc,
    investigation = Investigation_Code_1_Desc
  ) %>%
  rowwise() %>%
  mutate(departure_dt=max(dta_dt,departure_dt, conclusion_dt, na.rm=TRUE)) %>%
  # mutate(departure_dt=max(departure_dt, conclusion_dt, na.rm=TRUE)) %>%
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
  # for any given patient & day of arrival, take the last record (if there is
  # more than one)
  slice_max(arrival_dt, na_rm = TRUE) %>%
  # filter(arrival_dt == max(arrival_dt, na.rm = TRUE)) %>%
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
string_attr <- glue::glue("select distinct 
 nhs_number
from MODELLING_SQL_AREA.dbo.primary_care_attributes
where attribute_period between '{report_start}' and '{report_end}'")


dat_attr <- sqlQuery(con,string_attr) %>%
  filter(!is.na(nhs_number)) %>%
  mutate(nhs_number = as.character(nhs_number))

# saveRDS(dat_attr, "data/dat_attr.RDS")
dat_attr <- readRDS("data/dat_attr.RDS")

# arrivals for joining to sus
arrivals <- 
  dat_ecds %>%
  filter(!is.na(dta_time), nhs_number %in% unique(dat_attr$nhs_number), !is.na(nhs_number)) %>%
  group_by(nhs_number, arr_id) %>%
  summarise(departure_dt = as.Date(departure_dt)[1]
  )

# SUS query (takes a long time, should prune the variables it is returning)
string_apc <- {glue::glue("SELECT 
       [AIMTC_Pseudo_NHS]                    
      ,[PBRspellID]
      ,[CensusDate]
      ,[ExtractDateTime]
      ,[ReportPeriodStartDate]
      ,[ReportPeriodEndDate]
      ,[NHSnumber]
      ,[DateOfBirth]
      ,[Sex]
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
  FROM [ABI].[dbo].[vw_APC_SEM_001]
  where cast(StartDate_HospitalProviderSpell as date) between '{report_start}' and '{report_end}'")
}

dat_apc <- sqlQuery(con,string_apc) %>%
  mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS))

saveRDS(dat_apc, "data/dat_apc_new.RDS")
dat_apc <- readRDS("data/dat_apc_new.RDS")

dat_scores <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, StartDate_HospitalProviderSpell, contains("ICD")) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(diff == min(diff), diff <= 1)  %>%
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
  filter(diff == min(diff), diff <= 1) %>%
  select(nhs_number, arr_id, MainSpecialtyCode)

# d2a pathway
pathway_df <- local({
  nctr_df <-
    RODBC::sqlQuery(
      con,
      "SELECT [RN]
      ,[Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
      ,[Month_Date]
      ,[Month]
      ,[Day_Of_Week]
      ,[Week_Start_Date]
      ,[NHS_Number]
      ,[Person_Stated_Gender_Code]
      ,[Person_Age]
      ,[CDS_Unique_Identifier]
      ,[Sub_ICB_Location]
      ,[Organisation_Site_Code]
      ,[Current_Ward]
      ,[Specialty_Code]
      ,[Bed_Type]
      ,[Date_Of_Admission]
      ,[BNSSG]
      ,[Local_Authority]
      ,[Criteria_To_Reside]
      ,[Date_NCTR]
      ,[Current_LOS]
      ,[Days_NCTR]
      ,[Days_NCTR_On_Current_Code]
      ,[Current_Delay_Code]
      ,[Local_Authority_grouped]
      ,[Site_Name]
      ,[Current_Delay_Code_Standard]
      ,[Current_Delay_Code_Detailed]
      ,[Acute Community split]
      ,[Current_Covid_Status]
      ,[Planned_Date_Of_Discharge]
      ,[Date_Toc_Form_Completed]
      ,[Toc_Form_Status]
      ,[Discharge_Pathway]
      ,[DER_File_Name]
      ,[DER_Load_Timestamp]
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI"
    )
  
  
  pathway_df <- nctr_df %>%
    mutate(pathway = recode(Current_Delay_Code_Standard,
                            "P3 / Other Complex Discharge" = "P3",
                            "18a  Infection  bxviii  Standard" = "Other",
                            "Uncoded" = "Other",
                            "Repatriation" = "Other",
                            "NCTR Null" = "Other",
                            "Not Set" = "Other",
                            "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
                            "15b  Repat  bxv  WGH" = "Other"),
           pathway = coalesce(pathway, "Other")) %>%
    mutate(pathway = if_else(pathway %in% c("Other", "P1", "P2", "P3"), pathway, "Other")) %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
           sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
    group_by(nhs_number, adm_date = Date_Of_Admission) %>%
    arrange(Census_Date) %>%
    reframe(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other"),
            sex = sex[1],
            age = Person_Age[1],
            spec = Specialty_Code[1],
            bed_type = Bed_Type[1]) %>%
    select(nhs_number,
           adm_date,
           pathway,
           spec,
           bed_type 
    ) %>%
    filter(adm_date > ymd("2023-06-01"))
  pathway_df
})


pathway_arr <- arrivals %>%
  left_join(pathway_df) %>%
  mutate(diff = interval(departure_dt, as.Date(adm_date))/ddays(1)) %>%
  filter(between(diff, 0, 1)) %>%
  select(nhs_number, arr_id, pathway)

# social care:

# social care data (this outcome has to be joined earlier as it related to
# arr_id

dat_socare <- local({
  string_sc <- glue::glue("
                              select * 
                              FROM [Analyst_SQL_Area].[dbo].[tbl_Adult_Social_Care_CLD]
                              where Event_Start_Date between '{report_start}' and '{report_end}'
                              ")
  
  dat_sc <- sqlQuery(con, string_sc) %>%
    filter(Event_Start_Date > ymd("2023-04-01")) %>%
    select(nhs_number = NHS_Number,
           event_start = Event_Start_Date,
           event_end = Event_End_Date) %>%
    filter(!is.na(nhs_number)) %>%
    group_by(nhs_number) %>%
    mutate(nhs_number = as.character(nhs_number),
           pre_event_end = if_else(is.na(event_end), lag(event_end), event_end)) %>%
    ungroup() %>%
    distinct()
  dat_sc
})



dat_sc_outcome <- arrivals %>%
  select(nhs_number,
         arr_id,
         departure_dt) %>%
  inner_join(dat_socare) %>%
  group_by(nhs_number, arr_id) %>%
  # course filter any patients who have had SC before
  filter(all(event_start > departure_dt), all(event_end > departure_dt)) %>%
  mutate(sc = interval(departure_dt, event_start)/ddays(1) < 60) %>%
  select(nhs_number, arr_id, sc) %>%
  distinct() %>%
  mutate(sc = ifelse(sc, yes = "yes", no = "no"))

dat_sc_outcome <- arrivals %>%
  left_join(dat_sc_outcome) %>%
  mutate(sc = coalesce(sc, "missing")) %>%
  select(-departure_dt) %>% 
  distinct()


dat_adm <- dat_ecds %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  group_by(nhs_number) %>%
  slice_min(dta_dt, na_rm = TRUE) %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(age < 120, !is.na(age)) %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  # filter(dta_dt == min(dta_dt, na.rm = TRUE)) %>%
  ungroup() %>%
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
    arrival_mode,
    arrival_cat,
    diagnoses,
    investigation
  ) %>%
  mutate(arrival_hour = floor_date(arrival_dt, "hour")) %>%
  left_join(dat_ecds_perf) %>%
  left_join(dat_ecds_n_ed) %>% 
  left_join(dat_scores) %>%
  left_join(dat_spec) %>%
  left_join(pathway_arr) %>%
  mutate(pathway = coalesce(pathway, "missing")) %>%
  left_join(dat_sc_outcome) %>%
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  na.omit()

# saveRDS(dat_adm, "data/dat_adm_new.RDS")


mortality_df <- local({
  string_mortality <- glue::glue("SELECT
      [Derived_Pseudo_NHS]
      ,[Dec_Age_At_Death]
      ,[DEC_SEX]
      ,[DEC_SEX_DESC]
      ,[DEC_MARITAL_STATUS]
      ,[DEC_MARITAL_STATUS_DESC]
      ,[DEC_AGEC]
      ,[DEC_AGECUNIT]
      ,[DEC_AGECUNIT_DESC]
      ,[REG_DATE_OF_DEATH]
      ,[REG_DATE]
  FROM [ABI].[Civil_Registration].[Mortality]
  WHERE REG_DATE_OF_DEATH between '{lubridate::ymd(report_start)}' AND '{lubridate::ymd(report_end) + lubridate::ddays(30)}'
  ")
  con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
  
  sqlQuery(con, string_mortality) %>%
    na.omit() %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS),
           REG_DATE_OF_DEATH = lubridate::ymd(REG_DATE_OF_DEATH)) %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS)) %>%
    select(nhs_number = Derived_Pseudo_NHS, date_death = REG_DATE_OF_DEATH)
})



smr_df <- local({
  string_mortality <- glue::glue("SELECT
      [Derived_Pseudo_NHS]
      ,[Dec_Age_At_Death]
      ,[DEC_SEX]
      ,[DEC_SEX_DESC]
      ,[DEC_MARITAL_STATUS]
      ,[DEC_MARITAL_STATUS_DESC]
      ,[DEC_AGEC]
      ,[DEC_AGECUNIT]
      ,[DEC_AGECUNIT_DESC]
      ,[REG_DATE_OF_DEATH]
      ,[REG_DATE]
  FROM [ABI].[Civil_Registration].[Mortality]
  WHERE REG_DATE_OF_DEATH between '{lubridate::ymd('2022-01-01')}' AND '{lubridate::ymd(report_end) + lubridate::ddays(30)}'
  ")
  con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
  
  sqlQuery(con, string_mortality) %>%
    na.omit() %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS),
           REG_DATE_OF_DEATH = lubridate::ymd(REG_DATE_OF_DEATH)) %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS)) %>%
    select(nhs_number = Derived_Pseudo_NHS, date_death = REG_DATE_OF_DEATH)
})


t_max <- 24*60
days_mort <- 30


mod_df <- dat_adm %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  filter(sex %in% 1:2) %>%
  left_join(mortality_df) %>%
  # mutate(age = cut(age, breaks = seq(0, 120, 10), include.lowest = TRUE)) %>%
  # mutate(dta_time = dta_time > 4*60) %>%
  # mutate(dta_time = cut(dta_time, breaks = 60*seq(0, 24, 4), include.lowest = TRUE)) %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  # mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(age_sq = age^2) %>%
  mutate(perf_sq = perf^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(vw_score_sq = vw_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(imd = fct_recode(imd, "1 (least deprived)" = "1", "10 (most deprived)" = "10")) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  mutate(mort_30 = factor(ifelse(mort_30, "yes", "no"), levels = c("no", "yes"))) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_time = fct_recode(arrival_time,
                                   "00:00-06:00" = "00:00:00",
                                   "06:00-12:00" = "06:00:00",
                                   "12:00-18:00" = "12:00:00",
                                   "18:00-24:00" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    perf_sq,
    age,
    age_sq,
    n_ed,
    n_ed_sq,
    # Acuity_Desc,
    # arrival_mode,
    # arrival_cat,
    # diagnoses,
    # investigation,
    # n_adm,    # bring these back in when they aren't all
    # n_adm_sq,
    sex,
    imd,
    vw_score,
    vw_score_sq,
    mort_30,
    pathway,
    sc,
    dta_time
  )

# mort outcome

split_mort <- initial_split(mod_df, strata = mort_30) 
glm_spec <- logistic_reg()

rec_mort <- recipe(mort_30 ~ ., data = training(split_mort)) %>%
  update_role(pathway, sc, new_role = "outcomes") %>%
  step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  step_novel(all_nominal_predictors(), new_level = "novel") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_mort <- workflow() %>%
  add_recipe(rec_mort) %>%
  add_model(glm_spec)

fit_mort <- last_fit(wf_mort, split_mort)

augment(fit_mort) %>%
  roc_curve(mort_30,
            .pred_yes,
            event_level = "second") %>%
  autoplot()

# fit on all data

full_fit_mort <- fit(wf_mort, mod_df)
full_fit_mort %>% broom::tidy() %>% filter(str_detect(term, "dta"))



# pathway outcome

mod_df_pathway <- mod_df %>%
  filter(pathway != "missing") %>%
  mutate(pathway = factor(pathway != "Other"))

split_pathway <- initial_split(mod_df_pathway, strata = pathway) 
glm_spec <- logistic_reg()

rec_pathway <- recipe(pathway ~ ., data = training(split_pathway)) %>%
  update_role(mort_30, sc, new_role = "outcomes") %>%
  update_role(dta_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_pathway <- workflow() %>%
  add_recipe(rec_pathway) %>%
  add_model(glm_spec)

# fit_pathway <- last_fit(wf_pathway, split_pathway)
# 
# augment(fit_pathway) %>%
#   roc_curve(mort_30,
#             .pred_TRUE,
#             event_level = "second") %>%
#   autoplot()

# fit on all data

full_fit_pathway <- fit(wf_pathway, mod_df_pathway)
# full_fit_pathway %>%
#   extract_fit_engine() %>%
#   broom::tidy() %>%
#   filter(term == "dta_time")
# 
# term      estimate std.error statistic p.value
# <chr>        <dbl>     <dbl>     <dbl>   <dbl>
#   1 dta_time 0.0000469 0.0000945     0.496   0.620

full_pred_pathway <- mod_df_pathway %>%
  filter(dta_time <= time_max) %>%
  bake(prep(rec_pathway), new_data = .) %>%
  predict(extract_fit_engine(full_fit_pathway),
          newdata = .,
          type = "response")

yardstick::roc_auc_vec(mod_df_pathway$pathway, estimate = full_pred_pathway, event_level = "second")
# 0.7713718


bin <- 7
time_max <- 24*60
p = seq(0, 1, length = bin + 1)

t_var_raw_pathway <- mod_df_pathway %>%
  filter(dta_time <= time_max) %>%
  pull(dta_time)

plot_df <- 
  mod_df_pathway %>%
  filter(dta_time <= time_max) %>%
  mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
  mutate(pathway = pathway == "TRUE") %>%
  select(dta_time, pathway) %>%
  mutate(pred = full_pred_pathway) %>%
  group_by(dta_time) %>%
  summarise(
    n = n(),
    expected = sum(pred),
    actual = sum(pathway),
    smr = actual / expected
  ) %>%
  mutate(
    left = head(unique(quantile(t_var_raw_pathway, p)), -1),
    right = tail(unique(quantile(t_var_raw_pathway, p)), -1),
    mid = (left + right) / 2,
    width = right - left,
    weight = 1/width
  ) %>%
  mutate(
    z = qnorm(0.95 + (1 - 0.95) / 2),
    u_ci = smr + z*sqrt(smr/n),
    l_ci = smr - z*sqrt(smr/n)
  )

plot_df <- bind_rows(plot_df, 
                     {tail(plot_df, 1) %>% mutate(left = right)})


shade_df <- unique(plot_df$left) %>%
  sort() %>%
  head(n = 2*length(.) %/% 2) %>% # slice the nearest multiple of 2 values 
  matrix(ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  mutate(min = min(pred_df$l_se, plot_df$smr, plot_df$l_ci))

x_breaks <- unique(plot_df$left, plot_df$right) %>%
  tail(-1) %>%
  head(-1) %>%
  c(0, time_max) %>%
  sort()



(smr_pathway <- ggplot(plot_df, aes(x = mid/60, y = smr)) +
    # geom_step(aes(x = left/60)) +
    geom_rect(shade_df, mapping = aes(x = NULL, y = NULL, xmin = V1/60, xmax = V2/60, ymin = -Inf, ymax = Inf), alpha = 0.15) + 
    # geom_ribbon(data = pred_df, aes(x = mid/60, y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    # geom_line(data = pred_df, aes(x = mid/60, y = pred), col = bnssgcols[4]) +
    # geom_linerange(aes(xmin = left/60, xmax = right/60), linetype = 2) +
    geom_errorbar(aes(ymin = l_ci, ymax = u_ci, width = width/(2*60))) +
    # geom_vline(aes(xintercept = right/60), linetype = 2) +
    # geom_vline(aes(xintercept = left/60), linetype = 2) +
    # geom_col(aes(width = (right-left)/60),col = bnssgcols[4]) +
    # geom_line(linetype = 2) +
    geom_point() +
    # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(#limits = c(NA, (time_max/60)-1),
      breaks = x_breaks/60,
      labels = lab_fn(x_breaks),
      guide = guide_axis(n.dodge = 2)
    ) +
    # bnssgtheme() +
    theme_minimal() +
    # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, time_max/60)) +
    labs(y = "Standardised D2A Utilisation\nRatio",
         x = "Time Waiting for Admission"))

saveRDS(smr_pathway, "data/smr_pathway.RDS")


# sc outcome

mod_df_sc <- mod_df %>%
  filter(sc != "missing")

split_sc <- initial_split(mod_df_sc, strata = sc) 
glm_spec <- logistic_reg()

rec_sc <- recipe(sc ~ ., data = training(split_sc)) %>%
  update_role(mort_30, pathway, new_role = "outcomes") %>%
  update_role(dta_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_sc <- workflow() %>%
  add_recipe(rec_sc) %>%
  add_model(glm_spec)

# fit_sc <- last_fit(wf_sc, split_sc)
# fit_sc$.metrics


# augment(fit_sc) %>%
#   roc_curve(mort_30,
#             .pred_yes,
#             event_level = "second") %>%
#   autoplot()

# fit on all data

full_fit_sc <- fit(wf_sc, mod_df_sc)
# full_fit_sc %>%
#   extract_fit_engine() %>%
#   broom::tidy() %>%
#   filter(term == "dta_time")
# # A tibble: 1 × 5
# term      estimate std.error statistic p.value
# <chr>        <dbl>     <dbl>     <dbl>   <dbl>
#   1 dta_time 0.0000924  0.000143     0.647   0.518

full_pred_sc <- mod_df_sc %>%
  filter(dta_time <= time_max) %>%
  bake(prep(rec_sc), new_data = .) %>%
  predict(extract_fit_engine(full_fit_sc),
          newdata = .,
          type = "response")


yardstick::roc_auc_vec(factor(mod_df_sc$sc), estimate = full_pred_sc, event_level = "second")
# 0.9031044

bin <- 7
time_max <- 24*60
p = seq(0, 1, length = bin + 1)

t_var_raw_sc <- mod_df_sc %>%
  filter(dta_time <= time_max) %>%
  pull(dta_time)



plot_df <- 
  mod_df_sc %>%
  filter(dta_time <= time_max) %>%
  mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
  mutate(sc = sc == "yes") %>%
  select(dta_time, sc) %>%
  mutate(pred = full_pred_sc) %>%
  group_by(dta_time) %>%
  summarise(
    n = n(),
    expected = sum(pred),
    actual = sum(sc),
    smr = actual / expected
  ) %>%
  mutate(
    left = head(unique(quantile(t_var_raw_sc, p)), -1),
    right = tail(unique(quantile(t_var_raw_sc, p)), -1),
    mid = (left + right) / 2,
    width = right - left,
    weight = 1/width
  ) %>%
  mutate(
    z = qnorm(0.95 + (1 - 0.95) / 2),
    u_ci = smr + z*sqrt(smr/n),
    l_ci = smr - z*sqrt(smr/n)
  )

plot_df <- bind_rows(plot_df, 
                     {tail(plot_df, 1) %>% mutate(left = right)})


shade_df <- unique(plot_df$left) %>%
  sort() %>%
  head(n = 2*length(.) %/% 2) %>% # slice the nearest multiple of 2 values 
  matrix(ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  mutate(min = min(pred_df$l_se, plot_df$smr, plot_df$l_ci))

x_breaks <- unique(plot_df$left, plot_df$right) %>%
  tail(-1) %>%
  head(-1) %>%
  c(0, time_max) %>%
  sort()



(smr_sc <- ggplot(plot_df, aes(x = mid/60, y = smr)) +
    # geom_step(aes(x = left/60)) +
    geom_rect(shade_df, mapping = aes(x = NULL, y = NULL, xmin = V1/60, xmax = V2/60, ymin = -Inf, ymax = Inf), alpha = 0.15) + 
    # geom_ribbon(data = pred_df, aes(x = mid/60, y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    # geom_line(data = pred_df, aes(x = mid/60, y = pred), col = bnssgcols[4]) +
    # geom_linerange(aes(xmin = left/60, xmax = right/60), linetype = 2) +
    geom_errorbar(aes(ymin = l_ci, ymax = u_ci, width = width/(2*60))) +
    # geom_vline(aes(xintercept = right/60), linetype = 2) +
    # geom_vline(aes(xintercept = left/60), linetype = 2) +
    # geom_col(aes(width = (right-left)/60),col = bnssgcols[4]) +
    # geom_line(linetype = 2) +
    geom_point() +
    # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(#limits = c(NA, (time_max/60)-1),
      breaks = x_breaks/60,
      labels = lab_fn(x_breaks),
      guide = guide_axis(n.dodge = 2)
    ) +
    # bnssgtheme() +
    theme_minimal() +
    # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, time_max/60)) +
    labs(y = "Standardised Social Care\nUtilisation Ratio",
         x = "Time Waiting for Admission"))

saveRDS(smr_sc, "data/smr_sc.RDS")

smr_mort <- readRDS("data/smr_mort.RDS")
smr_readm <- readRDS("data/smr_readm.RDS")
smr_reatt <- readRDS("data/smr_reatt.RDS")

patchwork::wrap_plots(smr_mort,
                      smr_reatt,
                      smr_readm,
                      smr_pathway,
                      smr_sc,
                      byrow = FALSE,
                      ncol = 2, axes = "collect")

ggsave("materials/scratch_smr_multiplot.png",
       last_plot(),
       height = 15,
       width = 15, 
       scale = 0.7)
