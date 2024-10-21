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
                        Organisation_Code_Commissioner,
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
  filter(Organisation_Code_Commissioner == '15C') %>% # filter for 15C as we can not pick up deaths for non BNSSG patients
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
# string_attr <- glue::glue("select distinct 
#  nhs_number
# from MODELLING_SQL_AREA.dbo.primary_care_attributes
# where attribute_period between '{report_start}' and '{report_end}'")
# 
# 
# dat_attr <- sqlQuery(con,string_attr) %>%
#   filter(!is.na(nhs_number)) %>%
#   mutate(nhs_number = as.character(nhs_number))
# 
# saveRDS(dat_attr, "data/dat_attr.RDS")
# dat_attr <- readRDS("data/dat_attr.RDS")

# arrivals for joining to sus
arrivals <- 
  dat_ecds %>%
  filter(!is.na(dta_time),
         # nhs_number %in% unique(dat_attr$nhs_number),
         !is.na(nhs_number)) %>%
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

# dat_apc <- sqlQuery(con,string_apc) %>%
#   mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS))
# 
# saveRDS(dat_apc, "data/dat_apc_new.RDS")
dat_apc <- readRDS("data/dat_apc_new.RDS")

# calculate the number of missing ICD-10 codes
is_na_or_blank <- \(x) is.na(x) | x == ""  
n_na_icd <- dat_apc %>%
  select(contains("ICD")) %>%
  as.matrix() %>%
  is_na_or_blank() %>%
  rowSums()

dat_apc$n_na_icd <- n_na_icd

dat_apc <- dat_apc %>% 
  slice_min(n_na_icd, by = c(AIMTC_Pseudo_NHS, PBRspellID))



dat_scores <- arrivals %>%
  left_join(
    dat_apc %>%
      select(-n_na_icd) %>%
      select(AIMTC_Pseudo_NHS, StartDate_HospitalProviderSpell, contains("ICD")) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  filter(!is.na(diff)) %>%
  filter(between(diff, 0 , 2))  %>%
  group_by(arr_id) %>%
  arrange(diff) %>%
  slice(1) %>%
  ungroup()   %>%
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
  filter(!is.na(diff)) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  filter(diff == min(diff)) %>% 
  arrange(diff) %>%
  slice(1) %>%
  ungroup() %>%
  select(nhs_number, arr_id, MainSpecialtyCode) 

dat_los <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  filter(!is.na(diff)) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  filter(diff == min(diff)) %>% 
  arrange(diff) %>%
  slice(1) %>%
  mutate(los = interval(StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell)/ddays(1)) %>%
  ungroup() %>%
  select(nhs_number, arr_id, los) 

dat_disch <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>%
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  filter(between(diff, 0, 2)) %>%
  filter(diff == min(diff)) %>% 
  arrange(diff) %>%
  slice(1) %>%
  select(nhs_number, arr_id, discharge_date = DischargeDate_FromHospitalProviderSpell) %>%
  distinct()


# reattendance/readmissions outcomes

arrivals_full <- dat_ecds %>%
  select(nhs_number, arrival_dt) %>%
  distinct()

admissions_full <- dat_ecds %>%
  select(nhs_number, arrival_dt, dta_dt) %>%
  filter(!is.na(dta_dt)) %>%
  distinct()

dat_reatt <- arrivals %>%
  left_join(dat_disch, by = join_by(nhs_number == nhs_number, arr_id == arr_id)) %>%
  left_join(arrivals_full, by = join_by(nhs_number == nhs_number)) %>%
  mutate(discharge_date = discharge_date %>% ymd()) %>%
  mutate(discharge_date = if_else(is.na(discharge_date), departure_dt, discharge_date)) %>%
  mutate(reatt = interval(discharge_date, arrival_dt)/ddays(1) <= days_outcome_window & # reatt within limit
           as.Date(arrival_dt) > discharge_date # 'new' admission is new (not prior)
         # |
         #   interval(departure_dt, arrival_dt)/ddays(1) <= days_outcome_window &
         #   as.Date(arrival_dt) > discharge_date
  ) %>% 
  group_by(arr_id) %>%
  slice_max(reatt, n = 1, with_ties = FALSE) %>%
  select(nhs_number, arr_id, reatt_dt = arrival_dt, reatt)

dat_readm <- arrivals %>%
  left_join(dat_disch, , by = join_by(nhs_number == nhs_number, arr_id == arr_id)) %>%
  left_join(admissions_full, by = join_by(nhs_number == nhs_number)) %>%
  mutate(discharge_date = discharge_date %>% ymd()) %>%
  mutate(discharge_date = if_else(is.na(discharge_date), departure_dt, discharge_date)) %>%
  mutate(readm = interval(discharge_date, arrival_dt)/ddays(1) <= days_outcome_window & # reaadm within limit
           as.Date(arrival_dt) > discharge_date 
         # |
         #   interval(departure_dt, arrival_dt)/ddays(1) <= days_outcome_window &
         #   as.Date(arrival_dt) > discharge_date
  ) %>% # 'new' admission is new (not prior) 
  group_by(arr_id) %>%
  slice_max(readm, n = 1, with_ties = FALSE) %>%
  select(nhs_number, arr_id, readm_dt = arrival_dt, readm)



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



 dat_adm <- 
  dat_ecds %>%
  # remove arrivals that dont have full outcome window
  filter(arrival_dt < ymd(report_end) - ddays(days_outcome_window)) %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  group_by(nhs_number) %>%
  slice_min(dta_dt, na_rm = TRUE) %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(age < 120, !is.na(age))   %>%
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
    arrival_cat#,
    # diagnoses,
    # investigation
  ) %>%
  mutate(arrival_hour = floor_date(arrival_dt, "hour")) %>%
  left_join(dat_ecds_perf) %>%
  left_join(dat_ecds_n_ed) %>% 
  left_join(dat_scores) %>%
  left_join(dat_spec) %>%
  left_join(dat_los) %>%
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  # na.omit() %>%
  distinct()

saveRDS(dat_adm, "data/dat_adm_final.RDS")
dat_adm <- readRDS("data/dat_adm_final.RDS")



# imputing missings

dat_adm <- dat_adm %>%
  recipe() %>%
  update_role(everything(), new_role = "predictor") %>%
  step_impute_knn(imd, vw_score, Acuity_Desc) %>%
  prep() %>%
  juice() 


t_max <- 24*60

mod_df <-  dat_adm %>%
  left_join(dat_reatt) %>%
  left_join(dat_readm)%>%
  left_join(mortality_df) %>% 
  filter(is.na(date_death) | (date_death >= readm_dt & date_death >= reatt_dt)) %>% 
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  filter(sex %in% 1:2) %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_time = fct_recode(arrival_time,
                                   "00:00-06:00" = "00:00:00",
                                   "06:00-12:00" = "06:00:00",
                                   "12:00-18:00" = "12:00:00",
                                   "18:00-24:00" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    nhs_number,
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    age,
    n_ed,
    Acuity_Desc,
    arrival_mode,
    arrival_cat,
    sex,
    imd,
    vw_score,
    readm,
    reatt,
    dta_time,
    ed_time
  ) %>%
  # na.omit() %>%
  distinct() %>%
  select(-nhs_number)


saveRDS(mod_df, "data/readm_reatt_mod_df_final.RDS")

mod_df <- readRDS("data/readm_reatt_mod_df_final.RDS")

library(tidyverse)
library(tidymodels)

mod_df <- mod_df %>%
  filter(ed_time < 60*30) %>%
  mutate(reatt = factor(ifelse(reatt, "yes", "no"))) %>%
  mutate(readm = factor(ifelse(readm, "yes", "no")))



mod_raw <- glm(
  formula = reatt ~ . 
  # -n_ed
  + I(age ^ 2)
  + I(perf ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2),
  data = select(mod_df, -readm, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat),
  family = binomial(link = "logit")
) 

mod_raw %>%
  broom::tidy() %>% 
  filter(str_detect(term, "dta_time"))

vip::vip(mod_raw)

raw_pred = predict(mod_raw, mod_df, type = "response")

yardstick::roc_auc_vec(mod_df$reatt, estimate = raw_pred, event_level = "second")


mod_raw %>%
  jtools::effect_plot(pred = "dta_time", interval = TRUE) +
  # scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.02)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal()


mod_raw <- glm(
  formula = readm ~ . 
  # -n_ed
  + I(age ^ 2)
  + I(perf ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2),
  data = select(mod_df, -reatt, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat),
  family = binomial(link = "logit")
) 

mod_raw %>%
  broom::tidy() %>% 
  filter(str_detect(term, "dta_time"))

vip::vip(mod_raw, num_features = 20)

raw_pred = predict(mod_raw, mod_df, type = "response")

yardstick::roc_auc_vec(mod_df$readm, estimate = raw_pred, event_level = "second")


readm_effects_plot <- mod_raw %>%
  jtools::effect_plot(pred = "dta_time", interval = TRUE) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.02)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model Estimated 30-day Readmission Rate.", 50)) +
  theme_minimal()


ggplot2::ggsave("materials/readm_model_effects_plot.png",
                readm_effects_plot,
                bg = "white",
                height = 7.5,
                width = 10,
                scale = 0.5)

saveRDS(readm_effects_plot, "data/readm_effects_plot.RDS")
