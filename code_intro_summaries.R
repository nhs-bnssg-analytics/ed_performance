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
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start) - ddays(pre_window_days)}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
      'Emergency department discharge to same day emergency care service',
      'Emergency department discharge to ambulatory emergency care service',
      'Emergency department discharge to neonatal intensive care unit',
      'Emergency department discharge to special care baby unit',
      'Admission to the mortuary',
      'Discharge to home',
      'Discharge to hospital at home service',
      'Discharge to nursing home',
      'Discharge to residential home',
      'Discharge to police custody',
      'Patient discharge, to legal custody',
      'Urgent admission to hospice'
      )
      ")

dat_ecds_raw<-sqlQuery(con,string_ecds)
# saveRDS(dat_ecds_raw, "data/dat_ecds_raw.RDS")
dat_ecds_raw <- readRDS("data/dat_ecds_raw.RDS")



dat_ecds<-dat_ecds_raw %>%
  filter(Organisation_Code_Commissioner == '15C') %>% # filter for 15C as we can not pick up deaths for non BNSSG patients
  mutate(NHS_Number = as.character(NHS_Number)) %>%
  mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
  mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
  mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  select(
    nhs_number = NHS_Number,
    tfc = Treatment_Function_Code,
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
    destination_desc,
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



string_ecds_full <-glue::glue("select 
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
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start) - ddays(pre_window_days)}' and '{report_end}'
      ")

dat_ecds_full_raw<-sqlQuery(con,string_ecds_full)

dat_ecds_full <- dat_ecds_full_raw %>%
  filter(Organisation_Code_Commissioner == '15C') %>% # filter for 15C as we can not pick up deaths for non BNSSG patients
  mutate(NHS_Number = as.character(NHS_Number)) %>%
  mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
  mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
  mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  select(
    nhs_number = NHS_Number,
    tfc = Treatment_Function_Code,
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
    destination_desc,
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


# saveRDS(dat_apc, "data/dat_apc_new.RDS")
dat_apc <- readRDS("data/dat_apc_new.RDS")

# full count of medical admissions

# arrivals for joining to sus
arrivals <- 
  dat_ecds %>%
  filter(!is.na(dta_time),
         arrival_dt >= report_start,
         # nhs_number %in% unique(dat_attr$nhs_number),
         !is.na(nhs_number)) %>%
  group_by(nhs_number, arr_id) %>%
  summarise(
    tfc = tfc[1],
    departure_dt = as.Date(departure_dt)[1]
  )


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


dat_spec <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, as_date(StartDate_HospitalProviderSpell))/ddays(1))) %>%
  filter(!is.na(diff)) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  filter(diff == min(diff)) %>% 
  arrange(diff) %>%
  slice(1) %>%
  select(nhs_number, arr_id, tfc, MainSpecialtyCode) %>%
  mutate(tfc = ifelse(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
  mutate(MainSpecialtyCode = ifelse(MainSpecialtyCode %in% c(300, 326, 430), "medical", "not medical")) %>% 
  mutate(spec = ifelse(tfc == "medical" | MainSpecialtyCode == "medical", "medical", "not medical")) %>%
  select(nhs_number, arr_id, spec)


time_to_dta <- dat_ecds %>%
  filter(!is.na(dta_dt)) %>%
  mutate(time_int = interval(arrival_dt, dta_dt)/dminutes(1)) %>%
  # mutate(time_int = interval(arrival_dt, pmin(dta_dt, conclusion_dt, departure_dt, na.rm = TRUE))/dminutes(1)) %>%
  pull(time_int) %>%
  keep(.p = is.finite) %>%
  mean()
time_to_dta


dat_adm <- dat_ecds %>%
  # remove arrivals that dont have full outcome window
  filter(arrival_dt < ymd(report_end) - ddays(days_outcome_window)) %>%
  left_join(dat_spec) %>% 
  mutate(nhs_number = as.character(nhs_number)) 


# spec proportion (although this doesn't align with what I have in the paper..)
adm_prop <- dat_adm %>%
  filter(!is.na(spec)) %>%
  count(spec) %>%
  pull(n) %>%
  proportions()


dat_adm %>%
  filter(!is.na(spec)) %>%
  summarise(count = n(),
            delay_time = median(interval(dta_dt, pmax(conclusion_dt, departure_dt, na.rm = TRUE))/dminutes(1), na.rm = TRUE),
            .by = spec) %>%
  mutate(prop = count/sum(count)) %>%
  mutate(delay_prop = delay_time*prop/(sum(delay_time*prop)))


dat_adm <- dat_adm %>%
  filter(spec == "medical") 

n_adm_1 <-  dat_adm %>%
  nrow()


dat_adm <- dat_adm %>%
  filter(spec == "medical") %>%
  slice_min(arrival_dt, by = nhs_number)

n_adm_2 <- dat_adm %>% nrow()

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

dat_los <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, as_date(StartDate_HospitalProviderSpell))/ddays(1))) %>%
  filter(!is.na(diff)) %>%
  group_by(arr_id) %>%
  filter(between(diff, 0, 2)) %>%
  filter(diff == min(diff)) %>% 
  arrange(diff) %>%
  slice(1) %>%
  mutate(los = interval(StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell)/ddays(1)) %>%
  ungroup() %>%
  select(nhs_number, arr_id, los) 


dat_adm <- dat_adm %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(between(age, 0, 120), !is.na(age))   %>% # DQ check on age which can be missing or recorded incorrectly
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
    arrival_cat
  ) %>%
  mutate(arrival_hour = ceiling_date(arrival_dt, "4 hour")) %>%
  left_join(dat_scores) %>%
  left_join(dat_los) %>%
  select(-arr_id) %>%
  na.omit() %>%
  distinct()


