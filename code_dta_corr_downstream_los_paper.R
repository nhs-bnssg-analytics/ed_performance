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
  filter(dta_time > 0) %>%
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

saveRDS(dat_attr, "data/dat_attr.RDS")
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


dat_los <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(diff == min(diff), diff < 2) %>% 
  mutate(los = interval(StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell)/ddays(1)) %>%
  select(nhs_number, arr_id, los) %>%
  distinct()



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


dat_adm <- dat_ecds %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  group_by(nhs_number) %>%
  slice_min(dta_dt, na_rm = TRUE) %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(age < 120, !is.na(age)) %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
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
  left_join(dat_los) %>%
  left_join(dat_spec) %>%
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  na.omit()

saveRDS(dat_adm, "data/dat_adm_new_LOS.RDS")

dat_adm_los <- readRDS("data/dat_adm_new_LOS.RDS")

(los_plot <- local({
  
  plot_df <- dat_adm_los %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    distinct() %>%
    filter(between(dta_time, 0, 60 * 24)) %>%
    filter(los < 75) 
  
  plot_df %>%
    mutate(dta_time = 30*dta_time%/%30) %>%
    ungroup %>%
    nest() %>%
    expand_grid(q = c(0.5, 0.9)) %>%
    mutate(data = map2(data, q,
                       ~.x %>% group_by(dta_time) %>% summarise(los = quantile(los, .y))
    ))%>%
    unnest(cols = c(data)) %>%
    mutate(q = scales::percent(q) %>% factor(labels = c("Median", "90% quantile"))) %>%
    ggplot(aes(x = dta_time/60, y = los, shape = q)) +
    geom_point(data = plot_df, aes(shape = "All data"),  alpha = 0.015) +
    # geom_smooth(method = "lm", show.legend = FALSE) +
    geom_point(size = 2.5, fill = "white") +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 45, 10)) +
    guides(alpha = "none") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text()) +
    labs(shape = "",
         x = "Time waiting for admission (hour)",
         y = "In-hospital length of stay (day)",
         colour = "Total length of stay")
})
)


ggsave("materials/los_downstream_paper.png",
       los_plot,
       bg = "white",
       height = 7.5,
       width = 7.5, 
       scale = 0.7)



dat_adm_los %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  distinct() %>%
  filter(between(dta_time, 0, 60 * 24)) %>%
  filter(los < 75)  %>%
  mutate(dta_time = 30*dta_time%/%30) %>%
  ungroup %>%
  nest() %>%
  expand_grid(q = c(0.5, 0.9)) %>%
  mutate(data = map2(data, q,
                     ~.x %>% group_by(dta_time) %>% summarise(los = quantile(los, .y))
                     )
         ) %>%
  mutate(mod = map(data, ~lm(los ~ dta_time, data = .x))) %>%
  mutate(stats = map(mod, broom::tidy)) %>%
  unnest(cols = c(stats))



# standardised model

t_max <- 24*60

mod_df_los <- dat_adm_los %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  # mutate(age = cut(age, breaks = seq(0, 120, 10), include.lowest = TRUE)) %>%
  # mutate(dta_time = dta_time > 4*60) %>%
  # mutate(dta_time = cut(dta_time, breaks = 60*seq(0, 24, 4), include.lowest = TRUE)) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(vw_score_sq = vw_score^2) %>%
  mutate(dta_time = dta_time/60) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    age,
    age_sq,
    n_ed,
    n_ed_sq,
    sex,
    imd,
    vw_score,
    vw_score_sq,
    los,
    dta_time
  )

lm(los ~ ., data = mod_df_los) %>%
  summary()
