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


dat_disch <- arrivals %>%
  left_join(
    dat_apc %>%
      select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell, DischargeDate_FromHospitalProviderSpell) %>%
      mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
      filter(!is.na(AIMTC_Pseudo_NHS)),
    by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
  mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
  group_by(arr_id) %>%
  filter(diff == min(diff), diff <= 1) %>%
  select(nhs_number, arr_id, discharge_date = DischargeDate_FromHospitalProviderSpell) %>%
  distinct()


# reattendance/readmissions outcomes

days_reatt_adm <- 30

arrivals_full <- dat_ecds %>%
  select(nhs_number, arrival_dt) %>%
  distinct()

admissions_full <- dat_ecds %>%
  select(nhs_number, arrival_dt, dta_dt) %>%
  filter(!is.na(dta_dt)) %>%
  distinct()

dat_reatt <- arrivals %>%
  left_join(dat_disch, , by = join_by(nhs_number == nhs_number, arr_id == arr_id)) %>%
  left_join(arrivals_full, by = join_by(nhs_number == nhs_number)) %>%
  filter(!is.na(discharge_date)) %>%
  mutate(reatt = interval(discharge_date, arrival_dt)/ddays(1) <= days_reatt_adm & # reatt within limit
           as.Date(arrival_dt) > discharge_date) %>% # 'new' admission is new (not prior) 
  group_by(arr_id) %>%
  slice_max(reatt, n = 1, with_ties = FALSE) %>%
  select(nhs_number, arr_id, reatt)

dat_readm <- arrivals %>%
  left_join(dat_disch, , by = join_by(nhs_number == nhs_number, arr_id == arr_id)) %>%
  left_join(admissions_full, by = join_by(nhs_number == nhs_number)) %>%
  mutate(readm = interval(discharge_date, arrival_dt)/ddays(1) <= days_reatt_adm & # reatt within limit
           as.Date(arrival_dt) > discharge_date) %>% # 'new' admission is new (not prior) 
  group_by(arr_id) %>%
  slice_max(readm, n = 1, with_ties = FALSE) %>%
  select(nhs_number, arr_id, readm)


dat_adm <- dat_ecds %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  group_by(nhs_number) %>%
  slice_min(dta_dt, na_rm = TRUE) %>%
  filter(dta_time >= 0) %>% # some records are negative 0 exactly, seems to be DQ
  filter(age < 120, !is.na(age)) %>%
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
  left_join(dat_reatt) %>%
  left_join(dat_readm) %>%
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  na.omit()

# saveRDS(dat_adm, "data/dat_adm_new.RDS")
# dat_adm <- readRDS("data/dat_adm_new.RDS")


t_max <- 24*60

mod_df <- dat_adm %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  filter(sex %in% 1:2) %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  mutate(age_sq = age^2) %>%
  mutate(perf_sq = perf^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(vw_score_sq = vw_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(reatt = factor(ifelse(reatt, "yes", "no"), levels = c("no", "yes"))) %>%
  mutate(readm = factor(ifelse(readm, "yes", "no"), levels = c("no", "yes"))) %>%
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
    Acuity_Desc,
    arrival_mode,
    arrival_cat,
    diagnoses,
    investigation,
    # n_adm,    # bring these back in when they aren't all
    # n_adm_sq,
    sex,
    imd,
    vw_score,
    vw_score_sq,
    reatt,
    readm,
    dta_time,
    ed_time
  ) %>%
  na.omit()

saveRDS(mod_df, "data/smr_mod_df_reatt_readm.RDS")
# mod_df <- readRDS("data/smr_mod_df_new.RDS")

# reatt model
set.seed(123)
split <- initial_split(mod_df, strata = reatt) 
glm_spec <- logistic_reg()

rec <- recipe(reatt ~ ., data = training(split)) %>%
  update_role(readm, new_role = "extra outcome") %>%
  update_role(ed_time, new_role = "not for prediction") %>%
  # update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)

fit <- last_fit(wf, split)

augment(fit) %>%
  roc_curve(reatt,
            .pred_yes,
            event_level = "second") %>%
  autoplot()

# fit on all data

rec <- recipe(reatt ~ ., data = mod_df) %>%
  update_role(readm, new_role = "extra outcome") %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)



full_fit <- fit(wf, mod_df)
# predictions 
full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")
yardstick::roc_auc_vec(mod_df$reatt, estimate = full_pred, event_level = "second")


# smr

bin <- 7
time_max <- 24*60


p = seq(0, 1, length = bin + 1)

t_var_raw <- mod_df %>%
  filter(dta_time <= time_max) %>%
  pull(dta_time)

full_pred <- mod_df %>%
  filter(dta_time <= time_max) %>%
  bake(prep(rec), new_data = .) %>%
  predict(extract_fit_engine(full_fit),
          newdata = .,
          type = "response")

plot_df <- 
  mod_df %>%
  filter(dta_time <= time_max) %>%
  mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
  mutate(readm = readm == "yes") %>%
  select(dta_time, readm) %>%
  mutate(pred = full_pred) %>%
  group_by(dta_time) %>%
  summarise(
    n = n(),
    expected = sum(pred),
    actual = sum(readm),
    smr = actual / expected
  ) %>%
  mutate(
    left = head(unique(quantile(t_var_raw, p)), -1),
    right = tail(unique(quantile(t_var_raw, p)), -1),
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



pred_df <- lm(smr ~ mid, weights = weight, data = plot_df) %>%
  predict(newdata = plot_df, se.fit = TRUE) %>%
  as_tibble() %>%
  transmute(pred = fit,
            u_se = fit + se.fit,
            l_se = fit - se.fit) %>%
  bind_cols(select(plot_df, mid)) 


pred_df <- bind_rows(pred_df, {
  #predict at limits
  lm(smr ~ mid, weights = weight, data = plot_df) %>%
    predict(newdata = data.frame(mid = range(plot_df$left, plot_df$right)), se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit,
              u_se = fit + se.fit,
              l_se = fit - se.fit) %>%
    mutate(mid = range(plot_df$left, plot_df$right))
})


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

lab_fn <- function(x) {
  
  x <- round(x)
  glue::glue("{x%/%60}h {x - (60*x%/%60)}m") |>
    str_replace_all(" 0m", "") |>
    str_replace_all("^0h ", "")
}


(smr_reatt <- ggplot(plot_df, aes(x = mid/60, y = smr)) +
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
  labs(y = "Standardised Re-Attendance\nRatio",
       x = "Time Waiting for Admission"))

saveRDS(smr_reatt, "data/smr_reatt.RDS")


# readm model

rec <- recipe(readm ~ ., data = mod_df) %>%
  update_role(reatt, new_role = "extra outcome") %>%
  # update_role(ed_time, new_role = "not for prediction") %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)



full_fit <- fit(wf, mod_df)
full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")
yardstick::roc_auc_vec(mod_df$readm, estimate = full_pred, event_level = "second")

# predictions 

t_var_raw <- mod_df %>%
  filter(dta_time <= time_max) %>%
  pull(dta_time)

full_pred <- mod_df %>%
  filter(dta_time <= time_max) %>%
  bake(prep(rec), new_data = .) %>%
  predict(extract_fit_engine(full_fit),
          newdata = .,
          type = "response")

plot_df <- 
  mod_df %>%
  filter(dta_time <= time_max) %>%
  mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
  mutate(readm = readm == "yes") %>%
  select(dta_time, readm) %>%
  mutate(pred = full_pred) %>%
  group_by(dta_time) %>%
  summarise(
    n = n(),
    expected = sum(pred),
    actual = sum(readm),
    smr = actual / expected
  ) %>%
  mutate(
    left = head(unique(quantile(t_var_raw, p)), -1),
    right = tail(unique(quantile(t_var_raw, p)), -1),
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



pred_df <- lm(smr ~ mid, weights = weight, data = plot_df) %>%
  predict(newdata = plot_df, se.fit = TRUE) %>%
  as_tibble() %>%
  transmute(pred = fit,
            u_se = fit + se.fit,
            l_se = fit - se.fit) %>%
  bind_cols(select(plot_df, mid)) 


pred_df <- bind_rows(pred_df, {
  #predict at limits
  lm(smr ~ mid, weights = weight, data = plot_df) %>%
    predict(newdata = data.frame(mid = range(plot_df$left, plot_df$right)), se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit,
              u_se = fit + se.fit,
              l_se = fit - se.fit) %>%
    mutate(mid = range(plot_df$left, plot_df$right))
})


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



(smr_readm <- ggplot(plot_df, aes(x = mid/60, y = smr)) +
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
  labs(y = "Standardised Re-Admission\nRatio",
       x = "Time Waiting for Admission"))

saveRDS(smr_readm, "data/smr_readm.RDS")
