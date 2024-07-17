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
  select(-arr_id) %>%
  filter(MainSpecialtyCode %in% c(300, 326, 430)) %>% # filter General Internal Medicine
  select(-MainSpecialtyCode) %>%
  na.omit()

saveRDS(dat_adm, "data/dat_adm_new.RDS")


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
days_mort <- 7

mod_df <- dat_adm %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  left_join(mortality_df) %>%
  # mutate(age = cut(age, breaks = seq(0, 120, 10), include.lowest = TRUE)) %>%
  # mutate(dta_time = dta_time > 4*60) %>%
  # mutate(dta_time = cut(dta_time, breaks = 60*seq(0, 24, 4), include.lowest = TRUE)) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(vw_score_sq = vw_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  mutate(mort_30 = factor(ifelse(mort_30, "yes", "no"), levels = c("no", "yes"))) %>%
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
   dta_time,
   ed_time
  )

saveRDS(mod_df, "data/smr_mod_df_new.RDS")
mod_df <- readRDS("data/smr_mod_df_new.RDS")

split <- initial_split(mod_df, strata = mort_30) 
glm_spec <- logistic_reg()

rec <- recipe(mort_30 ~ ., data = training(split)) %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  step_novel(all_nominal_predictors(), new_level = "novel") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)

fit <- last_fit(wf, split)

augment(fit) %>%
  roc_curve(mort_30,
            .pred_yes,
            event_level = "second") %>%
  autoplot()

# fit on all data

full_fit <- fit(wf, mod_df)

# predictions 
full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")

# smr

smr_plot_quant <- function(t_var,
                           time_max = 30*60,
                           n_bin = 10,
                           x_lab = "Time waiting for admission"){ 
  
  p = seq(0, 1, length = n_bin + 1)
  
  t_var_raw <- mod_df %>%
    filter({{t_var}} <= time_max) %>%
    pull({{t_var}})
  
  full_pred <- mod_df %>%
    filter({{t_var}} <= time_max) %>%
    bake(prep(rec), new_data = .) %>%
    predict(extract_fit_engine(full_fit),
            newdata = .,
            type = "response")
  
  plot_df <- 
    mod_df %>%
    filter({{t_var}} <= time_max) %>%
    mutate({{t_var}} := cut({{t_var}}, breaks = unique(quantile({{t_var}}, p)), include.lowest = TRUE)) %>%
    mutate(mort_30 = mort_30 == "yes") %>%
    select({{t_var}}, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by({{t_var}}) %>%
    summarise(
      n = n(),
      expected_mort = sum(pred_mort),
      actual_mort = sum(mort_30),
      smr = actual_mort / expected_mort
    ) %>%
    mutate(
      left = head(unique(quantile(t_var_raw, p)), -1),
      right = tail(unique(quantile(t_var_raw, p)), -1),
      mid = (left + right) / 2,
      width = right - left,
      weight = 1/width
    )
  
  plot_df <- bind_rows(plot_df, 
                       {tail(plot_df, 1) %>% mutate(left = right)})
  
  
  
  pred_df <- lm(smr~mid,
                weights = weight,
                data = plot_df) %>%
    predict(newdata = plot_df, se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit, u_se = fit + se.fit, l_se = fit - se.fit) %>%
    bind_cols(select(plot_df, mid))
  # 
  
  x_lab = glue::glue("{x_lab}\n(hours, {n_bin} bins)")
  ggplot(plot_df, aes(x = mid/60, y = smr)) +
    geom_step(aes(x = left/60)) +
    geom_ribbon(data = pred_df, aes(x = mid/60, y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    geom_line(data = pred_df, aes(x = mid/60, y = pred), col = bnssgcols[4]) +
    # geom_linerange(aes(xmin = left/60, xmax = right/60)) +
    # geom_vline(aes(xintercept = right/60), linetype = 2) +
    geom_point(col = bnssgcols[4]) +
    bnssgtheme() +
    # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, time_max/60)) +
    labs(y = "Standardised Mortality Ratio",
         x = x_lab)
  
}

map(seq(5, 10, 1), ~smr_plot_quant(dta_time,
                                   time_max = 10*60,
                                   n_bin = .x)) %>%
  patchwork::wrap_plots(axes = "collect")


ggsave("materials/smr_quant_bin.png",
       last_plot(),
       height = 7.5,
       width = 12.5, 
       scale = 0.9)






smr_plot <- function(t_var, bin = 2*60, x_lab = "Time waiting for admission"){
  
  
  plot_df <- 
  mod_df %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    mutate({{t_var}} := (bin)*({{t_var}} %/% (bin))) %>%
    select({{t_var}}, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by({{t_var}}) %>%
    summarise(
      n = n(),
      expected_mort = sum(pred_mort),
      actual_mort = sum(mort_30 == "yes"),
      smr = actual_mort / expected_mort
    ) %>%
    filter(n > 50)
  
  df_mod <- bind_cols(x = pull(plot_df, {{t_var}}), y = pull(plot_df, smr), n = pull(plot_df, n))
  
    pred_df <- lm(y~x, weights = n, data = df_mod) %>%
    predict(newdata = df_mod, se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit, u_se = fit + se.fit, l_se = fit - se.fit) %>%
    bind_cols(select(plot_df, {{t_var}}))

  
   x_lab = glue::glue("{x_lab} (hours, {bin/60}-hour bins)")
    ggplot(plot_df, aes(x = {{t_var}}/60, y = smr)) +
    # geom_smooth(method = "lm", col = bnssgcols[4]) +
    geom_ribbon(data = pred_df, aes(y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    geom_line(data = pred_df, aes(y = pred), col = bnssgcols[4]) +
    geom_point(col = bnssgcols[4]) +
    bnssgtheme() +
    labs(y = "Standardised Mortality Ratio",
         x = x_lab)
}


smr_plots <- expand_grid(t_var = vars(dta_time, ed_time),
            bin = 60*c(1:4)) %>%
  mutate(x_lab = rep(c("Time waiting for admission", "Time in ED"), each = 4)) %>%
  mutate(smr_p = pmap(list(t_var, bin, x_lab), ~smr_plot(..1, ..2, ..3))) %>%
  select(-x_lab) %>%
  pivot_wider(id_cols = c(bin), names_from = t_var, values_from = smr_p)


map2(smr_plots$`~dta_time`, smr_plots$`~ed_time`, patchwork::wrap_plots, axes = "collect") %>%
  walk2(smr_plots$bin,
        ~ ggsave(
          glue::glue(
            "materials/smr_plot_bin-{..2/60}.png"
          ),
          ..1,
          height = 5,
          width = 15,
          scale = 0.9
        ))
        

pwalk(list(smr_plots$smr_p,
            smr_plots$x_lab,
            smr_plots$bin),
      ~ggsave(glue::glue("materials/smr_plot_{snakecase::to_snake_case(..2)}_bin-{..3/60}.png"),
              ..1,
              height = 5,
              width = 7.5, 
              scale = 0.9))


ggsave("materials/smr_plot_adm.png",
       smr_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)



dat_adm %>%
  left_join(mortality_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(dta_time = 120*(dta_time %/% 120)) %>%
  mutate(mort_30 = ifelse((
    interval(arrival_dt, date_death) / ddays(1)
  ) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(dta_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  filter(dta_time < 24*60, dta_time > 0) %>%
  mutate(dta_time = cut(dta_time,
                       breaks = 60*c(0, 4, 8, 12, 20, Inf),
                       labels = c("0-4 hours",
                                  "4-8 hours",
                                  "8-12 hours",
                                  "12-20 hours",
                                  "20-24 hours")
  )) %>%
  # mutate(dta_time = cut(dta_time,
  #                      breaks = 60*c(0, 4, 10, 16, 20, Inf),
  #                      labels = c("0-4 hours",
  #                                 "4-10 hours",
  #                                 "10-16 hours",
  #                                 "16-20 hours",
  #                                 "20-24 hours"
  #                      )
  # )) %>%
  group_by(dta_time) %>%
  summarise(n = n(),
            expected_mort_rate = sum(pred_mort)/n,
            actual_mort_rate = sum(mort_30)/n,
            nnh = 1/(actual_mort_rate - expected_mort_rate)) %>%
  show_in_excel()



dat_adm %>%
  left_join(mortality_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(dta_time = 30*(dta_time %/% 30)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(dta_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  group_by(dta_time) %>%
  summarise(
    n = n(),
    expected_mort = sum(pred_mort),
    actual_mort = sum(mort_30),
    smr = actual_mort / expected_mort
  ) %>%
  filter(n > 50,  dta_time > 0) %>%
  mutate(dta_time = dta_time/60) %>%
  lm(smr ~ dta_time, data = .) %>%
  summary()


(smr_plot_ed <- dat_adm %>%
    left_join(mortality_df) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    mutate(ed_time = (2*60) * (ed_time %/% (2*60))) %>%
    mutate(mort_30 = ifelse((
      interval(arrival_dt, date_death) / ddays(1)
    ) < days_mort, TRUE, FALSE)) %>%
    mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
    select(ed_time, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by(ed_time) %>% 
    summarise(
      n = n(),
      expected_mort = sum(pred_mort),
      actual_mort = sum(mort_30),
      smr = actual_mort / expected_mort
    ) %>%
    filter(n > 50,  ed_time > 0) %>%
    ggplot(aes(x = ed_time/60, y = smr)) +
    geom_smooth(method = "lm",col = bnssgcols[4]) +
    geom_point(col = bnssgcols[4]) +
    bnssgtheme() +
    labs(y = "Standardised Mortality Ratio", x = "Time waiting in ED (hour)"))

ggsave("materials/smr_plot_ed_2hr.png",
       smr_plot_ed,
       height = 5,
       width = 7.5, 
       scale = 0.9)

show_in_excel <- function(.data){
  
  tmp <- paste0(tempfile(), ".csv")
  
  write.csv(.data, tmp)
  
  fs::file_show(path = tmp)
  
}

dat_adm %>%
  left_join(mortality_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(ed_time = (2*60)*(ed_time %/% (2*60))) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(ed_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  filter(ed_time < 24*60, ed_time > 0) %>%
  mutate(ed_time = cut(ed_time,
                        breaks = 60*c(0, 4, 10, 16, 20, Inf),
                        labels = c("0-4 hours",
                                   "4-10 hours",
                                   "10-16 hours",
                                   "16-20 hours",
                                   "20-24 hours"
                                   )
                       )) %>%
  group_by(ed_time) %>%
  summarise(n = n(),
            expected_mort_rate = sum(pred_mort)/n,
            actual_mort_rate = sum(mort_30)/n,
            nnh = 1/(actual_mort_rate - expected_mort_rate)) %>%
  show_in_excel()




(smr_plot <- dat_adm %>%
    left_join(mortality_df) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    mutate(ed_time_cut = cut(ed_time, breaks = quantile(ed_time, seq(0, 1, 0.025)), include.lowest = TRUE)) %>%
    mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
    mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
    select(ed_time, ed_time_cut, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by(ed_time_cut) %>%
    summarise(n = n(), ed_time = min(ed_time), expected_mort = sum(pred_mort), actual_mort = sum(mort_30), smr = actual_mort/expected_mort) %>%
    # filter(n > 50,  ed_time > 0) %>%
    ggplot(aes(x = ed_time, y = smr)) +
    geom_point() +
    geom_smooth(method = "lm") +
    bnssgtheme() +
    labs(y = "SMR", x = "Time waiting in ED"))



dat_adm %>%
  left_join(mortality_df) %>%
  mutate(ed_time = Hmisc::cut2(ed_time
                               