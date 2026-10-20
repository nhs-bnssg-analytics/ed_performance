# notes use the number of dtas to try to explain downstream effects on patients
# (LOS/Discharge pathway), adding in some attributes to try to control for
# casemix

require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)


report_start <- '2022-01-01'
report_end <- '2024-05-01'
pre_window_days <- 60


dta_df <- local({
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
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date >= '{ymd(report_start) - ddays(pre_window_days)}'
      ")
  
  dat_ecds_raw<-sqlQuery(con,string_ecds)
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
      arrival_dt,
      initial_dt,
      treatment_dt,
      dta_dt,
      departure_dt,
      conclusion_dt,
      site = Site_Name,
      Acuity_Desc
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
  
  
  dat_ecds_perf <- dat_ecds %>%
    group_by(site, arrival_hour = floor_date(arrival_dt, unit = "hour")) %>%
    summarise(perf = 1-mean(breach))
  
  string_attr <- {glue::glue("SELECT
 a.[nhs_number]
,c.[Index of Multiple Deprivation (IMD) Decile] as IMD
,e.[cambridge_score]
,e.[cancer]
,e.[painful_conditions]
,e.[psychosis_bipolar]
,e.[thyroid]
,e.[substance_misus]
,e.[hearing_loss]
,e.[chd]
,e.[ckd]
,e.[hypertension]
,e.[stroke_tia]
,e.[diabetes]
,e.[anorexia_bulimia]
,e.[anxiety_depression]
,e.[ibd]
,e.[ibs]
,e.[epilepsy]
,e.[ms]
,e.[atrial_fribrillation]
,e.[copd]
,e.[alc_dependency]
,e.[heart_failure]
,e.[asthma]
,e.[dementia]
,e.[learning_dis]
,a.[age]
,a.[sex]
,a.[bmi]
,a.[smoking]
,d.[main group] as ethnicity
--,a.[qof_af]
--,a.[qof_chd]
--,a.[qof_hf]
--,a.[qof_ht]
,a.[qof_pad]
--,a.[qof_stroke]
--,a.[qof_asthma]
--,a.[qof_copd]
,a.[qof_obesity]
--,a.[qof_ckd]
--,a.[qof_diabetes]
,a.[qof_pall]
--,a.[qof_dementia]
--,a.[qof_epilepsy]
--,a.[qof_learndis]
--,a.[qof_mental]
,a.[qof_osteoporosis]
,a.[qof_rheumarth]
	  ,CASE WHEN a.dep_alcohol = 1 OR
               a.dep_opioid = 1 OR
               a.dep_cocaine = 1 OR
               a.dep_cannabis = 1 OR
               a.dep_benzo = 1 OR
               a.dep_other = 1 THEN 1 ELSE 0 END AS 'addiction_flag'
	  ,CASE WHEN a.[age]>=18 THEN (
        COALESCE(convert(int,a.[disorder_pers]) , 0) +
        COALESCE(convert(int,a.[SAD]) , 0) +
        COALESCE(convert(int,a.[ptsd]) , 0) +
        COALESCE(convert(int,a.[qof_depression]) , 0) +
        COALESCE(convert(int,a.[qof_mental]) , 0)+
        COALESCE(convert(int,a.[disorder_eating]) , 0) +
        COALESCE(convert(int,a.[self_harm]) , 0)  
                                    )
        WHEN a.[age]<18 and (COALESCE(convert(int,a.[depression]) , 0) = 1 or
                             COALESCE(convert(int,a.[SAD]) , 0) = 1) then 1 else 0 +  
        COALESCE(convert(int,a.[disorder_pers]) , 0) +
        COALESCE(convert(int,a.[ptsd]) , 0) +
        COALESCE(convert(int,a.[qof_mental]) , 0)+
        COALESCE(convert(int,a.[disorder_eating]) , 0) +
        COALESCE(convert(int,a.[SAD]) , 0) +
        COALESCE(convert(int,a.[self_harm]) , 0)   END AS 'number_of_mh_flags'
FROM
  (
	select
	nhs_number
	,max(attribute_period) as max_date
	,count(attribute_period) as date_count
	from
	MODELLING_SQL_AREA.dbo.primary_care_attributes
	where attribute_period between '{report_start}' and '{report_end}'
	group by nhs_number 
  ) b
  INNER JOIN MODELLING_SQL_AREA.dbo.primary_care_attributes a
  ON a.[nhs_number] = b.[nhs_number] and a.[attribute_period] = b.[max_date]
  LEFT JOIN Analyst_SQL_Area.dbo.tbl_BNSSG_Datasets_LSOA_IMD_2019 c
  ON a.lsoa = c.[LSOA code]
  LEFT JOIN modelling_sql_area.dbo.swd_ethnicity_groupings d
  ON a.ethnicity = d.ethnicity_description
  LEFT JOIN [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] e
  ON e.[nhs_number] = b.[nhs_number] and e.[attribute_period] = a.[attribute_period]
")}
  
  attr <- RODBC::sqlQuery(con, string_attr) %>%
    mutate(nhs_number = as.character(nhs_number))
  
  
  string_swd <- glue::glue("SELECT
      [nhs_number]
      ,[arr_date]
      ,[dep_date]
      ,[prov_code]
      ,[spec_l1a]
      ,[spec_l1b]
  FROM [MODELLING_SQL_AREA].[swd].[activity]
  where pod_l2a = 'ip'
  and cast(arr_date as date) between '{report_start}' and '{report_end}'
")
  
  dat_swd <- RODBC::sqlQuery(con, string_swd) %>%
    mutate(nhs_number = as.character(nhs_number))
  
  dat_swd <- RODBC::sqlQuery(con, string_swd) %>% 
    mutate(nhs_number = as.character(nhs_number))
  
  dat_adm_spec <- dat_ecds %>%
    filter(!is.na(dta_dt), dta_dt >= ymd(report_start)) %>%
    mutate(adm_dep_date = as.Date(departure_dt)) %>%
    left_join(dat_swd, by = join_by(nhs_number == nhs_number, adm_dep_date == arr_date)) %>% 
    mutate(specialty = coalesce(spec_l1b, "unknown")) %>%
    group_by(arr_id) %>%
    slice(1) %>%
    select(arr_id, specialty) %>%
    ungroup()
  
  dat_dtas <- dat_ecds %>%
    mutate(nhs_number = as.character(nhs_number)) %>%
    group_by(nhs_number) %>%
    filter(dta_dt == max(dta_dt, na.rm = TRUE)) %>%
    ungroup() %>%
    select(site, nhs_number, arr_id, arrival_dt, dta_dt, departure_dt, dta_time, ed_time) %>%
    mutate(arrival_hour = floor_date(arrival_dt, "hour")) %>%
    left_join(dat_ecds_perf) %>%
    left_join(dat_ecds_n_ed) %>% 
    left_join(select(attr, nhs_number, age, sex, imd = IMD, cambridge_score)) %>%
    left_join(dat_adm_spec) %>%
    select(-arr_id) %>%
    na.omit()
  dat_dtas
})

# saveRDS(dta_df, "data/smr_dta_df.RDS")
# dta_df <- readRDS("data/smr_dta_df.RDS")

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


mod_df <- dta_df %>%
  left_join(mortality_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < 30, TRUE, FALSE)) %>%
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
    # n_adm,
    # n_adm_sq,
    sex,
    imd,
    cambridge_score,
    cambridge_score_sq,
    specialty,
    mort_30
  )

saveRDS(mod_df, "data/smr_mod_df.RDS")
mod_df <- readRDS("data/smr_mod_df.RDS")

# modelling

split <- initial_split(mod_df, strata = mort_30) 
glm_spec <- logistic_reg()

rec <- recipe(mort_30 ~ ., data = training(split)) %>%
  step_other(all_nominal_predictors()) %>%
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

# lr_metrics <- probably::threshold_perf(fit$.predictions[[1]],
#                                        mort_30,
#                                        `.pred_yes`,
#                                        event_level = "second",
#                                        thresholds = seq(0, 1, 0.01))
# 
# lr_metrics %>%
#   ggplot(aes(x = .threshold, y = .estimate, col = .metric)) +
#   geom_line()
# 
# th_optim <- lr_metrics %>%
#   filter(.metric == "distance") %>%
#   filter(.estimate == min(.estimate)) %>%
#   pull(.threshold)
# 
# # produce predictions based on optimum threshold
# lr_preds_optim <- fit$.predictions[[1]] %>%
#   mutate(.pred_class = factor(ifelse(.pred_yes >= th_optim, "yes", "no"), levels = c("no", "yes")))
# 
# lr_preds_optim %>%
#   conf_mat(truth = mort_30, estimate = .pred_class) %>%
#   autoplot()

# fit on all data

full_fit <- fit(wf, mod_df)

# extract_fit_engine(full_fit) %>%
#   jtools::export_summs(coefs = c("(Intercept)",
#                                  "4-hour performance" = "perf",
#                                  "Number previous ED attendances" = "n_ed",
#                                  "Number previous ED attendances²" = "n_ed_sq",
#                                  "Number previous emergency admissions" = "n_adm",
#                                  "Number previous emergency admissions²" = "n_adm_sq",
#                                  "Cambridge score" = "cambridge_score",
#                                  "Arrival: 06:00-12:00" = "arrival_time_X06.00.00",
#                                  "Arrival: 12:00-18:00" = "arrival_time_X12.00.00",
#                                  "Arrival: 18:00-24:00" = "arrival_time_X18.00.00"
#   ),
#   error_pos = "right")

# predictions 

full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")

# smr

(smr_plot <- dta_df %>%
  left_join(mortality_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(dta_time = 30*(dta_time %/% 30)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < 30, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(dta_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  group_by(dta_time) %>%
  summarise(n = n(), expected_mort = sum(pred_mort), actual_mort = sum(mort_30), smr = actual_mort/expected_mort) %>%
  filter(n > 50,  dta_time > 0) %>%
  ggplot(aes(x = dta_time, y = smr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  bnssgtheme() +
  labs(y = "SMR", x = "Time waiting for admission"))


ggsave("materials/smr_plot.png",
       smr_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)

(smr_plot_ed <- dta_df %>%
  left_join(mortality_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(ed_time = 30*(ed_time %/% 30)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < 30, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(ed_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  group_by(ed_time) %>%
  summarise(n = n(), expected_mort = sum(pred_mort), actual_mort = sum(mort_30), smr = actual_mort/expected_mort) %>%
  filter(n > 50, ed_time > 0, ed_time < 48*60) %>%
  ggplot(aes(x = ed_time, y = smr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  bnssgtheme() +
  labs(y = "SMR", x = "Time waiting in ED"))


ggsave("materials/smr_plot_2.png",
       smr_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)

smr_mod <- dta_df %>%
  left_join(mortality_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(dta_time = 30*(dta_time %/% 30)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < 30, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(dta_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  group_by(dta_time) %>%
  summarise(expected_mort = sum(pred_mort), actual_mort = sum(mort_30), smr = actual_mort/expected_mort) %>%
  lm(smr ~ dta_time, data = .)

jtools::export_summs(smr_mod)


dta_df %>%
  left_join(mortality_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(dta_time = 30*(dta_time %/% 30)) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < 30, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  select(dta_time, mort_30) %>%
  mutate(pred_mort = full_pred) %>%
  filter(dta_time < 12*60, dta_time > 0) %>%
  mutate(dta_time = cut(dta_time,
                        breaks = c(0, 4*60, 6*60, 8*60, 12*60),
                        labels = c("0-4 hours",
                                   "4-6 hours", 
                                   "6-8 hours", 
                                   "8-12 hours" 
                                   ))) %>%
  group_by(dta_time) %>%
  summarise(n = n(),
            expected_mort_rate = sum(pred_mort)/n,
            actual_mort_rate = sum(mort_30)/n,
            nnh = 1/(actual_mort_rate - expected_mort_rate)) %>%
  show_in_excel()



# D2A pathway

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

pw <- c("Other", "P1", "P2", "P3")
pw_complexity <- map(head(seq_along(pw), -1), ~pw[1:.x]) %>%
  set_names(nm = c("pw_t1", "pw_t2", "pw_t3"))

d2a_mod_df <- dta_df %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(departure_dt > as.Date(min(pathway_df$adm_date))) %>%
  left_join(pathway_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(spec = factor(spec)) %>%
  mutate(bed_type = factor(bed_type)) %>%
  mutate(pathway_target = list(map_lgl(pw_complexity, ~!pathway %in% .x))) %>% 
  mutate(pathway_target = map(pathway_target, ~factor(ifelse(.x, "yes", "no"), levels = c("no", "yes")))) %>%
  unnest_wider(pathway_target) %>%
  # # mutate(pathway = ifelse(pathway == "Other", FALSE, TRUE)) %>%
  # # mutate(pathway = ifelse(pathway %in% c("Other", "P1", "P2"), FALSE, TRUE)) %>%
  # mutate(pathway = coalesce(pathway, FALSE)) %>%
  # mutate(pathway = factor(ifelse(pathway, "yes", "no"), levels = c("no", "yes"))) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
  select(
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    age,
    age_sq,
    n_ed,
    n_ed_sq,
    n_adm,
    n_adm_sq,
    sex,
    imd,
    cambridge_score,
    cambridge_score_sq,
    spec,
    bed_type,
    pw_t1,
    pw_t2,
    pw_t3
  ) %>%
  distinct() %>%
  na.omit()


targets <- names(pw_complexity)
predictors <- setdiff(names(d2a_mod_df), targets)

fmlas <- map(targets, ~as.formula(paste0(.x, " ~ ", paste0(predictors, collapse = " + "))))

build_model <- function(fmla, dat) {
  glm_spec <- logistic_reg()
  rec <- recipe(fmla, data = dat) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv()
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(glm_spec)
  fit(wf, dat)
}

build_model_valid <- function(fmla, dat) {
  split <- initial_split(dat)
  glm_spec <- logistic_reg()
  rec <- recipe(fmla, data = training(split)) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv()
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(glm_spec)
  last_fit(wf, split)
}

d2a_fits_valid <- map(fmlas, ~build_model_valid(.x , d2a_mod_df))
d2a_fits <- map(fmlas, ~build_model(.x , d2a_mod_df))
d2a_preds <- map(d2a_fits, ~predict(extract_fit_engine(.x),
                                    bake(extract_recipe(.x),
                                         new_data = d2a_mod_df), type = "response"))


# d2a_full_fit <- fit(d2a_wf, d2a_mod_df)
# 
# d2a_full_pred <- predict(extract_fit_engine(d2a_full_fit), bake(prep(d2a_rec), new_data = d2a_mod_df), type = "response")



(d2a_smr_plot <- 
  dta_df %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(pathway_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(dta_time = 60*(dta_time %/% 60)) %>%
  # mutate(pathway = ifelse(pathway == "Other", FALSE, TRUE)) %>%
  # mutate(pathway = ifelse(pathway %in% c("Other", "P1", "P2"), FALSE, TRUE)) %>%
  # mutate(pathway = coalesce(pathway, FALSE)) %>%
  mutate(pathway_target = list(map_lgl(pw_complexity, ~!pathway %in% .x))) %>% 
  unnest_wider(pathway_target) %>%
  ungroup() %>%
  na.omit() %>%
  select(dta_time, any_of(names(pw_complexity))) %>%
  # mutate(pred_pathway = d2a_full_pred) %>% 
  bind_cols(d2a_preds %>% set_names(paste0("pred_", names(pw_complexity))) %>% as_tibble()) %>%
  group_by(dta_time) %>%
  summarise(across(starts_with("pred_"), sum, .names = "expected_{.col}"),
            across(starts_with("pw_"), sum, .names = "actual_{.col}")) %>%
  pivot_longer(cols = -dta_time, names_to = "metric", values_to = "value") %>%
  separate_wider_delim(metric, "_", names = c("cat", "metric"), too_many = "merge") %>%
  mutate(metric = str_remove_all(metric, "pred_")) %>%
  pivot_wider(names_from = cat, values_from = value) %>%
  mutate(ratio = actual/expected) %>%
  mutate(metric = recode(metric, 
                         "pw_t1" = "Discharge pathway complexity tier 1",
                         "pw_t2" = "Discharge pathway complexity tier 2",
                         "pw_t3" = "Discharge pathway complexity tier 3"
                         )) %>%
    
  # summarise(expected_pathway = sum(pred_pathway),
  #           actual_pathway = sum(pathway),
  #           smr = actual_pathway/expected_pathway) %>%
  filter(dta_time < 12*60, dta_time > 0) %>%
  ggplot(aes(x = dta_time, y = ratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(metric), ncol = 1) +
  bnssgtheme() +
  labs(y = "Actual:expected discharge pathway requirement", x = "Time waiting for admission")
)

ggsave("materials/d2a_smr_plot.png",
       d2a_smr_plot,
       height = 9,
       width = 7.5, 
       scale = 0.9)

(d2a_smr_plot <- 
  dta_df %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(pathway_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(ed_time = 60*(ed_time %/% 60)) %>%
  # mutate(pathway = ifelse(pathway == "Other", FALSE, TRUE)) %>%
  # mutate(pathway = ifelse(pathway %in% c("Other", "P1", "P2"), FALSE, TRUE)) %>%
  # mutate(pathway = coalesce(pathway, FALSE)) %>%
  mutate(pathway_target = list(map_lgl(pw_complexity, ~!pathway %in% .x))) %>% 
  unnest_wider(pathway_target) %>%
  ungroup() %>%
  na.omit() %>%
  select(ed_time, any_of(names(pw_complexity))) %>%
  # mutate(pred_pathway = d2a_full_pred) %>% 
  bind_cols(d2a_preds %>% set_names(paste0("pred_", names(pw_complexity))) %>% as_tibble()) %>%
  group_by(ed_time) %>%
  summarise(across(starts_with("pred_"), sum, .names = "expected_{.col}"),
            across(starts_with("pw_"), sum, .names = "actual_{.col}")) %>%
  pivot_longer(cols = -ed_time, names_to = "metric", values_to = "value") %>%
  separate_wider_delim(metric, "_", names = c("cat", "metric"), too_many = "merge") %>%
  mutate(metric = str_remove_all(metric, "pred_")) %>%
  pivot_wider(names_from = cat, values_from = value) %>%
  mutate(ratio = actual/expected) %>%
  mutate(metric = recode(metric, 
                         "pw_t1" = "Discharge pathway complexity tier 1",
                         "pw_t2" = "Discharge pathway complexity tier 2",
                         "pw_t3" = "Discharge pathway complexity tier 3"
                         )) %>%
    
  # summarise(expected_pathway = sum(pred_pathway),
  #           actual_pathway = sum(pathway),
  #           smr = actual_pathway/expected_pathway) %>%
  filter(ed_time < 16*60, ed_time > 0) %>%
  # filter out only tier 1 (as other tiers dont show anything interesting)
  filter(metric == "Discharge pathway complexity tier 1") %>%
  ggplot(aes(x = ed_time/60, y = ratio)) +
  geom_smooth(method = "lm",  col = bnssgcols[4]) +
  geom_point(col = bnssgcols[4]) +
  # facet_wrap(vars(metric), ncol = 1) +
  bnssgtheme() +
  labs(y = "Actual:expected discharge\npathway requirement", x = "Time waiting in ED (hour)")
)

ggsave("materials/d2a_smr_plot.png",
       d2a_smr_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)


show_in_excel <- function(.data) {
  tmp <- paste0(tempfile(), ".csv")
  write.csv(.data, tmp)
  fs::file_show(path = tmp)
}


# NNH d2a

dta_df %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(pathway_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(ed_time = 60*(ed_time %/% 60)) %>%
  mutate(pathway_target = list(map_lgl(pw_complexity, ~!pathway %in% .x))) %>% 
  unnest_wider(pathway_target) %>%
  ungroup() %>%
  na.omit() %>%
  select(ed_time, pw_t1) %>%
  mutate(pred_d2a = d2a_preds[[1]]) %>%
  filter(ed_time < 12*60, ed_time > 0) %>%
  mutate(ed_time = cut(ed_time,
                        breaks = 60*c(0, 4, 6, 10, 12),
                        labels = c("0-4 hours",
                                   "4-6 hours",
                                   "6-10 hours",
                                   "10-12 hours"
                        ))) %>%
  group_by(ed_time) %>%
  summarise(n = n(),
            expected_d2a_rate = sum(pred_d2a)/n,
            actual_d2a_rate = sum(pw_t1)/n,
            nnh = 1/(actual_d2a_rate - expected_d2a_rate)) %>%
  show_in_excel()


# LOS

los_df <- local({
  nctr_df <-
    RODBC::sqlQuery(
      con,
      "SELECT
       [Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
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
      ,[Current_Delay_Code]
      ,[Current_Covid_Status]
      ,[Planned_Date_Of_Discharge]
      ,[Date_Toc_Form_Completed]
      ,[Toc_Form_Status]
      ,[Discharge_Pathway]
      ,[DER_File_Name]
      ,[DER_Load_Timestamp]
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI"
    )
  
  max_census <- max(nctr_df$Census_Date)
  
  date_co <- as.Date(ymd(max_census) - lubridate::dmonths(6))
  
  los_df <- nctr_df %>%
    ungroup() %>%
    mutate(Census_Date = ymd(Census_Date)) %>%
    filter(Census_Date > date_co, Date_Of_Admission > date_co) %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
           sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>% 
    group_by(nhs_number, Date_Of_Admission) %>%
    mutate(spell_id = cur_group_id()) %>%
    group_by(spell_id) %>%
    group_by(nhs_number, adm_date = Date_Of_Admission) %>%
    filter(Current_LOS == max(Current_LOS)) %>%
    mutate(day_of_admission = weekdays(Date_Of_Admission)) %>%
    ungroup() %>%
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                              Organisation_Site_Code == 'RA701' ~ 'bri',
                                              Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                              TRUE ~ '')) %>%
    mutate(Organisation_Site_Code = factor(Organisation_Site_Code)) %>%
    dplyr::select(
      nhs_number,
      adm_date,
      los = Current_LOS
    )
  })


los_mod_df <- dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(los_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(los = factor(ifelse(los > 14, "yes", "no"))) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
  filter(between(dta_time, 0, 2000)) %>%
  select(arrival_time, arrival_month, arrival_year, dta_time, perf, age, age_sq, n_ed, n_ed_sq, n_adm, n_adm_sq, sex, imd, cambridge_score, cambridge_score_sq, los) %>%
  distinct()


build_model_valid(los ~ ., los_mod_df) %>%
  pluck(".metrics")


(los_plot <- dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(los_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
  select(arrival_time, arrival_month, arrival_year, dta_time, perf, age, age_sq, n_ed, n_ed_sq, n_adm, n_adm_sq, sex, imd, cambridge_score, cambridge_score_sq, los) %>%
  distinct() %>%
  filter(between(dta_time, 0, 60*24)) %>%
  filter(los < 75) %>%
  ggplot(aes(x = dta_time/60, y = los)) +
  # geom_boxplot(aes(group = cut_width(dta_time, 60))) +
  geom_boxplot(aes(group = cut_interval(dta_time, 24)), position= position_nudge(x=.5)) +
  bnssgtheme() +
  labs(x = "Time waiting for admission, hours",
       y = "Medical LOS, days"))


ggsave("materials/los_plot.png",
       los_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)

(los_plot <- local({
  plot_df <- dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(los_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
    select(
      arrival_time,
      arrival_month,
      arrival_year,
      dta_time,
      perf,
      age,
      age_sq,
      n_ed,
      n_ed_sq,
      n_adm,
      n_adm_sq,
      sex,
      imd,
      cambridge_score,
      cambridge_score_sq,
      los
    ) %>%
    distinct() %>%
  filter(between(dta_time, 0, 60*24)) %>%
  filter(los < 75) 
  
  plot_df %>%
  mutate(dta_time = 30*dta_time%/%30) %>%
  group_by(dta_time) %>%
  summarise(los = quantile(los, 0.9)) %>%
  ggplot(aes(x = dta_time/60, y = los, col = "90% quantile")) +
  geom_point(data = plot_df, aes(col = "All data"),  alpha = 0.015) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", show.legend = FALSE) +
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

ggsave("materials/downstream_los.png",
       los_plot,
       height = 5,
       width = 7.5, 
       scale = 0.9)  


los_mod_df <- dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(los_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
  select(
    arrival_time,
    arrival_month,
    arrival_year,
    dta_time,
    perf,
    age,
    age_sq,
    n_ed,
    n_ed_sq,
    n_adm,
    n_adm_sq,
    sex,
    imd,
    cambridge_score,
    cambridge_score_sq,
    los
  ) %>%
  distinct() %>%
  filter(between(dta_time, 0, 60*24)) %>%
  filter(los < 75)  %>%
  mutate(dta_time = 30*dta_time%/%30) %>%
  group_by(dta_time) %>%
  summarise(los = quantile(los, 0.9))


los_mod <- lm(los ~ dta_time, data = los_mod_df)
summary(los_mod)

dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(los_df) %>%
  mutate(diff = abs(departure_dt - adm_date)/ddays(1)) %>%
  group_by(nhs_number) %>%
  # find nearest adm match, and reject any over 1 day
  filter(diff == min(diff), diff < 1) %>%
  mutate(age_sq = age^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(cambridge_score_sq = cambridge_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_month = factor(month(arrival_dt))) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  ungroup() %>%
  select(arrival_time,
         arrival_month,
         arrival_year,
         dta_time,
         perf,
         age,
         # age_sq,
         n_ed,
         # n_ed_sq,
         n_adm,
         # n_adm_sq,
         sex,
         imd,
         cambridge_score,
         # cambridge_score_sq,
         los) %>%
  distinct() %>%
  lm(los ~ dta_time, data = .) %>%
  summary()
