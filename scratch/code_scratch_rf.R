require(tidyverse)
require(RODBC)
require(lubridate)
library(tidymodels)
library(iml)

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"

report_start <- '2022-01-01'

n_hour <- 4

cached_data <- TRUE

con<-odbcDriverConnect(con_string)

perf_df <- local({
  string_ecds<-glue::glue("select Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Departure_Date,
                        Departure_Time,
                        Site_Name,
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date >= '{report_start}'
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  
  dat_ecds<-dat_ecds_raw %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
    rowwise() %>%
    mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dt=floor_date(departure_dt, glue::glue("{n_hour} hour"))) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # - 4h performance for all ED departures in day
  # - 4h performance for all ED departures in day, for admitted
  # - 4h performance for all ED departures in day, for non-admitted
  
  dat_perf<-dat_ecds %>%
    mutate(time_arr_to_initial=difftime(initial_dt,arrival_dt,units="mins")) %>%
    mutate(time_initial_to_treatment=difftime(treatment_dt,initial_dt,units="mins")) %>%
    mutate(time_arr_to_dep=difftime(departure_dt,arrival_dt,units="mins")) %>%
    mutate(admitted=case_when(is.na(dta_dt)==TRUE ~"NO",.default="YES")) %>%
    rbind(mutate(., ed_category = "all")) %>%
    group_by(site = Site_Name, dt, ed_category) %>%
    summarise(n = n(), perf_all=sum(time_arr_to_dep<4*60)/n()) %>%
    ungroup()
  
  #,
  #perf_admitted=sum(time_arr_to_dep[admitted=="YES"]<4*60)/sum(admitted=="YES"),
  #perf_notadmitted=sum(time_arr_to_dep[admitted=="NO"]<4*60)/sum(admitted=="NO"),
  #mean_time_arr_to_initial=as.numeric(mean(time_arr_to_initial,na.rm=TRUE)),
  #mean_time_initial_to_treatment=as.numeric(mean(time_initial_to_treatment,na.rm=TRUE)))
  
  dat_perf
})

perf_df <- perf_df %>%
  filter(ed_category == "all", site == "SOUTHMEAD HOSPITAL")

amb_conv_df <- local({
  string_actv_swafst <- glue::glue(
    "SELECT DISTINCT
    [ID]
    ,[Derived_Pseudo_NHSNumber]
    ,[Incident_Number]
    ,[Time_Of_Incident]
    ,[CCG]
    ,[Post_Code_Sector]
    ,[Symptom_Group_Chief_Complaint]
    ,[Pathways_AMPDS_Disposition]
    ,[Managed_Incident]
    ,[Incident_Colour]
    ,[Incident_Colour_Split]
    ,[Incident_Colour_Split_Full]
    ,[Response_Target_1_Clock Start]
    ,[Response_Target_2_Clock Start]
    ,[Response_Target_1_Clock Stop]
    ,[Response_Target_2_Clock Stop]
    ,[Outcome]
    ,[Conveyed_To_Ward_Type]
   ,b.[OrganisationName] as 'Site_Name'
    ,[First_Hospital_Attended]
    ,[First_Ward_Attended]
    ,[Time_of_Handover_at_First_Hospital_Attended]
    ,[First_MIU_Attended]
    ,[CCG_of_First_MIU_Attended]
    ,[Number_of_Patients_Carried]
    ,[Contract_Activity]
    ,[Month_of_Call]
    ,[Age_Bracket]
    ,[Residential_or_Nursing_home_Location]
    ,[Residential_or_Nursing_home_Location_Name]
    ,[Authorising_Doctor_Practice]
    ,[Authorising_Hospital]
    ,[Booked_Pickup_Hospital]
    ,[Booked_Pickup_Ward]
    ,[Booked_Destination_Hospital]
    ,[Booked_Destination_Ward]
    ,[Local_Authority]
    ,[Ward]
    ,[T0_Time_of_Call]
    ,[T1_Time_Call_answered]
    ,[Time_First_Core_Resource_Allocated]
    ,[Time_Last_Core_Resource_Cleared]
    ,[Mental_Health_Call_Indicator]
    ,[Suicide_Call_Indicator]
    ,[Provisional_Diagnosis]
    ,[High_Intensity_User_Call_Indicator]
    ,[SourceFileID]
    ,c.[Time_Resource_Arrived_at_Receiving_Location]
    ,c.[Time_of_Patient_Handover]
    ,c.[Time_Clear]
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
LEFT JOIN [ABI].[Lard].[vw_Organisation_Lookup] b on a.[First_Hospital_Attended]=b.[OrganisationCode]
LEFT JOIN [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_SWAST_AR119] c on a.Incident_Number=c.Call_Number
where Outcome = 'See & Convey' AND
b.[OrganisationName] in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') AND
Conveyed_To_Ward_Type = 'A&E' AND
  (CONVERT(DATE, [Time_Of_Incident]) > '{report_start}' OR
  CONVERT(DATE, [T0_Time_of_Call]) > '{report_start}' OR
  CONVERT(DATE, [T1_Time_Call_answered]) > '{report_start}' OR
  CONVERT(DATE, [Time_First_Core_Resource_Allocated]) > '{report_start}' OR
  CONVERT(DATE, [Time_Last_Core_Resource_Cleared]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Stop]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Stop]) > '{report_start}')"
  )
  
  actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst)
  
  
  event_log_swafst <- actv_swafst %>%
    dplyr::rename(
      "Time_of_Call_response_start" = "Response_Target_1_Clock Start",
      "Time_of_Response_Target_2_Clock Start" = "Response_Target_2_Clock Start",
      "Time_of_Ambulance_arrived_on_scene" = "Response_Target_1_Clock Stop",
      "Time_of_Response_Target_2_Clock Stop" = "Response_Target_2_Clock Stop",
      "Time_Ambulance_arrived_at_hospital" = "Time_Resource_Arrived_at_Receiving_Location",
      "Time_of_Patient_Handed_over" = "Time_of_Patient_Handover",
    ) %>%
    # dplyr::left_join(sites, by = c("First_Hospital_Attended" = "site_code")) %>%
    dplyr::filter(!is.na(Derived_Pseudo_NHSNumber)) %>%
    dplyr::filter(Site_Name %in% c('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')) %>%
    dplyr::filter(First_Ward_Attended == "ED") %>%
    dplyr::filter(!dplyr::if_all(dplyr::matches("Time_"), ~ is.na(.x))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Derived_Pseudo_NHSNumber = as.character(Derived_Pseudo_NHSNumber),
      across(dplyr::matches("Time_"), as.character)
    ) %>%
    mutate(ed_category = dplyr::case_when(Incident_Colour %in% c(
      "Category 1",
      "Category 2"
    ) ~ "major",
    Incident_Colour %in% c(
      "Category 3",
      "Category 4",
      "Category 5") ~ "minor",
    is.na(Incident_Colour) ~ NA_character_)) %>%
    tidyr::pivot_longer(cols = dplyr::matches("Time_"),
                        names_pattern = '^(?:T[10]_)?Time_(?:of|Of)?_?(.*)$',
                        names_to = c("activity_id"),
                        values_to = "timestamp"#,
                        #names_transform = ~str_to_lower(str_replace(.x, "_Time", ""))
    ) %>%
    dplyr::filter(
      !is.na(timestamp),
      !stringr::str_replace_all(activity_id, "_", " ") %in% c(
        "Call",
        "Call response start",
        "Response target 2 clock start",
        "Response target 2 clock stop",
        "First core resource allocated",
        "Last core resource cleared",
        "Handover at first hospital attended",
        "Incident",
        "Resource arrived at geofence at receiving location",
        "Clear"
      )) %>%
    dplyr::mutate(timestamp = lubridate::parse_date_time(timestamp, orders = c("ymd_HMS", "dmy_HMS")),
                  #activity_id = if_else(activity_id %in% c("Arrival", "Initial_Assessment"), glue::glue("{activity_id}: {arrival_type}"), activity_id),
                  activity_id = stringr::str_replace_all(activity_id, "_", " "),
                  activity_id = stringr::str_to_sentence(activity_id),
                  resource_id = "swasft") %>%
    group_by(Derived_Pseudo_NHSNumber) %>%
    arrange(timestamp) %>%
    mutate(last_daily_event = lubridate::day(timestamp) < lead(lubridate::day(timestamp), 1, 0),
           timestamp = dplyr::if_else(last_daily_event, lubridate::ceiling_date(timestamp, unit = "day") + lubridate::dseconds(1), timestamp)) %>%
    dplyr::group_by(Derived_Pseudo_NHSNumber) %>%
    dplyr::select(Site_Name, NHS_Number = Derived_Pseudo_NHSNumber, activity_id, timestamp, resource_id, ed_category)
  
  event_log_swafst %>%
    filter(activity_id == "Ambulance arrived at hospital") %>%
    rbind(mutate(., ed_category = "all")) %>%
    group_by(site = Site_Name, ed_category, dt = ceiling_date(timestamp, glue::glue("{n_hour} hour"))) %>%
    count(name = "n_conv") %>%
    na.omit() %>%
    ungroup()
})

amb_conv_df <- amb_conv_df %>%
  filter(ed_category == "all", site == "SOUTHMEAD HOSPITAL")


bed_occ_df <- local({
  RODBC::sqlQuery(con, glue::glue("
        SELECT * FROM
        [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_UrgentCare_Daily]
        WHERE METRIC_ID IN (347351, 346203, 346117)
        AND report_date > '{report_start}'
                      ")) %>%
    mutate(site = case_when(Provider == 'NBT' ~ 'SOUTHMEAD HOSPITAL',
                            Provider == 'BRI' ~ 'BRISTOL ROYAL INFIRMARY',
                            Provider == 'WGH' ~ 'WESTON GENERAL HOSPITAL'),
           date = ymd(Report_Date)) %>%
    dplyr::select(site, date, bed_occ = Value)
})

bed_occ_df <- bed_occ_df %>%
  filter(site == "SOUTHMEAD HOSPITAL")


walk_ins_df <- local({
  string_ecds<-glue::glue("select NHS_Number,
                    Arrival_Date,
                    Arrival_Time,
                    Site_Name,
                    Arrival_Mode_Desc,
                    Decision_To_Admit_Date,
                    Decision_To_Admit_Time,
                    Departure_Date,Departure_Time,
                    Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Arrival_Date >= '{report_start}'
      and Arrival_Mode_Desc not like '%ambulance%'
      and Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  
  dat_ecds_raw %>%
    mutate(dt=parse_date_time(glue::glue("{Arrival_Date} {Arrival_Time}"), orders = "ymd_HMS")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    rbind(mutate(., ed_category = "all")) %>%
    group_by(site = Site_Name, dt = ceiling_date(dt, glue::glue("{n_hour} hour")), ed_category) %>%
    count(name = "walk_ins") %>%
    ungroup()
  
})

walk_ins_df <- walk_ins_df %>%
  filter(ed_category == "all", site == "SOUTHMEAD HOSPITAL")


# workforce


work_df <- local <- ({
  maj_resus_staff_df <- read_csv("data/nbt_maj_resus_staff.csv") %>%
    pivot_longer(cols = -c(`Majors & Resus`), 
                 names_to = "date",
                 values_to = "count") %>%
    mutate(dt = parse_date_time(glue::glue("{date} {`Majors & Resus`}"), orders = "dmy HMS"),
           dt = `second<-`(dt, 0),
           category = "Majors & Resus") %>%
    select(dt, category, count) %>%
    replace_na(list(count = 0)) 
  
  edou_staff_df <- read_csv("data/nbt_edou_staff.csv") %>%
    pivot_longer(cols = -c(`EDOU`), 
                 names_to = "date",
                 values_to = "count") %>%
    mutate(dt = parse_date_time(glue::glue("{date} {`EDOU`}"), orders = "dmy HMS"),
           dt = `second<-`(dt, 0),
           category = "EDOU") %>%
    select(dt, category, count) %>%
    replace_na(list(count = 0)) 
  
  min_staff_df <- read_csv("data/nbt_min_staff.csv") %>%
    pivot_longer(cols = -c(`Minors`), 
                 names_to = "date",
                 values_to = "count") %>%
    mutate(dt = parse_date_time(glue::glue("{date} {`Minors`}"), orders = "dmy HMS"),
           dt = `second<-`(dt, 0),
           category = "Minors") %>%
    select(dt, category, count)%>%
    replace_na(list(count = 0))  
  
  
  bind_rows(maj_resus_staff_df, edou_staff_df, min_staff_df)
  
})


work_df <- work_df %>%
  group_by(dt = ceiling_date(dt, glue::glue("{n_hour} hour")), category) %>%
  summarise(count = mean(count)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = dt,
    names_from = category,
    values_from = count,
    names_glue = "wf_{str_replace_all(str_to_lower(category), ' ', '_')}"
  ) %>%
  janitor::clean_names() 


work_df <- work_df %>%
  pivot_longer(cols = -dt) %>%
  group_by(dt) %>%
  summarise(wf = sum(value))


arr_df <- walk_ins_df %>%
  left_join(amb_conv_df) %>%
  select(-site, -ed_category) %>%
  pivot_longer(cols = -dt) %>%
  group_by(dt) %>%
  summarise(arr = sum(value))


mod_df <- perf_df %>%
  filter(n >= 5) %>% 
  left_join(work_df) %>%
  left_join(arr_df) %>%
  mutate(wf_arr_ratio = wf/arr) %>%
  na.omit() %>%
  select(-site, -ed_category, -n, -wf, -arr) %>%
  timetk::tk_augment_lags(c(wf_arr_ratio), .lags = 1:11) %>%
  na.omit() 

date_co <- ymd("2024-03-14")

split <- make_splits(
  x = list(analysis = which(mod_df$dt < date_co),
           assessment = which(mod_df$dt >= date_co)),
  data = mod_df
)

# split <- initial_split(mod_df)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")


# spec <- rand_forest() %>%
#   set_mode("regression") %>%
#   set_engine("ranger")

rec <- recipe(perf_all ~ ., data = training(split))  %>%
  update_role(dt, new_role = "id")

# wf <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(spec)

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tune_spec)

folds <- vfold_cv(training(split), v = 10)

# cl <- parallel::makeCluster(parallel::detectCores() - 2)
# doParallel::registerDoParallel(cl)
# 
# tune_res <- tune_grid(
#   tune_wf,
#   resamples = folds,
#   grid = 10,
#   control = control_grid(parallel_over = "everything")
# )
# 
# saveRDS(tune_res, "data/tune_res.RDS")
# tune_res <- readRDS("data/tune_res.RDS")
# 
# tune_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   select(mean, min_n, mtry) %>%
#   pivot_longer(min_n:mtry,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "RMSE")
# 

rf_grid <- grid_regular(
  trees(range = c(500, 1000)),
  mtry(range = c(2, 10)),
  min_n(range = c(100, 250)),
  levels = 5
)

cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)

reg_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid,
  control = control_grid(parallel_over = "everything")
)

saveRDS(reg_res, "data/reg_res.RDS")
reg_res <- readRDS("data/reg_res.RDS")

reg_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  facet_wrap(vars(trees)) +
  labs(y = "RMSE")

best_rmse <- select_best(reg_res, "rmse")

final_spec <- finalize_model(tune_spec, best_rmse) %>%
  set_engine("ranger", importance = "permutation")

# final_spec <- rand_forest(
#   mtry = 1,
#   trees = 1000,
#   min_n = 250) %>%
#   set_mode("regression") %>%
#   set_engine("ranger", importance = "permutation")

final_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_spec)

fit <- last_fit(final_wf, split)

fit$.metrics

fit$.predictions[[1]] %>% ggplot(aes(x = .pred, y = perf_all)) + geom_point()

fit %>% extract_fit_engine() %>% vip::vip(geom = "point")

# ranger fit

# x <- fit$.workflow[[1]] %>% extract_recipe() %>% bake(mod_df)
# 
# rf <- ranger(perf_all ~ ., x, mtry = 150, min.node.size = 10)
# 
# x_dat <- x[which(names(x) != "perf_all")]
# predictor <- Predictor$new(fit %>% extract_fit_parsnip(), data = x_dat, y = x$perf_all)
# 
# ale <- FeatureEffect$new(predictor, feature = "walk_ins", grid.size = 10)
# ale$plot()
# 


pred_df <- fit %>%
  extract_fit_engine() %>%
  predict(data = testing(split), predict.all = TRUE) %>%
  pluck("predictions") %>%
  list(pred_mean = rowMeans(.),
       pred_u95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.975),
       pred_l95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.025)
  ) %>%
  `[`(-1) %>%
  as_tibble()


pred_df %>%
  bind_cols(dplyr::select(testing(split), dt, perf_all)) %>%
  mutate(across(c(pred_mean, pred_u95, pred_l95), ~.x - perf_all)) %>%
  ggplot(aes(x = dt, y = pred_mean)) +
  geom_ribbon(aes(ymax = pred_u95, ymin = pred_l95), alpha = 0.5) + 
  geom_line() 

scen_df <- fit %>%
  extract_fit_engine() %>%
  predict(data = mutate(testing(split), across(contains("wf"), ~.x*1.1)), predict.all = TRUE) %>%
  pluck("predictions") %>%
  list(scen_mean = rowMeans(.),
       scen_u95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.975),
       scen_l95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.025)
  ) %>%
  `[`(-1) %>%
  as_tibble()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  ggplot(aes(x = dt, y = scen_mean)) +
  geom_ribbon(aes(ymax = scen_u95, ymin = scen_l95), alpha = 0.5) + 
  geom_line()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  ggplot(aes(y = scen_mean)) +
  # geom_ribbon(aes(ymax = scen_u95, ymin = scen_l95), alpha = 0.5) + 
  geom_boxplot()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  pull(scen_mean) %>%
  median() * 100


best_fit <- fit(final_wf, mod_df)



features <- mod_df %>% 
  bake(prep(rec), .) %>%
  select(-perf_all, -dt)

target <- mod_df %>%
  select(perf_all)

predict_wrapper <- function(model, newdata){
  workflows:::predict.workflow(object = model, new_data = newdata)
}

predictor <- Predictor$new(
  model = best_fit,
  data = features, 
  y = target,
  predict.fun = predict_wrapper
)

# imp <- FeatureImp$new(predictor, loss = "ce")

# ale <- FeatureEffect$new(predictor, feature = "n_adm", grid.size = 10)
# ale$plot()



ale <- FeatureEffects$new(predictor, features = str_subset(names(features), "wf_mino"), grid.size = 10)
ale$plot()

# ale <- FeatureEffects$new(predictor, features = str_subset(names(features), "n_dta"), grid.size = 10)
# ale$plot()

# modelling with deasonalised data

library(feasts)

mod_ds_df <- mod_df %>%
  pivot_longer(-dt) %>%
  as_tsibble(index = dt, key = c(name)) %>%
  tsibble::fill_gaps() %>%
  tidyr::fill(value, .direction = "down") %>%
  model(STL(value)) %>%
  components() %>%  
  dplyr::select(dt, name, remainder) %>%
  as_tibble() %>%
  pivot_wider(names_from = "name", values_from = "remainder")



mod_ds_df <- mod_ds_df %>%
  # select(-contains(c("walk"))) %>%
  dplyr::select(-contains(c("n_dta", "n_ed", "n_adm", "hour", "month", "year", "weekday", "bank_holiday")))


date_co <- ymd("2024-02-14")


split <- make_splits(
  x = list(analysis = which(mod_ds_df$dt < date_co),
           assessment = which(mod_ds_df$dt >= date_co)),
  data = mod_ds_df
)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")


rec <- recipe(perf_all ~ ., data = training(split))  %>%
  update_role(dt, new_role = "id")

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tune_spec)

folds <- vfold_cv(training(split), v = 10)

rf_grid <- grid_regular(
  mtry(range = c(2, 50)),
  min_n(range = c(50, 250)),
  levels = 10
)

cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)

reg_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid,
  control = control_grid(parallel_over = "everything")
)

saveRDS(reg_res, "data/reg_res.RDS")
reg_res <- readRDS("data/reg_res.RDS")

reg_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

best_rmse <- select_best(reg_res, "rmse")

final_spec <- finalize_model(tune_spec, best_rmse) %>%
  set_engine("ranger", importance = "permutation")

final_spec <- rand_forest(
  mtry = 2,
  trees = 1000,
  min_n = 100) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")

final_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_spec)

fit <- last_fit(final_wf, split)

fit$.metrics

fit$.predictions[[1]] %>% ggplot(aes(x = .pred, y = perf_all)) + geom_point()

fit %>% extract_fit_engine() %>% vip::vip(geom = "point")


pred_df <- fit %>%
  extract_fit_engine() %>%
  predict(data = testing(split), predict.all = TRUE) %>%
  pluck("predictions") %>%
  list(pred_mean = rowMeans(.),
       pred_u95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.975),
       pred_l95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.025)
  ) %>%
  `[`(-1) %>%
  as_tibble()


pred_df %>%
  bind_cols(dplyr::select(testing(split), dt, perf_all)) %>%
  mutate(across(c(pred_mean, pred_u95, pred_l95), ~.x - perf_all)) %>%
  ggplot(aes(x = dt, y = pred_mean)) +
  geom_ribbon(aes(ymax = pred_u95, ymin = pred_l95), alpha = 0.5) + 
  geom_line() 

scen_df <- fit %>%
  extract_fit_engine() %>%
  predict(data = mutate(testing(split), across(contains("wf"), ~.x*1.1)), predict.all = TRUE) %>%
  pluck("predictions") %>%
  list(scen_mean = rowMeans(.),
       scen_u95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.975),
       scen_l95 = array_branch(., margin = 1) %>% map_dbl(quantile, 0.025)
  ) %>%
  `[`(-1) %>%
  as_tibble()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  ggplot(aes(x = dt, y = scen_mean)) +
  geom_ribbon(aes(ymax = scen_u95, ymin = scen_l95), alpha = 0.5) + 
  geom_line()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  ggplot(aes(y = scen_mean)) +
  # geom_ribbon(aes(ymax = scen_u95, ymin = scen_l95), alpha = 0.5) + 
  geom_boxplot()

pred_df %>%
  bind_cols(dplyr::select(testing(split), dt)) %>%
  bind_cols(scen_df) %>%
  mutate(across(c(scen_mean, scen_u95, scen_l95), ~.x - pred_mean)) %>%
  pull(scen_mean) %>%
  mean()

# # DTAs /ED occ
# 
# dta_df <- local({
#   
#   report_end <- today()
#   # n_hour <- 1
#   
#   # sequence of time snapshots (i.e. when we measure DTA queue)
#   snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
#                              to = as.POSIXct(lubridate::ymd(report_end)),
#                              by = glue::glue("{n_hour} hours"))
#   
#   # sequence of time intervals (i.e. over which we measure mean handover delay)
#   snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
#                                                          .x + dminutes((60*n_hour)/2)))
#   
#   string_ecds<-glue::glue("select Arrival_Date,
#                         Arrival_Time,
#                         Initial_Assessment_Date,
#                         Initial_Assessment_Time,
#                         Seen_For_Treatment_Date,
#                         Seen_For_Treatment_Time,
#                         Decision_To_Admit_Date,
#                         Decision_To_Admit_Time,
#                         Departure_Date,
#                         Departure_Time,
#                         Site_Name,
#                         Acuity_Desc
#       FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
#       where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
#       and Arrival_Date >= '{report_start}'
#       ")
#   dat_ecds_raw<-sqlQuery(con,string_ecds)
#   
#   dat_ecds<-dat_ecds_raw %>%
#     mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
#     mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     select(arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
#     rowwise() %>%
#     mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
#     ungroup() %>%
#     mutate(dta_int = interval(dta_dt, departure_dt)) %>%
#     mutate(date=floor_date(departure_dt,"day")) %>%
#     mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
#                                  TRUE~"major"))
#   
#   
#   
#   # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
#   # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
#   
#   dat_dtas <- dat_ecds %>%
#     group_by(site = Site_Name) %>%
#     nest() %>%
#     mutate(dtas = list(map2_dbl(snapshot_seq, data, ~ sum(
#       .x %within% na.omit(.y$dta_int)
#     )))) %>%
#     mutate(dta_df = map(dtas, function(n_dta) map2_df(n_dta, snapshot_seq, ~tibble(dt = .y, n_dta = .x))))  %>%
#     select(site, dta_df) %>%
#     unnest(dta_df)
#   dat_dtas
# })
# 
# saveRDS(dta_df, "data/n_dta_df.RDS")
# dta_df <- readRDS("data/n_dta_df.RDS")
# 
# dta_df <- dta_df %>%
#   filter(site == "SOUTHMEAD HOSPITAL")
# 
# 
# ed_df <- local({
#   
#   report_end <- today()
#   # n_hour <- 1
#   
#   # sequence of time snapshots (i.e. when we measure DTA queue)
#   snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
#                              to = as.POSIXct(lubridate::ymd(report_end)),
#                              by = glue::glue("{n_hour} hours"))
#   
#   string_ecds<-glue::glue("select Arrival_Date,
#                         Arrival_Time,
#                         Initial_Assessment_Date,
#                         Initial_Assessment_Time,
#                         Seen_For_Treatment_Date,
#                         Seen_For_Treatment_Time,
#                         Decision_To_Admit_Date,
#                         Decision_To_Admit_Time,
#                         Departure_Date,
#                         Departure_Time,
#                         Site_Name,
#                         Acuity_Desc
#       FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
#       where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
#       and Arrival_Date >= '{report_start}'
#       ")
#   dat_ecds_raw<-sqlQuery(con,string_ecds)
#   
#   dat_ecds<-dat_ecds_raw %>%
#     mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
#     mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     select(arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
#     rowwise() %>%
#     mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
#     ungroup() %>%
#     mutate(ed_int = interval(arrival_dt, departure_dt)) %>%
#     mutate(date=floor_date(departure_dt,"day")) %>%
#     mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
#                                  TRUE~"major"))
#   
#   
#   
#   # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
#   # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
#   
#   dat_ed <- dat_ecds %>%
#     group_by(site = Site_Name) %>%
#     nest() %>%
#     mutate(ed_n = list(map2_dbl(snapshot_seq, data, ~ sum(
#       .x %within% na.omit(.y$ed_int)
#     )))) %>%
#     mutate(ed_df = map(ed_n, function(n_ed) map2_df(n_ed, snapshot_seq, ~tibble(dt = .y, n_ed = .x))))  %>%
#     select(site, ed_df) %>%
#     unnest(ed_df)
#   dat_ed
# })
# 
# saveRDS(ed_df, "data/n_ed_df.RDS")
# ed_df <- readRDS("data/n_ed_df.RDS")
# 
# ed_df <- ed_df %>%
#   filter(site == "SOUTHMEAD HOSPITAL") %>%
#   left_join(dta_df) %>%
#   mutate(n_ed = n_ed - n_dta)
#   
# 
# # number of ed admits
# 
# adm_df <- local({
#   
#   report_end <- today()
#   # n_hour <- 1
#   
#   # sequence of time snapshots (i.e. when we measure DTA queue)
#   snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
#                              to = as.POSIXct(lubridate::ymd(report_end)),
#                              by = glue::glue("{n_hour} hours"))
#   
#   # sequence of time intervals (i.e. over which we measure mean handover delay)
#   snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
#                                                          .x + dminutes((60*n_hour)/2)))
#   
#   string_ecds<-glue::glue("select Arrival_Date,
#                         Arrival_Time,
#                         Initial_Assessment_Date,
#                         Initial_Assessment_Time,
#                         Seen_For_Treatment_Date,
#                         Seen_For_Treatment_Time,
#                         Decision_To_Admit_Date,
#                         Decision_To_Admit_Time,
#                         Departure_Date,
#                         Departure_Time,
#                         Site_Name,
#                         Acuity_Desc
#       FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
#       where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
#       and Arrival_Date >= '{report_start}'
#       ")
#   dat_ecds_raw<-sqlQuery(con,string_ecds)
#   
#   dat_ecds<-dat_ecds_raw %>%
#     mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
#     mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     select(arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
#     rowwise() %>%
#     mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
#     ungroup() %>%
#     mutate(dta_int = interval(dta_dt, departure_dt)) %>%
#     mutate(date=floor_date(departure_dt,"day")) %>%
#     mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
#                                  TRUE~"major"))
#   
#   
#   
#   # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
#   # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
#   
#   dat_adm <- dat_ecds %>%
#     filter(!is.na(dta_dt)) %>%
#     group_by(site = Site_Name, dt = ceiling_date(departure_dt, glue::glue("{n_hour} hour"))) %>%
#     count(name = "n_adm")
#   dat_adm
# })
# 
# saveRDS(adm_df, "data/adm_df.RDS")
# adm_df <- readRDS("data/adm_df.RDS")
# 
# adm_df <- adm_df %>%
#   filter(site == "SOUTHMEAD HOSPITAL")