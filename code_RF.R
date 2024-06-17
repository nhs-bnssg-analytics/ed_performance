require(tidyverse)
require(RODBC)
require(lubridate)
library(tidymodels)

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"

report_start <- '2022-02-01'

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
    mutate(dt=floor_date(departure_dt,"hour")) %>%
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
    group_by(site = Site_Name, ed_category, dt = ceiling_date(timestamp, "hour")) %>%
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
    group_by(site = Site_Name, dt = ceiling_date(dt, "hour"), ed_category) %>%
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


work_df <- pivot_wider(work_df,
                       id_cols = dt,
                       names_from = category,
                       values_from = count,
                       names_glue = "wf_{str_replace_all(str_to_lower(category), ' ', '_')}")


# DTAs /ED occ

dta_df <- local({
  
  report_end <- today()
  n_hour <- 1
  
  # sequence of time snapshots (i.e. when we measure DTA queue)
  snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                             to = as.POSIXct(lubridate::ymd(report_end)),
                             by = glue::glue("{n_hour} hours"))
  
  # sequence of time intervals (i.e. over which we measure mean handover delay)
  snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
                                                         .x + dminutes((60*n_hour)/2)))
  
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
    mutate(dta_int = interval(dta_dt, departure_dt)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
  # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  
  dat_dtas <- dat_ecds %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(dtas = list(map2_dbl(snapshot_seq, data, ~ sum(
      .x %within% na.omit(.y$dta_int)
    )))) %>%
    mutate(dta_df = map(dtas, function(n_dta) map2_df(n_dta, snapshot_seq, ~tibble(dt = .y, n_dta = .x))))  %>%
    select(site, dta_df) %>%
    unnest(dta_df)
  dat_dtas
})

# saveRDS(dta_df, "data/n_dta_df.RDS")
dta_df <- readRDS("data/n_dta_df.RDS")

dta_df <- dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL")


ed_df <- local({
  
  report_end <- today()
  n_hour <- 1
  
  # sequence of time snapshots (i.e. when we measure DTA queue)
  snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                             to = as.POSIXct(lubridate::ymd(report_end)),
                             by = glue::glue("{n_hour} hours"))
  
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
    mutate(ed_int = interval(arrival_dt, departure_dt)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
  # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  
  dat_ed <- dat_ecds %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(ed_n = list(map2_dbl(snapshot_seq, data, ~ sum(
      .x %within% na.omit(.y$ed_int)
    )))) %>%
    mutate(ed_df = map(ed_n, function(n_ed) map2_df(n_ed, snapshot_seq, ~tibble(dt = .y, n_ed = .x))))  %>%
    select(site, ed_df) %>%
    unnest(ed_df)
  dat_ed
})

saveRDS(ed_df, "data/n_ed_df.RDS")
ed_df <- readRDS("data/n_ed_df.RDS")

ed_df <- ed_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  left_join(dta_df) %>%
  mutate(n_ed = n_ed - n_dta)
  

mod_df <- perf_df %>%
  filter(n >= 5) %>%
  mutate(date = as.Date(dt)) %>%
  left_join(amb_conv_df) %>%
  left_join(walk_ins_df) %>%
  left_join(work_df) %>%
  left_join(ed_df) %>%
  left_join(bed_occ_df) %>%
  na.omit() %>%
  select(-site, -date, -ed_category, -n) %>%
  timetk::tk_augment_lags(c(n_conv, walk_ins, `wf_majors_&_resus`, wf_edou, wf_minors, n_dta, n_ed), .lags = 1:24) %>%
  na.omit()


split <- initial_split(mod_df)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")


spec <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger")

rec <- recipe(perf_all ~ ., data = training(split))

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(spec)

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tune_spec)

folds <- vfold_cv(training(split))
doParallel::registerDoParallel()

tune_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = 5
)

fit <- last_fit(wf, split)

fit$.metrics

fit$.predictions[[1]] %>% ggplot(aes(x = .pred, y = perf_all)) + geom_point()