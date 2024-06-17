require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)


report_start <- '2023-08-01'
report_end <- '2024-03-01'
n_hour <- 1

# sequence of time snapshots (i.e. when we measure DTA queue)
snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                           to = as.POSIXct(lubridate::ymd(report_end)),
                           by = glue::glue("{n_hour} hours"))

# sequence of time intervals (i.e. over which we measure mean handover delay)
snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
                                                       .x + dminutes((60*n_hour)/2)))

dta_df <- local({
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

dta_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(dt %within% interval(ymd("2024-02-15"), ymd("2024-02-15")+dweeks(4))) %>%
  ggplot(aes(x = dt, y = n_dta)) +
  geom_line() + 
  bnssgtheme() +
  labs(x = "Date", y = "DTA queue") 

ggsave("materials/dta_plot.png",
       last_plot(),
       height = 5,
       width = 7.5, 
       scale = 0.9)

ed_df <- local({
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

handover_df <- local({
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
    ,c.[Handover_Duration__seconds_]
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
  
  actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst) %>%
    mutate(handover_time = lubridate::parse_date_time(Time_of_Patient_Handover, orders = "dmy HMS")) 
  
  
  
  
  # max_dt <- ceiling_date(max(actv_swafst$handover_time, na.rm = TRUE), "4 hours")
  # handover_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  # handover_int <- map(handover_seq, ~lubridate::interval(.x, .x+dhours(n_hour)))
  
  handover_df <- actv_swafst %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(handover_time = list(map2_dbl(snapshot_int, data, ~filter(.y, handover_time %within% .x) %>%
                                         pull(Handover_Duration__seconds_) %>%
                                         median(na.rm = TRUE)))) %>%
    mutate(handover_df = map(handover_time, function(handover_time) map2_df(handover_time, snapshot_seq, ~tibble(dt = .y, handover_time = .x)))) %>%
    select(site, handover_df) %>%
    unnest(handover_df)
  
  handover_df
})

handover_df %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(dt %within% interval(ymd("2024-02-15"), ymd("2024-02-15")+dweeks(4))) %>%
  ggplot(aes(x = dt, y = handover_time/60)) +
  geom_line() + 
  bnssgtheme() +
  labs(x = "Date", y = "Median ambulance handover time\n(mins)") 

ggsave("materials/handover_plot.png",
       last_plot(),
       height = 5,
       width = 7.5, 
       scale = 0.9)



minor_time_in_ed_df <- local({
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
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- ceiling_date(max(dat_ecds$departure_dt, na.rm = TRUE), "4 hours")
  # minor_dep_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  # minor_dep_int <- map(minor_dep_seq, ~lubridate::interval(.x, .x+dhours(n_hour)))
  
  
  dat_minor_dep <- dat_ecds %>%
    filter(ed_category == "minor") %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(minor_time = list(map2_dbl(snapshot_int, data, ~ .y %>%
                                  filter(departure_dt %within% .x) %>%
                                  summarise(time_in_ed = mean((departure_dt - arrival_dt)/dminutes(1))) %>%
                                  pull(time_in_ed))
                                  )) %>%
    mutate(minor_time_df = map(minor_time, function(time_in_ed) map2_df(time_in_ed,
                                                                        snapshot_seq,
                                                                        ~tibble(dt = .y, time_in_ed = .x))))  %>%
    select(site, minor_time_df) %>%
    unnest(minor_time_df)
  dat_minor_dep
})

# amb response time

resp_time_df <- local({
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
   ,b.[OrganisationName] as [Site_Name]
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
    --,c.[Time_Resource_Arrived_at_GeoFence_at_Receiving_Location]
    ,c.[Time_Resource_Arrived_at_Receiving_Location]
    ,c.[Time_of_Patient_Handover]
    ,c.[Time_Clear]
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
LEFT JOIN [ABI].[Lard].[vw_Organisation_Lookup] b on a.[First_Hospital_Attended]=b.[OrganisationCode]
LEFT JOIN [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_SWAST_AR119] c on a.Incident_Number=c.Call_Number
where Outcome = 'See & Convey' AND
OrganisationName in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') AND
Conveyed_To_Ward_Type = 'A&E' AND
  CONVERT(DATE, [Time_Of_Incident]) > '{report_start}' OR
  CONVERT(DATE, [T0_Time_of_Call]) > '{report_start}' OR
  CONVERT(DATE, [T1_Time_Call_answered]) > '{report_start}' OR
  CONVERT(DATE, [Time_First_Core_Resource_Allocated]) > '{report_start}' OR
  CONVERT(DATE, [Time_Last_Core_Resource_Cleared]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Stop]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Stop]) > '{report_start}'"
  )
  
  actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst)
  
  
  
  response_time <- actv_swafst %>%
    mutate(across(matches("Time|Clock"),
                  .f = ~lubridate::parse_date_time(.x, orders = c("ymd_HMS", "dmy_HMS")))) %>%
    filter(Outcome %in% c("See & Treat", "See & Convey"))  %>%
    filter(T0_Time_of_Call >= ymd(report_start),
           T0_Time_of_Call <= ymd(report_end) ) %>%
    group_by(dt = floor_date(T0_Time_of_Call, "hour"),
             call_category = Incident_Colour) %>%
    summarise(avg_response = mean(`Response_Target_1_Clock Stop` - `Response_Target_1_Clock Start`, na.rm = TRUE)/dminutes(1)) %>%
    filter(call_category %in% str_c("Category ", c(1, 2))) %>%
    mutate(call_category = recode(call_category, 
                                  "Category 2"= "cat2",
                                  "Category 1"= "cat1")) %>%
    pivot_wider(names_from = "call_category", values_from = "avg_response", names_prefix = "resp_time_")
  
  response_time
  })


# handover time

(p_ccf_1 <- dta_df %>% 
  left_join(handover_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  as_tsibble(key = site, index = dt) %>%
  na.omit() %>%
  tsibble::fill_gaps() %>%
  CCF(x = n_dta, y = handover_time, , lag_max = 48) %>%
  autoplot() +
  bnssgtheme() + 
  labs(title = "Raw time series",
       x = "Lag of DTA queue [1hr]",
       y = "Cross-correlation with mean handover time"))


(p_ccf_2 <- dta_df %>% 
  left_join(handover_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>% 
  pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
  as_tsibble(index = dt, key = c(site, metric)) %>%
  tsibble::fill_gaps() %>%
  tidyr::fill(value, .direction = "down") %>%
  model(STL(value)) %>%
  components() %>%  
  select(dt, site, metric, remainder) %>%
  # as_tibble() %>%
  pivot_wider(names_from = "metric", values_from = "remainder") %>%
  CCF(x = n_dta, y = handover_time, lag_max = 48) %>%
  autoplot() +
  bnssgtheme() + 
  labs(title = "Time series with trend/seasonality removed",
       x = "Lag of DTA queue [1hr]",
       y = "Cross-correlation with mean handover time"))


ccf_ptc <- patchwork::wrap_plots(p_ccf_1, p_ccf_2, ncol = 1, axis_titles = "collect")

# apply_consistent_y_lims <- function(this_plot){
#   num_plots <- length(this_plot$layers)
#   y_lims <- lapply(1:num_plots, function(x) ggplot_build(this_plot[[x]])$layout$panel_scales_y[[1]]$range$range)
#   min_y <- min(unlist(y_lims))
#   max_y <- max(unlist(y_lims))
#   this_plot & ylim(min_y, max_y)
# }
# 
# apply_consistent_y_lims(ccf_ptc)

ggsave("materials/ccf_plot.png",
       ccf_ptc,
       height = 7.5,
       width = 7.5, 
       scale = 0.9)

# modelling 

(scatter_1 <- dta_df %>% 
  left_join(handover_df) %>% 
  # left_join(minor_time_in_ed_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>% 
  ## this block will remove seasonality/trend
  # pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
  # as_tsibble(index = dt, key = c(site, metric)) %>%
  # tsibble::fill_gaps() %>%
  # tidyr::fill(value, .direction = "down") %>%
  # model(STL(value)) %>%
  # components() %>% 
  # select(dt, site, metric, remainder) %>%
  # # as_tibble() %>%
  # pivot_wider(names_from = "metric", values_from = "remainder") %>%
  ggplot(aes(x = n_dta, y = handover_time/60)) +
  geom_point() +
  # geom_boxplot(aes(group = cut_interval(n_dta, 9)), position= position_nudge(x=2.5)) +
  geom_smooth(method = "lm") +
  bnssgtheme() +
  labs(y = "Median ambulance handover time\n(min)", x = "DTA queue")
  )


ggsave("materials/dta_handover_scatter.png",
       scatter_1,
       height = 5,
       width = 7.5, 
       scale = 0.9)


(scatter_2 <- dta_df %>% 
  left_join(handover_df) %>% 
  # left_join(minor_time_in_ed_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>% 
  # this block will remove seasonality/trend
  pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
  as_tsibble(index = dt, key = c(site, metric)) %>%
  tsibble::fill_gaps() %>%
  tidyr::fill(value, .direction = "down") %>%
  model(STL(value)) %>%
  components() %>%
  select(dt, site, metric, remainder) %>%
  # as_tibble() %>%
  pivot_wider(names_from = "metric", values_from = "remainder") %>%
  ggplot(aes(x = n_dta, y = handover_time/60)) +
  geom_point() +
  geom_smooth(method = "lm") +
  bnssgtheme() +
  labs(y = "Mean handover time (min)", x = "DTA queue")
  )

model_spec <- linear_reg() %>%
  set_engine("lm")

dta_mod <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  left_join(resp_time_df) %>%
  mutate(n_ed = n_ed - n_dta) %>%
  ungroup() %>%
  na.omit() %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(handover_time > 0) %>%
  mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "yes", "no")),
         hour = factor(cut(hour(dt), breaks = seq(0, 24, 6), include.lowest = TRUE), ordered = FALSE)) %>% 
  mutate(hour = fct_relevel(hour, "(12,18]", after = 0)) %>%
  group_by(site) %>%
  nest() %>%
  mutate(splits = map(data, initial_split)) %>%
  mutate(
    rec = map(splits, ~recipe(formula = handover_time ~ n_dta + n_ed + wkend + hour, data = training(.x)) %>%
                step_interact(~n_dta:n_ed)),
    rec_trans = map(splits, ~recipe(formula = handover_time ~ n_dta + n_ed + wkend + hour, data = training(.x)) %>%
                      step_interact(~n_dta:n_ed) %>%
                      step_log(all_outcomes()))) %>%
  mutate(wf = map(rec, ~workflow() %>% add_recipe(.x) %>% add_model(model_spec))) %>%
  # mutate(wf_trans = map(rec_trans, ~workflow() %>% add_recipe(.x) %>% add_model(model_spec))) %>%
  mutate(fit = map2(wf, splits, ~last_fit(.x, split = .y))) 


dta_handover_lm <- lm(handover_time/60 ~ n_dta * n_ed + wkend + hour, data = dta_mod$data[[1]])
summary(dta_handover_lm)

jtools::summ(dta_handover_lm)

  jtools::export_summs(dta_handover_lm, coefs = c("(Intercept)",
                                                  "DTA Queue" = "n_dta",
                                                  "Patients in ED (non-DTA)" = "n_ed",
                                                  "Weekend" = "wkendyes",
                                                  "00:00-06:00" = "hour[0,6]",
                                                  "06:00-12:00" = "hour(6,12]",
                                                  "18:00-24:00" = "hour(18,24]",
                                                  "Interaction: DTA queue-Patients in ED" = "n_dta:n_ed"
                                                  ),
                       error_pos = "right"#,
                       # to.file = huxtable::quick_html(),
                       # file.name = "materials/handover_reg.html"
                       )

dta_resp_lm <- lm(resp_time_cat2 ~ n_dta * n_ed + wkend + hour, data = dta_mod$data[[1]])
summary(dta_resp_lm)
jtools::export_summs(dta_resp_lm, coefs = c("(Intercept)",
                                                "DTA Queue" = "n_dta",
                                                "Patients in ED (non-DTA)" = "n_ed",
                                                "Weekend" = "wkendyes",
                                                "00:00-06:00" = "hour[0,6]",
                                                "06:00-12:00" = "hour(6,12]",
                                                "18:00-24:00" = "hour(18,24]",
                                                "Interaction: DTA queue-Patients in ED" = "n_dta:n_ed"
),
error_pos = "right"#,
# to.file = huxtable::quick_html(),
# file.name = "materials/handover_reg.html"
)



dta_resp_lm_1 <- dta_mod$data[[1]] %>%
  mutate(n_ed_tot = n_dta + n_ed) %>%
lm(resp_time_cat2 ~ n_ed_tot + wkend + hour, data = .)
summary(dta_resp_lm_1)

anova(dta_resp_lm, dta_resp_lm_1)

lm(resp_time_cat1 ~ n_dta * n_ed + wkend + hour, data = dta_mod$data[[1]]) %>%
  jtools::export_summs(dta_resp_lm, coefs = c("(Intercept)",
                                              "DTA Queue" = "n_dta",
                                              "Patients in ED (non-DTA)" = "n_ed",
                                              "Weekend" = "wkendyes",
                                              "00:00-06:00" = "hour[0,6]",
                                              "06:00-12:00" = "hour(6,12]",
                                              "18:00-24:00" = "hour(18,24]",
                                              "Interaction: DTA queue-Patients in ED" = "n_dta:n_ed"
  ),
  model.names = c("Category 1 response time", "Category 2 response time"),
  error_pos = "right"#,
  # to.file = huxtable::quick_html(),
  # file.name = "materials/handover_reg.html"
  )

(p_ccf_resp <- dta_df %>% 
    left_join(resp_time_df) %>%
    filter(site == "SOUTHMEAD HOSPITAL") %>%
    as_tsibble(key = site, index = dt) %>%
    na.omit() %>%
    tsibble::fill_gaps() %>%
    CCF(x = n_dta, y = resp_time_cat2, , lag_max = 48) %>%
    autoplot() +
    bnssgtheme() + 
    labs(#title = "Raw time series",
         x = "Lag of DTA queue [1hr]",
         y = "Cross-correlation with mean cat 2 response time"))



# viz_df <- expand_grid(n_dta = seq(0, 40, 1),
#                       n_ed = 100,
#                       wkend = factor(
#                         c("yes", "no"),
#                         levels = c("yes", "no")),
#                       hour = factor(dta_mod$data[[1]]$hour %>% levels())) %>%
#   mutate(n_dta_x_n_ed = n_dta*n_ed)
# 
# viz_df$handover_time <- predict(dta_mod$fit[[1]] %>% extract_fit_engine(), viz_df)
# 
# 
# dta_mod$data[[1]] %>%
#   ggplot(aes(x=n_dta,y=handover_time/60, col = hour))+
#   geom_point(alpha = 0.25) +
#   geom_line(data=viz_df,aes(group=hour), size = 1.5) +
#   facet_grid(~wkend) +
# # scale_color_continuous(low="green",high="red") +
# # scale_colour_viridis_b(breaks = seq(-30, 30, 10)) +
#   scale_y_continuous(limits = c(-50, 300))


# # anova
# 
# rescale_coefs <- function(beta,mu,sigma) {
#   beta2 <- beta ## inherit names etc.
#   beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
#   beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
#   beta2
# } 
# 
# foo <- dta_df %>% 
#   left_join(handover_df) %>% 
#   left_join(ed_df) %>%
#   mutate(n_ed = n_ed - n_dta) %>%
#   ungroup() %>%
#   na.omit() %>%
#   filter(site == "SOUTHMEAD HOSPITAL") %>%
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>%
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   group_by(site) %>%
#   nest() %>%
#   mutate(var_mean = map(data, ~model.matrix(handover_time ~ n_dta * n_ed, .x) %>% 
#            array_branch(margin = 2) %>%
#            map(scale) %>%
#            map(attr, "scaled:center"))
#          ) %>%
#   mutate(var_sd = map(data, ~model.matrix(handover_time ~ n_dta * n_ed, .x) %>% 
#            array_branch(margin = 2) %>%
#            map(scale) %>%
#            map(attr, "scaled:scale")
#          )) %>%
#   mutate(data_raw = data) %>%
#   # mutate(data = map(data, ~.x %>% mutate(across(-dt, scale)))) %>%
#   # mutate(data = map(data, ~.x %>% mutate(across(-dt, as.numeric)))) %>%
#   mutate(mod_dta = map(data, ~lm(handover_time ~ n_dta, data = .x))) %>%
#   mutate(mod_ed = map(data, ~lm(handover_time ~ n_dta + n_ed, data = .x))) %>%
#   mutate(mod_dta_ed = map(data, ~lm(handover_time ~ n_dta * n_ed, data = .x))) %>%
#   mutate(anova = pmap(list(mod_dta,
#                            mod_ed,
#                            mod_dta_ed), ~anova(..1,
#                                                ..2,
#                                                ..3
#                                                ))) %>%
#   mutate(anova = map(anova, broom::tidy)) %>%
#   # # unnest(anova) %>%
#   mutate(mod_dta_ed_tidy = map(mod_dta_ed, broom::tidy)) %>%
#   unnest(mod_dta_ed_tidy) %>%
#   select(site, mod_dta_ed, data_raw, data, var_mean, var_sd, term, estimate) %>%
#   pivot_wider(names_from = "term", values_from = "estimate") %>%
#   nest(terms = c(-site, -mod_dta_ed, -data_raw, -data, -var_mean, -var_sd)) #%>%
#   # mutate(terms = map(terms, as.list)) %>%
#   # rowwise() %>%
#   # mutate(terms = list(pmap(list(terms, var_mean, var_sd), rescale_coefs)))
# 
# 
# # mod_df <- expand_grid(n_dta=seq(-10, 10, 1),n_ed=seq(-0, 20, 10)) %>%
# #   mutate(handover_time = foo$terms[[1]]$n_dta*n_dta +
# #          foo$terms[[1]]$n_ed*n_ed +
# #          foo$terms[[1]]$`n_dta:n_ed`*(n_dta*n_ed))
# 
# mod_df <- expand_grid(n_dta=seq(-15, 15, 1),n_ed=seq(0, 30, 15)) 
# mod_df$handover_time <- predict(foo$mod_dta_ed[[1]], mod_df)
# 
# ggplot(foo$data_raw[[1]],aes(x=n_dta,y=handover_time/60,color=n_ed))+
#   geom_point(alpha = 0.25) +
#   geom_line(data=mod_df,aes(group=n_ed), size = 1.5) +
#   # scale_color_continuous(low="green",high="red") +
#   scale_colour_viridis_b(breaks = seq(-30, 30, 10)) +
#   scale_y_continuous(limits = c(-50, 300))
# 
# foo <- dta_df %>% 
#   left_join(handover_df) %>% 
#   left_join(ed_df) %>%
#   mutate(n_ed = n_ed - n_dta) %>%
#   ungroup() %>%
#   na.omit() %>% 
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>% 
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   group_by(site) %>%
#   nest() %>%
#   mutate(mod = map(data, ~lm(handover_time/60 ~ n_dta, data = .x))) %>%
#   mutate(plot = map(mod, ggfortify:::autoplot.lm)) %>%
#   mutate(mod_glance = map(mod, glance)) %>%
#   # mutate(mod_augment = map(mod, augment)) %>%
#   mutate(mod_tidy = map(mod, tidy)) %>%
#   mutate(rsq = map_dbl(mod_glance, 'r.squared')) %>%
#   unnest(mod_tidy)
#   
# # lasso/ridge  
#   
# library(tidymodels)  
# 
# model_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
#   set_engine("glmnet")
# 
# penalty_grid = grid_regular(penalty(), levels = 50)
#   
# foo <- dta_df %>% 
#   left_join(handover_df) %>% 
#   left_join(ed_df) %>%
#   mutate(n_ed = n_ed - n_dta) %>%
#   ungroup() %>%
#   na.omit() %>% 
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>% 
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   ungroup() %>%
#   as_tibble() %>%
#   select(-dt) %>%
#   group_by(site) %>%
#   nest() %>%
#   mutate(boot = map(data, ~bootstraps(.x, strata = handover_time))) %>%
#   mutate(rec = map(data, ~recipe(handover_time ~ ., data = .x) %>%
#            step_zv(all_numeric(), -all_outcomes()) %>%
#            step_normalize(all_numeric(), -all_outcomes()) %>% prep())) %>%
#   mutate(wf = map(rec, ~ workflow() %>% add_recipe(.x))) %>%
#   mutate(tune = map2(wf, boot, ~tune_grid(.x %>% add_model(model_spec), resamples = .y, grid = penalty_grid)))
#   mutate(fit = map2(wf, data, ~.x %>%
#                       add_model(model_spec) %>%
#                       fit(data = .y) %>%
#                       extract_fit_parsnip() %>%
#                       tidy())) %>%
#   unnest(fit)
#   
#   
# # minors time in ed
# 
# dta_df %>% 
#   left_join(minor_time_in_ed_df) %>%
#   as_tsibble(key = site, index = dt) %>%
#   na.omit() %>%
#   tsibble::fill_gaps() %>%
#   CCF(x = n_dta, y = time_in_ed) %>%
#   autoplot()
# 
# dta_df %>% 
#   left_join(minor_time_in_ed_df) %>%
#   na.omit() %>% 
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>%  
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   CCF(x = n_dta, y = handover_time) %>%
#   autoplot()
# 
# 
# dta_df %>% 
#   left_join(handover_df) %>% 
#   left_join(minor_time_in_ed_df) %>%
#   na.omit() %>% 
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>% 
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   ggplot(aes(x = lag(n_dta), y = handover_time)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_wrap(vars(site))
# 
# dta_df %>% 
#   left_join(handover_df) %>% 
#   left_join(minor_time_in_ed_df) %>%
#   na.omit() %>% 
#   pivot_longer(cols = -c(site, dt), names_to = "metric", values_to = "value") %>%
#   as_tsibble(index = dt, key = c(site, metric)) %>%
#   tsibble::fill_gaps() %>%
#   tidyr::fill(value, .direction = "down") %>%
#   model(STL(value)) %>%
#   components() %>% 
#   select(dt, site, metric, remainder) %>%
#   # as_tibble() %>%
#   pivot_wider(names_from = "metric", values_from = "remainder") %>%
#   group_by(site) %>%
#   nest() %>%
#   mutate(mod = map(data, ~lm(handover_time/60 ~ lag(n_dta), data = .x))) %>%
#   mutate(mod = map(mod, broom::tidy)) %>%
#   unnest(mod)
# 
# # dta_df %>% 
# #   left_join(minor_time_in_ed_df) %>%
# #   as_tsibble(key = site, index = dt) %>%
# #   na.omit() %>%
# #   tsibble::fill_gaps() %>%
# #   CCF(x = n_dta, y = time_in_ed) %>%
# #   autoplot()
# # 
# # dta_df %>% 
# #   left_join(handover_df) %>%
# #   ggplot(aes(x = lag(n_dta, 1), y = handover_time)) + 
# #   geom_jitter() +
# #   geom_smooth(method = "lm") +
# #   facet_wrap(vars(site))
# # 
# # dta_df %>% 
# #   left_join(handover_df) %>%
# #   group_by(site) %>%
# #   nest() %>%
# #   mutate(mod = map(data, ~lm(handover_time/60 ~ lag(n_dta), data = .x))) %>%
# #   mutate(mod = map(mod, broom::tidy)) %>%
# #   unnest(mod)
# # 
# # 
# # dta_df %>% 
# #   left_join(handover_df) %>%
# #   group_by(site) %>%
# #   nest() %>%
# #   # centre and scale
# #   # mutate(data = map(data, ~.x %>% mutate(value = scale(value)[,1]))) %>%
# #   mutate(cor = map(data, ~cor.test(.x$handover_time, lag(.x$n_dta)) %>% broom::tidy())) %>%
# #   unnest(cor) 
# 
# 
# 
# 
# 
# 
# 
