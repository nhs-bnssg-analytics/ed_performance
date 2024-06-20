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
n_hour <- 2

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
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
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

# treatment time
trt_df <- local({
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
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
  # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  
  dat_trt <- dat_ecds %>%
    mutate(dt = floor_date(arrival_dt, unit = glue::glue("{n_hour} hours"))) %>%
    select(dt, site = Site_Name, trt_time) %>%
    na.omit()
  
  dat_trt
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
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
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
  
  
  
  # handover_df <- actv_swafst %>%
  #   group_by(site = Site_Name) %>%
  #   nest() %>%
  #   mutate(handover_time = list(map2_dbl(snapshot_int, data, ~filter(.y, handover_time %within% .x) %>%
  #                                          pull(Handover_Duration__seconds_) %>%
  #                                          median(na.rm = TRUE)
  #                                        ))) %>%
  #   mutate(handover_df = map(handover_time, function(handover_time) map2_df(handover_time, snapshot_seq, ~tibble(dt = .y, handover_time = .x)))) %>%
  #   select(site, handover_df) %>%
  #   unnest(handover_df)

  handover_df <- actv_swafst %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(handover_time = list(map2(
      snapshot_int,
      data,
      ~ filter(.y, handover_time %within% .x) %>%
        select(age = Age_Bracket, handover_time = Handover_Duration__seconds_)
    ))) %>%
    mutate(handover_df = map(handover_time, function(handover_time)
      map2_df(handover_time, snapshot_seq, ~ bind_cols(tibble(dt = .y), .x)))) %>%
    select(site, handover_df) %>%
    unnest(handover_df) %>%
    arrange(dt)
  
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
    mutate(dt = floor_date(T0_Time_of_Call, glue::glue("{n_hour} hours")),
           call_category = Incident_Colour,
           response = (`Response_Target_1_Clock Stop` - `Response_Target_1_Clock Start`)/dminutes(1)) %>%
    filter(call_category %in% str_c("Category ", c(1, 2, 3))) %>%
    mutate(call_category = recode(call_category,
                                  "Category 3"= "cat3",
                                  "Category 2"= "cat2",
                                  "Category 1"= "cat1")) %>%
    select(dt, call_category, response) 
  
  response_time
})


# Handover time quantile analysis
# fitting model
# exponential model fits well for n_dta < 30
f <- function(x, a, c) {
 exp(a*x) + c 
}

plot_df <- map_dfr(seq(0.7, 0.9, 0.1), ~

{ plot_df <- dta_df %>% 
  left_join(handover_df) %>% 
  # left_join(minor_time_in_ed_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>%
  group_by(n_dta) %>%
  summarise(handover_time = quantile(handover_time/60, .x)) %>%
  filter(n_dta <= 30)

dta_handover_mod <- nls(formula = handover_time ~ f(n_dta, a, c), data = plot_df, start = list(a = 1, c = 0))

plot_df <- bind_cols(plot_df, investr::predFit(dta_handover_mod, plot_df, interval = "confidence", level= 0.9))
plot_df$q <- factor(.x, labels = scales::percent(.x))

plot_df$r <- round(log(30 - min(29, coef(dta_handover_mod)[2]))/coef(dta_handover_mod)[1])
plot_df$c <- coef(dta_handover_mod)[2]

plot_df}
)



(scatter_1 <- plot_df %>%
    ggplot(aes(x = n_dta, y = handover_time)) +
    geom_point(data = {dta_df %>% 
      left_join(handover_df) %>% 
      # left_join(minor_time_in_ed_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL", n_dta < 30) %>%
      mutate(handover_time = handover_time/60) %>%
      na.omit()}, aes(x = n_dta, y = handover_time, col = "All data"), alpha = 0.015) +
    geom_point(aes(col = q, group = q), size = 2.5) +
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    geom_line(aes(y = fit, col = q)) +
    geom_hline(yintercept = 30, linetype = 2) +
    bnssgtheme() +
    scale_colour_manual(values = c(viridis::viridis(n = 3, end = 0.8, begin = 0.2, direction = -1), "black"),
                        guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1))) +
    labs(y = "Ambulance handover time\n(min)",
         x = "DTA queue",
         colour = "Handover time quantile") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text())
)


ggsave("materials/dta_handover_scatter_quant.png",
       scatter_1,
       height = 5,
       width = 7.5, 
       scale = 0.9)

# handover time for patients 65+
dta_df %>% 
        left_join(handover_df) %>% 
        # left_join(minor_time_in_ed_df) %>%
        filter(site == "SOUTHMEAD HOSPITAL") %>%
    mutate(age_flag = if_else(
      age %in% c(
        "65-69 Years Old",
        "70-74 Years Old",
        "75-79 Years Old",
        "80-84 Years Old",
        "85-89 Years Old",
        "90-94 Years Old",
        "95-99 Years Old",
        "100 Years Old Or Over"
      ), "65+", "under 65")
    ) %>%
        mutate(handover_time = handover_time/60) %>%
        na.omit() %>%
  ggplot(aes(x = n_dta, y = handover_time, col = age_flag)) +
    geom_point(alpha = 0.015) +
    geom_smooth(method = "lm") +
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 180, 30)) +
    bnssgtheme()



# time seen for treatment vs dta

(scatter_2 <- dta_df %>%
  left_join(trt_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>%
  group_by(n_dta) %>%
  ggplot(aes(x = n_dta, y = trt_time)) + 
  geom_point(alpha = 0.015) +
  geom_smooth(data = {dta_df %>%
      left_join(trt_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL") %>%
      na.omit() %>%
      group_by(n_dta) %>%
      summarise(trt_time = quantile(trt_time, 0.9))},
      col = bnssgcols[4],
      method = "lm") +
  geom_point(data = {dta_df %>%
      left_join(trt_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL") %>%
      na.omit() %>%
      group_by(n_dta) %>%
      summarise(trt_time = quantile(trt_time, 0.9))},
      col = bnssgcols[4],
      size = 2.5) +
  scale_y_continuous(limits = c(NA, 600), breaks = seq(0, 600, 60)) +
  bnssgtheme() +
  labs(y = "Time to be seen for treatment\n(min)",
       x = "DTA queue"))



ggsave("materials/treat_time_scatter_quant.png",
       scatter_2,
       height = 5,
       width = 7.5, 
       scale = 0.9)

dta_df %>%
  left_join(trt_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>%
  group_by(n_dta) %>%
  summarise(trt_time = quantile(trt_time, 0.9)) %>%
  lm(trt_time ~ n_dta, data = .) %>%
  summary()


dta_df %>%
  left_join(trt_df) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit() %>%
  group_by(n_dta) %>%
  summarise(trt_time = quantile(trt_time, 0.9)) %>%
  lm(trt_time ~ n_dta, data = .) %>%
  summary()



model_spec <- linear_reg() %>%
  set_engine("lm")

dta_mod <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  # left_join(resp_time_df) %>%
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




dta_mod_age <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  # left_join(resp_time_df) %>%
  mutate(n_ed = n_ed - n_dta) %>%
  ungroup() %>%
  na.omit() %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(
    age %in% c(
      "65-69 Years Old",
      "70-74 Years Old",
      "75-79 Years Old",
      "80-84 Years Old",
      "85-89 Years Old",
      "90-94 Years Old",
      "95-99 Years Old",
      "100 Years Old Or Over"
    )
  ) %>%
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


dta_handover_age_lm <- lm(handover_time/60 ~ n_dta * n_ed + wkend + hour, data = dta_mod_age$data[[1]])
summary(dta_handover_age_lm)

dta_mod_age_2 <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  # left_join(resp_time_df) %>%
  mutate(n_ed = n_ed - n_dta) %>%
  ungroup() %>%
  na.omit() %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(
    !age %in% c(
      "65-69 Years Old",
      "70-74 Years Old",
      "75-79 Years Old",
      "80-84 Years Old",
      "85-89 Years Old",
      "90-94 Years Old",
      "95-99 Years Old",
      "100 Years Old Or Over"
    )
  ) %>%
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


dta_handover_age_lm_2 <- lm(handover_time/60 ~ n_dta * n_ed + wkend + hour, data = dta_mod_age_2$data[[1]])
summary(dta_handover_age_lm_2)

huxtable::quick_pptx(jtools::export_summs(
                     dta_handover_age_lm_2,
                     dta_handover_age_lm,
                     coefs = c("(Intercept)",
                                                "DTA Queue" = "n_dta",
                                                "Patients in ED (non-DTA)" = "n_ed",
                                                "Weekend" = "wkendyes",
                                                "00:00-06:00" = "hour[0,6]",
                                                "06:00-12:00" = "hour(6,12]",
                                                "18:00-24:00" = "hour(18,24]",
                                                "Interaction: DTA queue-Patients in ED" = "n_dta:n_ed"
),
error_pos = "right",
model.names = c("Under 65", "65+"),
file.name = "materials/handover_reg.pptx"
))

jtools::effect_plot(dta_handover_age_lm_2,
                    pred = "n_dta",
                    interval = TRUE)


dta_mod_age_3 <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  # left_join(resp_time_df) %>%
  mutate(n_ed = n_ed - n_dta) %>%
  ungroup() %>%
  na.omit() %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  mutate(age_flag = if_else(
    age %in% c(
      "65-69 Years Old",
      "70-74 Years Old",
      "75-79 Years Old",
      "80-84 Years Old",
      "85-89 Years Old",
      "90-94 Years Old",
      "95-99 Years Old",
      "100 Years Old Or Over"
    ), "65+", "under 65")
  ) %>%
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


dta_handover_age_lm_3 <- lm(handover_time/60 ~ n_dta * n_ed + n_dta*age_flag + wkend + hour, data = dta_mod_age_3$data[[1]])
summary(dta_handover_age_lm_3)



resp_plot_df <- dta_df %>%
  left_join(filter(resp_time_df, call_category == "cat3")) %>%
  filter(site == "SOUTHMEAD HOSPITAL") %>%
  na.omit()
  

  (scatter_3 <- resp_plot_df %>%
  group_by(n_dta) %>%
  summarise(response = quantile(response, 0.9)) %>%
  ggplot(aes(x = n_dta, y = response, col = "90% quantile")) +
  geom_point(data = resp_plot_df, aes(col = "All data"), alpha = 0.015) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", show.legend = FALSE)  +
  scale_y_continuous(limits = c(0, 480), breaks = seq(0, 480, 60)) +
  bnssgtheme() +
  scale_colour_manual(values = c(unname(bnssgcols[4]), "black"),
                      guide = guide_legend(override.aes = list(size = 3,
                                                               alpha = 1))) +
  guides(alpha = "none") +
  labs(y = "Category 3 response time\n(min)",
       x = "DTA queue",
       colour = "Category 3 response time:") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text()))


ggsave("materials/resptime_scatter_quant.png",
       scatter_3,
       height = 5,
       width = 7.5, 
       scale = 0.9)  

dta_mod$data[[1]] %>%
  group_by(n_dta, n_ed, wkend, hour) %>%
  summarise(resp_time_cat3 = quantile(resp_time_cat3, 0.9)) %>%
lm(resp_time_cat3 ~ n_dta * n_ed + wkend + hour, data = .) %>%
  summary()


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
error_pos = "right"
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

