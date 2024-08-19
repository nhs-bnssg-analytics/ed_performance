require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)


# report_start <- '2023-08-01'
# report_end <- '2024-03-01'
report_start <- '2021-06-01'
report_end <- '2024-06-01'
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
    mutate(dta_time = dta_int/dminutes(1)) %>%
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
  # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  
  dat_dtas <- dat_ecds %>%
    filter(dta_time > 0) %>%
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

# handover_df %>%
#   filter(site == "SOUTHMEAD HOSPITAL") %>%
#   filter(dt %within% interval(ymd("2024-02-15"), ymd("2024-02-15")+dweeks(4))) %>%
#   ggplot(aes(x = dt, y = handover_time/60)) +
#   geom_line() + 
#   bnssgtheme() +
#   labs(x = "Date", y = "Median ambulance handover time\n(mins)") 
# 
# ggsave("materials/handover_plot.png",
#        last_plot(),
#        height = 5,
#        width = 7.5, 
#        scale = 0.9)



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
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
where Outcome in ('See & Convey', 'See & Treat') AND
CCG = '15C' AND
  CONVERT(DATE, [T0_Time_of_Call]) > '{report_start}'"
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
    select(dt, outcome = Outcome, call_category, response) 
  
  response_time
})

saveRDS(dta_df, "data/upstream_dta_df.RDS")
saveRDS(ed_df, "data/upstream_ed_df.RDS")
saveRDS(handover_df, "data/upstream_handover_df.RDS")
saveRDS(resp_time_df, "data/upstream_resp_time_df.RDS")


dta_df <-  readRDS("data/upstream_dta_df.RDS")
ed_df <-  readRDS("data/upstream_ed_df.RDS")
handover_df <-  readRDS("data/upstream_handover_df.RDS")
resp_time_df <-  readRDS("data/upstream_resp_time_df.RDS")



# Handover time quantile analysis
# fitting model
# exponential model fits well for n_dta < 30
f <- function(x, a, c) {
 exp(a*x) + c 
}

# plot_df <- map_dfr(c(0.5, seq(0.7, 0.9, 0.1)), ~
plot_df <- map_dfr(c(0.5, 0.9), ~

{ plot_df <- dta_df %>% 
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  left_join(handover_df) %>% 
  # left_join(minor_time_in_ed_df) %>%
  na.omit() %>%
  group_by(n_dta) %>%
  summarise(handover_time = quantile(handover_time/60, .x)) %>%
  filter(n_dta <= 30)

dta_handover_mod <- nls(formula = handover_time ~ f(n_dta, a, c), data = plot_df, start = list(a = 1, c = 0))

plot_df <- bind_cols(plot_df, investr::predFit(dta_handover_mod, plot_df, interval = "confidence", level= 0.9))
plot_df$q <- factor(.x, labels = scales::percent(.x)) %>% paste0(" quantile") %>% forcats::fct_recode("Median" = "50% quantile")

plot_df$r <- round(log(30 - min(29, coef(dta_handover_mod)[2]))/coef(dta_handover_mod)[1])
plot_df$c <- coef(dta_handover_mod)[2]

plot_df}
)



(scatter_1 <- plot_df %>%
    ggplot(aes(x = n_dta, y = handover_time)) +
    geom_point(data = {dta_df %>% 
      left_join(handover_df) %>% 
      # left_join(minor_time_in_ed_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL", n_dta <= 30) %>%
      filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
      mutate(handover_time = handover_time/60) %>%
      na.omit()}, aes(x = n_dta, y = handover_time, col = "All data"), alpha = 0.015) +
    geom_point(aes(col = q, group = q), size = 2.5) +
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, col = q)) +
    geom_hline(yintercept = 30, linetype = 2) +
    bnssgtheme() +
    scale_colour_manual(values = c(unname(bnssgcols[c(7,4)]), "black"),
                        guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1))) +
    # scale_colour_manual(values = c(viridis::viridis(n = 2, end = 0.6, begin = 0.2, direction = -1), "black"),
    #                     guide = guide_legend(override.aes = list(size = 3,
    #                                                              alpha = 1))) +
    labs(y = "Ambulance handover time\n(min)",
         x = "DTA queue",
         colour = "Handover time quantile") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text())
)


ggsave("materials/dta_handover_scatter_quant_all_site.png",
       scatter_1,
       height = 5,
       width = 7.5, 
       scale = 0.9)

(scatter_1 <- plot_df %>%
    ggplot(aes(x = n_dta, y = handover_time)) +
    geom_point(data = {dta_df %>% 
        left_join(handover_df) %>% 
        # left_join(minor_time_in_ed_df) %>%
        filter(site == "SOUTHMEAD HOSPITAL", n_dta <= 30) %>%
        filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
        mutate(handover_time = handover_time/60) %>%
        na.omit()}, aes(x = n_dta, y = handover_time, shape = "All data"), alpha = 0.015) +
    geom_point(aes(shape = q, group = q), fill = "white", size = 2.5) +
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_hline(yintercept = 30, linetype = 2) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                      alpha = 1))) +
    # scale_colour_manual(values = c("black", "black", "black"),
    #                     guide = guide_legend(override.aes = list(size = 3,
    #                                                              alpha = 1))) +
    # scale_colour_manual(values = c(viridis::viridis(n = 2, end = 0.6, begin = 0.2, direction = -1), "black"),
    #                     guide = guide_legend(override.aes = list(size = 3,
    #                                                              alpha = 1))) +
    labs(y = "Ambulance handover time\n(min)",
         x = "Queue size for admission",
         shape = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text())
)


ggsave("materials/dta_handover_scatter_quant_paper_all_site.png",
       scatter_1,
       bg = "white",
       height = 5,
       width = 7.5, 
       scale = 0.7)


# Normalisation and standardisation
# Site normalisation data
site_resus_maj <- tibble(site = c("BRI", "WESTON", "SOUTHMEAD HOSPITAL"), maj = c(16, 9, 14), resus = c(8, 4, 6)) %>%
  mutate(total = maj + resus)



handover_boxplot <- ({
  
  x_lab <- "DTA Queue as a Percentage of Majors & Resus Cubicles"
  y_lab <- "Ambulance Handover Time\n(min)"

  # Non standardised plot
  p_raw <- dta_df %>%
    left_join(handover_df) %>%
    left_join(site_resus_maj) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    filter(n_dta < 35) %>%
    mutate(n_dta_norm = n_dta/total,
           standard = 30) %>%
    distinct() %>%
    # mutate(n_dta = factor(n_dta)) %>%
    ggplot(aes(x = n_dta_norm, y = handover_time/60)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta, width = 1)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "90th percentile", shape = "90th percentile")) +
    geom_point(aes(y = standard, col = "National target")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_x_continuous(labels = scales::percent) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    # scale_colour_manual(values = c(viridis::viridis(n = 2, end = 0.6, begin = 0.2, direction = -1), "black"),
    #                     guide = guide_legend(override.aes = list(size = 3,
    #                                                              alpha = 1))) +
    labs(title = "Raw data",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text())
  

  
# Standardised plot  
mod_dat <- dta_df %>% 
    left_join(handover_df) %>%
  left_join(site_resus_maj) %>%
  mutate(n_dta_norm = n_dta/total) %>%
  # left_join(ed_df) %>%
    # mutate(n_ed = n_ed - n_dta) %>%
    mutate(age = factor(age)) %>%
    mutate(age = fct_recode(age, 
                            "0-9 Years Old" = "0-4 Years Old",
                            "0-9 Years Old" = "5-9 Years Old",
                            "10-19 Years Old" = "10-14 Years Old",
                            "10-19 Years Old" = "15-19 Years Old",
                            "20-29 Years Old" = "20-24 Years Old",
                            "20-29 Years Old" = "25-29 Years Old",
                            "30-39 Years Old" = "30-34 Years Old",
                            "30-39 Years Old" = "35-39 Years Old",
                            "40-49 Years Old" = "40-44 Years Old",
                            "40-49 Years Old" = "45-49 Years Old",
                            "50-59 Years Old" = "50-54 Years Old",
                            "50-59 Years Old" = "55-59 Years Old",
                            "60-69 Years Old" = "60-64 Years Old",
                            "60-69 Years Old" = "65-69 Years Old",
                            "70-79 Years Old" = "70-74 Years Old",
                            "70-79 Years Old" = "75-79 Years Old",
                            "80-89 Years Old" = "80-84 Years Old",
                            "80-89 Years Old" = "85-89 Years Old",
                            "90-99 Years Old" = "90-94 Years Old",
                            "90-99 Years Old" = "95-99 Years Old"
    )) %>%
    mutate(age = fct_relevel(age, "100 Years Old Or Over", after = 10)) %>%
    ungroup() %>%
    na.omit() %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    filter(handover_time > 0) %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "Yes", "No")),
           hour = factor(cut(hour(dt), breaks = seq(0, 24, 6), include.lowest = TRUE), ordered = FALSE)) %>% 
    mutate(hour = fct_relevel(hour, "(12,18]", after = 0)) %>%
    mutate(hour = fct_recode(hour, "00:00-06:00" = "[0,6]",
                             "06:00-12:00" = "(6,12]",
                             "12:00-18:00" = "(12,18]",
                             "18:00-24:00" = "(18,24]")) %>%
    select(-site, -dt, -maj, -resus, -total) %>%
    ungroup() 

handover_mod <- lm(handover_time ~ . -n_dta -n_dta_norm, data = mod_dat)
handover_pred <- predict(handover_mod, mod_dat)

mod_dat <- mod_dat %>%
  mutate(handover_standard = handover_pred,
         handover_adj = handover_time - handover_standard) 



plot_df <- mod_dat %>%
  filter(n_dta < 35) 


p_std <- plot_df %>%
  ggplot(aes(x = n_dta_norm, y = handover_adj/60)) +
  # geom_boxplot(outliers = FALSE) +
  geom_boxplot(mapping = aes(group = cut_width(n_dta, width = 1)), outliers = FALSE, fatten = NULL) +
  # scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
  # geom_point(aes(y = standard), col = "red") +
  stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
  stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(shape = "90th percentile")) +
  # bnssgtheme() +
  theme_minimal() +
  scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                      alpha = 1))) +
  scale_x_continuous(labels = scales::percent) +
  scale_shape_manual(values = c(19, 17),
                      guide = guide_legend(override.aes = list(size = 3,
                                                               alpha = 1))) +
  # scale_colour_manual(values = c(viridis::viridis(n = 2, end = 0.6, begin = 0.2, direction = -1), "black"),
  #                     guide = guide_legend(override.aes = list(size = 3,
  #                                                              alpha = 1))) +
  labs(title = "Model Standardised",
       y = y_lab,
       x = x_lab,
       shape = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text())

list(p_raw = p_raw, p_std = p_std)

})

patchwork::wrap_plots(handover_boxplot$p_raw,
                      handover_boxplot$p_std,
                      ncol = 1, axes = "collect", guides = "collect") &
  theme(legend.position = "bottom")


ggsave("materials/dta_handover_box_paper.png",
       last_plot(),
       bg = "white",
       height = 7.5,
       width = 7.5, 
       scale = 0.8)

ggsave("materials/dta_handover_box_paper.png",
       handover_boxplot$p_raw,
       bg = "white",
       height = 5,
       width = 7.5, 
       scale = 0.7)


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
  geom_smooth(data = {dta_df %>%
      left_join(trt_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL") %>%
      na.omit() %>%
      group_by(n_dta) %>%
      summarise(trt_time = quantile(trt_time, 0.5))},
      col = bnssgcols[7],
      method = "lm") +
  geom_point(data = {dta_df %>%
      left_join(trt_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL") %>%
      na.omit() %>%
      group_by(n_dta) %>%
      summarise(trt_time = quantile(trt_time, 0.5))},
      aes(col = "Median"),
      size = 2.5) +
  geom_point(data = {dta_df %>%
      left_join(trt_df) %>%
      filter(site == "SOUTHMEAD HOSPITAL") %>%
      na.omit() %>%
      group_by(n_dta) %>%
      summarise(trt_time = quantile(trt_time, 0.9))},
      aes(col = "90%"),
      size = 2.5) +
    scale_colour_manual(values = c(unname(bnssgcols[c(4, 7)])),
                        guide = guide_legend(override.aes = list(size = 3,
                                                                 alpha = 1))) +
  scale_y_continuous(limits = c(NA, 600), breaks = seq(0, 600, 60)) +
  bnssgtheme() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text()) +
  labs(y = "Time to be seen for treatment\n(min)",
       x = "DTA queue",
       col = "Time to be seen for treatment quantile")) 



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
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
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
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  na.omit()

saveRDS(resp_plot_df, "data/dta_resp_plot_df_all_site.RDS")


  (scatter_3 <- resp_plot_df %>%
      ungroup() %>%
      nest() %>%
      expand_grid(q = c(0.5, 0.9)) %>%
      mutate(data = map2(data, q,
                         ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
      )) %>%
      unnest(cols = c(data)) %>%
  ggplot(aes(x = n_dta, y = response, col = factor(q))) +
  geom_point(data = resp_plot_df, aes(col = "All data"), alpha = 0.01) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)  +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, 60)) +
  scale_x_continuous(limits = c(0, 35)) +
  bnssgtheme() +
  scale_colour_manual(values = c(unname(bnssgcols[c(7, 4)]), "black"),
                      guide = guide_legend(override.aes = list(size = 3,
                                                               alpha = 1))) +
  guides(alpha = "none") +
  labs(y = "Category 3 response time\n(min)",
       x = "DTA queue",
       colour = "Category 3 response time:") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text()))


ggsave("materials/resptime_scatter_quant_all_site.png",
       scatter_3,
       height = 5,
       width = 7.5, 
       scale = 0.9)  


#   (scatter_3 <- resp_plot_df %>%
#       ungroup() %>%
#       nest() %>%
#       expand_grid(q = c(0.5, 0.9)) %>%
#       mutate(data = map2(data, q,
#                          ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
#       )) %>%
#       unnest(cols = c(data)) %>%
#       mutate(q = factor(q, labels = c("Median", "90% quantile"))) %>%
#   ggplot(aes(x = n_dta, y = response/60, shape = q)) +
#   geom_point(data = resp_plot_df, aes(shape = "All data"), alpha = 0.01) +
#   geom_point(size = 2.5, fill = "white") +
#   # geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, col = "black")  +
#   scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
#   scale_x_continuous(limits = c(0, 35)) +
#   theme_minimal() +
#   scale_shape_manual(values = c(21, 24, 19),
#                       guide = guide_legend(override.aes = list(size = 3,
#                                                                alpha = 1))) +
#   guides(alpha = "none") +
#   labs(y = "Category 3 response time\n(hour)",
#        x = "Queue size for admission",
#        shape = "") +
#   theme(legend.position = "bottom",
#         legend.justification = "center",
#         legend.title = element_text()))
# 
# 
# ggsave("materials/resptime_scatter_quant_paper_all_site.png",
#        scatter_3,
#        bg = "white",
#        height = 5,
#        width = 7.5, 
#        scale = 0.7)  


# resp_1_plot_df <- dta_df %>%
#   left_join(filter(resp_time_df, call_category == "cat1")) %>%
#   # filter(site == "SOUTHMEAD HOSPITAL") %>%
#   filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
#   na.omit()
# 
# 
# (scatter_3 <- resp_1_plot_df %>%
#     ungroup() %>%
#     nest() %>%
#     expand_grid(q = c(0.5, 0.9)) %>%
#     mutate(data = map2(data, q,
#                        ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
#     )) %>%
#     unnest(cols = c(data)) %>%
#     mutate(q = factor(q, labels = c("Median", "90% quantile"))) %>%
#     ggplot(aes(x = n_dta, y = response/60, shape = q)) +
#     geom_point(data = resp_plot_df, aes(shape = "All data"), alpha = 0.01) +
#     geom_point(size = 2.5, fill = "white") +
#     # geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, col = "black")  +
#     scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
#     scale_x_continuous(limits = c(0, 35)) +
#     theme_minimal() +
#     scale_shape_manual(values = c(21, 24, 19),
#                        guide = guide_legend(override.aes = list(size = 3,
#                                                                 alpha = 1))) +
#     guides(alpha = "none") +
#     labs(y = "Category 1 response time\n(hour)",
#          x = "Queue size for admission",
#          shape = "") +
#     theme(legend.position = "bottom",
#           legend.justification = "center",
#           legend.title = element_text()))
# 
# 
# resp_2_plot_df <- dta_df %>%
#   left_join(filter(resp_time_df, call_category == "cat2")) %>%
#   # filter(site == "SOUTHMEAD HOSPITAL") %>%
#   filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
#   na.omit()
# 
# 
# (scatter_4 <- resp_2_plot_df %>%
#     ungroup() %>%
#     nest() %>%
#     expand_grid(q = c(0.5, 0.9)) %>%
#     mutate(data = map2(data, q,
#                        ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
#     )) %>%
#     unnest(cols = c(data)) %>%
#     mutate(q = factor(q, labels = c("Median", "90% quantile"))) %>%
#     ggplot(aes(x = n_dta, y = response/60, shape = q)) +
#     geom_point(data = resp_plot_df, aes(shape = "All data"), alpha = 0.01) +
#     geom_point(size = 2.5, fill = "white") +
#     # geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, col = "black")  +
#     scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
#     scale_x_continuous(limits = c(0, 35)) +
#     theme_minimal() +
#     scale_shape_manual(values = c(21, 24, 19),
#                        guide = guide_legend(override.aes = list(size = 3,
#                                                                 alpha = 1))) +
#     guides(alpha = "none") +
#     labs(y = "Category 2 response time\n(hour)",
#          x = "Queue size for admission",
#          shape = "") +
#     theme(legend.position = "bottom",
#           legend.justification = "center",
#           legend.title = element_text()))
# 
# resp_3_plot_df <- dta_df %>%
#   left_join(filter(resp_time_df, call_category == "cat3")) %>%
#   # filter(site == "SOUTHMEAD HOSPITAL") %>%
#   filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
#   na.omit()
# 
# 
# (scatter_5 <- resp_3_plot_df %>%
#     ungroup() %>%
#     nest() %>%
#     expand_grid(q = c(0.5, 0.9)) %>%
#     mutate(data = map2(data, q,
#                        ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
#     )) %>%
#     unnest(cols = c(data)) %>%
#     mutate(q = factor(q, labels = c("Median", "90% quantile"))) %>%
#     ggplot(aes(x = n_dta, y = response/60, shape = q)) +
#     geom_point(data = resp_plot_df, aes(shape = "All data"), alpha = 0.01) +
#     geom_point(size = 2.5, fill = "white") +
#     # geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, col = "black")  +
#     scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
#     scale_x_continuous(limits = c(0, 35)) +
#     theme_minimal() +
#     scale_shape_manual(values = c(21, 24, 19),
#                        guide = guide_legend(override.aes = list(size = 3,
#                                                                 alpha = 1))) +
#     guides(alpha = "none") +
#     labs(y = "Category 3 response time\n(hour)",
#          x = "Queue size for admission",
#          shape = "") +
#     theme(legend.position = "bottom",
#           legend.justification = "center",
#           legend.title = element_text()))
# 
# 
# sct_ptc <- patchwork::wrap_plots(scatter_3, scatter_4, scatter_5,
#                                  ncol = 1,
#                                  axes = "collect",
#                                  guides = "collect") &
#   theme(legend.position = "bottom")
# 
# ggsave("materials/queue_paper_all_site.png",
#        sct_ptc,
#        bg = "white",
#        height = 10,
#        width = 7.5, 
#        scale = 0.7)  


dta_df %>%
  left_join(filter(resp_time_df, call_category == "cat3")) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  na.omit() %>%
  ungroup() %>%
  nest() %>%
  expand_grid(q = c(0.5, 0.9)) %>%
  mutate(data = map2(data, q,
                     ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
  )) %>%
  unnest(cols = c(data)) %>%
  group_by(q) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(response ~ n_dta, data = .x))) %>%
  mutate(stats = map(mod, broom::tidy)) %>%
  unnest(stats)


# q = 0.5, coeff = 6.01
# q = 0.9, coeff = 14.4

time_std <- tibble(call_category = c("cat1", "cat2", "cat3"),
                   std = c(15, 40, 120)) %>%
  expand_grid(n_dta = 1:35) %>%
  mutate(n_dta_norm = n_dta/total)
  


labels <- c(cat1 = "Category 1",
            cat2 = "Category 2",
            cat3 = "Category 3")

dta_df %>%
  filter(n_dta < 35) %>%
  group_by(dt) %>%
  summarise(n_dta = sum(n_dta)) %>%
  expand_grid({site_resus_maj %>% summarise(total = sum(total))}) %>%
  mutate(n_dta_norm = n_dta/total) %>%
  left_join(resp_time_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  na.omit() %>%
  distinct() %>% 
  # mutate(n_dta = factor(n_dta)) %>%
  ggplot(aes(x = n_dta_norm, y = response)) +
  # geom_boxplot(outliers = FALSE) +
  geom_boxplot(mapping = aes(group = cut_width(n_dta, width = 2)), outliers = FALSE, fatten = NULL) +
  # geom_point(data = time_std, mapping = aes(y = std, x = n_dta_norm), col = "red") + 
  stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
  stat_summary(fun = ~quantile(.x, 0.9), geom = "point", col = "black", aes(shape = "90th percentile")) +
  facet_wrap(vars(call_category), ncol = 1,
             scales = "free",
             labeller = as_labeller(labels)) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  guides(alpha = "none") +
  labs(y = "Ambulance Response Time\n(min)",
       x = "Queue size for admission",
       shape = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text())



ggsave("materials/dta_amb_resp_paper.png",
       last_plot(),
       bg = "white",
       height = 10,
       width = 7.5, 
       scale = 0.7)




# basic model
dta_df %>%
  left_join(resp_time_df) %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  na.omit() %>%
  ungroup() %>%
  group_by(call_category) %>%
  nest() %>%
  expand_grid(q = c(0.5, 0.9)) %>%
  mutate(data = map2(data, q,
                     ~.x %>% group_by(n_dta) %>% summarise(response = quantile(response, .y))
  )) %>%
  unnest(cols = c(data)) %>%
  group_by(q, call_category) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(response ~ n_dta, data = .x))) %>%
  mutate(stats = map(mod, broom::tidy)) %>%
  unnest(stats)

# call_category     q data              mod    term        estimate std.error statistic  p.value
# <chr>         <dbl> <list>            <list> <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#   1 cat3            0.5 <tibble [65 × 2]> <lm>   (Intercept)  -30.3    23.9        -1.27  2.10e- 1
# 2 cat3            0.5 <tibble [65 × 2]> <lm>   n_dta          6.01    0.641       9.37  1.47e-13
# 3 cat3            0.9 <tibble [65 × 2]> <lm>   (Intercept)  179.     30.6         5.86  1.87e- 7
# 4 cat3            0.9 <tibble [65 × 2]> <lm>   n_dta         14.4     0.820      17.5   6.98e-26
# 5 cat2            0.5 <tibble [65 × 2]> <lm>   (Intercept)  -31.8    14.2        -2.23  2.92e- 2
# 6 cat2            0.5 <tibble [65 × 2]> <lm>   n_dta          3.78    0.381       9.92  1.72e-14
# 7 cat2            0.9 <tibble [65 × 2]> <lm>   (Intercept)  -20.9    22.9        -0.911 3.66e- 1
# 8 cat2            0.9 <tibble [65 × 2]> <lm>   n_dta         10.3     0.614      16.8   6.03e-25
# 9 cat1            0.5 <tibble [65 × 2]> <lm>   (Intercept)    7.12    0.151      47.0   8.82e-51
# 10 cat1            0.5 <tibble [65 × 2]> <lm>   n_dta          0.103   0.00406    25.3   9.86e-35
# 11 cat1            0.9 <tibble [65 × 2]> <lm>   (Intercept)   16.1     0.403      40.0   1.78e-46
# 12 cat1            0.9 <tibble [65 × 2]> <lm>   n_dta          0.191   0.0108     17.7   3.85e-26

# adv model



resp_mods <- dta_df %>% 
  left_join(handover_df) %>% 
  left_join(ed_df) %>%
  left_join(resp_time_df) %>%
  # left_join(resp_time_df) %>%
  mutate(n_ed = n_ed - n_dta) %>%
  mutate(age = factor(age)) %>%
  mutate(age = fct_recode(age, 
      "0-9 Years Old" = "0-4 Years Old",
      "0-9 Years Old" = "5-9 Years Old",
      "10-19 Years Old" = "10-14 Years Old",
      "10-19 Years Old" = "15-19 Years Old",
      "20-29 Years Old" = "20-24 Years Old",
      "20-29 Years Old" = "25-29 Years Old",
      "30-39 Years Old" = "30-34 Years Old",
      "30-39 Years Old" = "35-39 Years Old",
      "40-49 Years Old" = "40-44 Years Old",
      "40-49 Years Old" = "45-49 Years Old",
      "50-59 Years Old" = "50-54 Years Old",
      "50-59 Years Old" = "55-59 Years Old",
      "60-69 Years Old" = "60-64 Years Old",
      "60-69 Years Old" = "65-69 Years Old",
      "70-79 Years Old" = "70-74 Years Old",
      "70-79 Years Old" = "75-79 Years Old",
      "80-89 Years Old" = "80-84 Years Old",
      "80-89 Years Old" = "85-89 Years Old",
      "90-99 Years Old" = "90-94 Years Old",
      "90-99 Years Old" = "95-99 Years Old"
  )) %>%
  mutate(age = fct_relevel(age, "100 Years Old Or Over", after = 10)) %>%
  ungroup() %>%
  na.omit() %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  filter(handover_time > 0) %>%
  mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "Yes", "No")),
         hour = factor(cut(hour(dt), breaks = seq(0, 24, 6), include.lowest = TRUE), ordered = FALSE)) %>% 
  mutate(hour = fct_relevel(hour, "(12,18]", after = 0)) %>%
  mutate(hour = fct_recode(hour, "00:00-06:00" = "[0,6]",
                           "06:00-12:00" = "(6,12]",
                           "12:00-18:00" = "(12,18]",
                           "18:00-24:00" = "(18,24]")) %>%
  select(-site, -handover_time, -dt) %>%
  ungroup() %>%
  group_by(call_category) %>%
  nest() %>%
  expand_grid(q = c(0.5, 0.9)) %>%
  mutate(data = map2(data, q,
                     ~.x %>% group_by(across(c(-response))) %>% summarise(response = quantile(response, .y))
  )) %>%
  unnest(cols = c(data)) %>%
  arrange(call_category, q) %>%
  group_by(q, call_category) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(response ~ . + n_dta*n_ed, data = .x)))  %>%
  mutate(call_category = glue::glue("{str_replace(call_category, 'cat', 'Category ')}")) %>%
  mutate(q = percent(q)) %>%
  mutate(q = recode(q, "50%" = "Median")) %>%
  mutate(label = glue::glue("{q} {call_category} response"))

saveRDS(resp_mods, "data/resp_mods.RDS")
resp_mods <- readRDS("data/resp_mods.RDS")

resp_mods %>%
  pull(mod) %>%
  jtools::export_summs(
    coefs = c(
      "(Intercept)",
      "DTA Queue" = "n_dta",
      "Patients in ED (non-DTA)" = "n_ed",
      "Weekend" = "wkendYes",
      "00:00-06:00" = "hour00:00-06:00",
      "06:00-12:00" = "hour06:00-12:00",
      "18:00-24:00" = "hour18:00-24:00",
      "Interaction: DTA queue-Patients in ED" = "n_dta:n_ed",
            "10-19 Years Old" ="age10-19 Years Old",
            "10-19 Years Old" ="age10-19 Years Old",
            "20-29 Years Old" ="age20-29 Years Old",
            "20-29 Years Old" ="age20-29 Years Old",
            "30-39 Years Old" ="age30-39 Years Old",
            "30-39 Years Old" ="age30-39 Years Old",
            "40-49 Years Old" ="age40-49 Years Old",
            "40-49 Years Old" ="age40-49 Years Old",
            "50-59 Years Old" ="age50-59 Years Old",
            "50-59 Years Old" ="age50-59 Years Old",
            "60-69 Years Old" ="age60-69 Years Old",
            "60-69 Years Old" ="age60-69 Years Old",
            "70-79 Years Old" ="age70-79 Years Old",
            "70-79 Years Old" ="age70-79 Years Old",
            "80-89 Years Old" ="age80-89 Years Old",
            "80-89 Years Old" ="age80-89 Years Old",
            "90-99 Years Old" ="age90-99 Years Old",
            "90-99 Years Old" ="age90-99 Years Old"
    ),
    
  error_pos = "right",
  model.names = resp_mods %>%
    pull(label)
  ) %>%
  huxtable::quick_xlsx(file = "materials/resp_reg.xlsx")
  # mutate(stats = map(mod, broom::tidy)) %>%
  # unnest(stats) %>%
  # filter(term == "n_dta")


tbl1 <- gtsummary::tbl_regression(resp_mods$mod[[1]],
                          label = list(
                            age = "Age",
                            n_dta = "Queue size for admission",
                            n_ed = "Number in ED (excluding DTA)",
                            wkend = "Weekend",
                            hour = "Time of day"
                            )) %>%
  gtsummary::add_glance_table(include = c(nobs, r.squared)) 

tbl2 <- gtsummary::tbl_regression(resp_mods$mod[[2]],
                          label = list(
                            age = "Age",
                            n_dta = "Queue size for admission",
                            n_ed = "Number in ED (excluding DTA)",
                            wkend = "Weekend",
                            hour = "Time of day"
                            )) %>%
  gtsummary::add_glance_table(include = c(nobs, r.squared)) 


tab_out <- resp_mods %>%
  ungroup() %>%
  select(call_category, mod) %>%
  mutate(tab = map(mod, ~{
    gtsummary::tbl_regression(.x,
                              label = list(
                                age = "Age",
                                n_dta = "Queue size for admission",
                                n_ed = "Number in ED (excluding DTA)",
                                wkend = "Weekend",
                                hour = "Time of day"
                              )) %>%
      gtsummary::add_glance_table(include = c(nobs, r.squared))
    })) %>%
  select(call_category, tab) %>%
  group_by(call_category) %>%
  nest() %>%
  mutate(tab = map2(call_category, data, ~{
    gtsummary::tbl_merge(.y$tab,
                         tab_spanner = c("**Median response time (min)**", "**90% quantile response time (min)**")) |>
      gtsummary::as_gt() %>%
      gt::tab_header(
        title = gt::md(glue::glue('**{.x} ambulance response time**'))#,
        # subtitle = "These are some fine automobiles"
      )
    })) %>%
  ungroup() %>%
   select(call_category, tab) %>%
   as.list() 
   
   
   walk2(tab_out$call_category, tab_out$tab, function(name, tab) gt::gtsave(tab, glue::glue("materials/resp_{name}_out.html")))



gtsummary::tbl_merge(list(tbl1, tbl2),
                     tab_spanner = c("**Median response (min)**", "**90% quantile response (min)**")) |>
  gtsummary::as_gt() %>%
  gt::tab_header(
    title = gt::md("**Category 1 ambulance response**")#,
    # subtitle = "These are some fine automobiles"
  )



# %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave("materials/gt_out.html")


  # gtsummary::as_hux_table() %>%
  # huxtable::quick_xlsx(file = "materials/resp_reg.xlsx")

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

