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

# Site normalisation data
site_resus_maj <- tibble(site = c("BRISTOL ROYAL INFIRMARY", "WESTON GENERAL HOSPITAL", "SOUTHMEAD HOSPITAL"), maj = c(16, 9, 14), resus = c(8, 4, 6)) %>%
  mutate(total = maj + resus)

# sequence of time snapshots (i.e. when we measure DTA queue)
snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                           to = as.POSIXct(lubridate::ymd(report_end)),
                           by = glue::glue("{n_hour} hours"))

# sequence of time intervals (i.e. over which we measure mean handover delay)
snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
                                                       .x + dminutes((60*n_hour)/2)))


sex_df <- readRDS("data/dat_attr_demog.RDS") %>%
  select(nhs_number, sex) %>%
  distinct()

imd_df <- local({
  
  string_ecds<-glue::glue("select Arrival_Date,
                        Arrival_Time,
                        NHS_Number,
                        Index_Of_Multiple_Deprivation
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{report_start}' and '{report_end}'
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  dat_ecds_raw %>%
    group_by(nhs_number = NHS_Number) %>%
    arrange(desc(Arrival_Time)) %>%
    slice(1) %>%
    mutate(nhs_number = as.character(nhs_number)) %>%
    select(nhs_number, imd = Index_Of_Multiple_Deprivation)
}) %>%
  na.omit()

# using sus
{
# dta_df <- local({
#   
#   string_ecds<-glue::glue("select 
#                         nhs_number,
#                         Treatment_Function_Code
#                         Arrival_Date,
#                         Arrival_Time,
#                         Initial_Assessment_Date,
#                         Initial_Assessment_Time,
#                         Seen_For_Treatment_Date,
#                         Seen_For_Treatment_Time,
#                         Decision_To_Admit_Date,
#                         Decision_To_Admit_Time,
#                         Organisation_Code_Commissioner,
#                         Departure_Date,
#                         Departure_Time,
#                         Site_Name,
#                         Acuity_Desc
#       FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
#       where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
#       and Arrival_Date between '{report_start}' and '{report_end}'
#       --and destination_desc = 'Discharge to ward'
#       and destination_desc not in ('Emergency department discharge to emergency department short stay ward',
#       'Emergency department discharge to same day emergency care service',
#       'Emergency department discharge to ambulatory emergency care service',
#       'Emergency department discharge to neonatal intensive care unit',
#       'Emergency department discharge to special care baby unit',
#       'Admission to the mortuary',
#       'Discharge to home',
#       'Discharge to hospital at home service',
#       'Discharge to nursing home',
#       'Discharge to residential home',
#       'Discharge to police custody',
#       'Patient discharge, to legal custody',
#       'Urgent admission to hospice'
#       )
#       ")
#   dat_ecds_raw<-sqlQuery(con,string_ecds)
#   
#   dat_ecds<-dat_ecds_raw %>%
#     mutate(nhs_number = as.character(nhs_number)) %>%
#     mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
#     mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
#     mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
#     select(nhs_number, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
#     rowwise() %>%
#     mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
#     ungroup() %>%
#     mutate(dta_int = interval(dta_dt, departure_dt)) %>%
#     mutate(dta_time = dta_int/dminutes(1)) %>%
#     mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
#     mutate(date=floor_date(departure_dt,"day")) %>%
#     mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
#                                  TRUE~"major")) %>%
#     mutate(arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt)) %>%
#     filter(Organisation_Code_Commissioner == '15C')
#   
#   
#   dat_apc <- readRDS("data/dat_apc_new.RDS")
#   
#   
#   arrivals <- 
#     dat_ecds %>%
#     filter(!is.na(dta_time),
#            !is.na(nhs_number)) %>%
#     group_by(nhs_number, arr_id) %>%
#     summarise(departure_dt = as.Date(departure_dt)[1]
#     )
#   
#   dat_spec <- arrivals %>%
#     left_join(
#       dat_apc %>%
#         select(AIMTC_Pseudo_NHS, MainSpecialtyCode, StartDate_HospitalProviderSpell) %>%
#         mutate(AIMTC_Pseudo_NHS = as.character(AIMTC_Pseudo_NHS)) %>%
#         filter(!is.na(AIMTC_Pseudo_NHS)),
#       by = join_by(nhs_number == AIMTC_Pseudo_NHS)) %>% 
#     mutate(diff = abs(interval(departure_dt, StartDate_HospitalProviderSpell)/ddays(1))) %>%
#     filter(!is.na(diff)) %>%
#     group_by(arr_id) %>%
#     filter(between(diff, 0, 2)) %>%
#     filter(diff == min(diff)) %>% 
#     arrange(diff) %>%
#     slice(1) %>%
#     ungroup() %>%
#     select(nhs_number, arr_id, MainSpecialtyCode) 
#   
#   # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
#   # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
#   
#   dat_dtas <- dat_ecds %>%
#     left_join(dat_spec) %>%
#     filter(dta_time > 0, MainSpecialtyCode %in% c(300, 326, 430)) %>%
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
}
# using TFC
dta_df <- local({
  
  string_ecds<-glue::glue("select 
                        nhs_number,
                        Treatment_Function_Code,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Organisation_Code_Commissioner,
                        Departure_Date,
                        Departure_Time,
                        Site_Name,
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and destination_desc not in ('Emergency department discharge to emergency department short stay ward',
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
  
  dat_ecds<-dat_ecds_raw %>%
    mutate(nhs_number = as.character(nhs_number)) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(nhs_number, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
    rowwise() %>%
    mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dta_int = interval(dta_dt, departure_dt)) %>%
    mutate(dta_time = dta_int/dminutes(1)) %>%
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    mutate(arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt)) 
  
  
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
  
  string_ecds<-glue::glue("select 
                        nhs_number,
                        Arrival_Date,
                        Arrival_Time,
                        Index_of_multiple_deprivation,
                        Hes_age_at_arrival,
                        Stated_Gender,
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
    filter(!is.na(Departure_Date)) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(nhs_number, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc, sex = Stated_Gender, imd = Index_of_multiple_deprivation, age = Hes_age_at_arrival) %>%
    rowwise() %>%
    mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(departure_int = interval(arrival_dt, departure_dt)/dminutes(1)) %>%
    mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
    mutate(date=floor_date(departure_dt,"day")) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major"))
  
  
  
  # max_dt <- max(dat_ecds$dta_dt, na.rm = TRUE)
  # dta_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)), to = max_dt, by = glue::glue("{n_hour} hours"))
  dat_trt <- dat_ecds %>%
    # take only non-admits
    filter(is.na(dta_dt), departure_int > 0) %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(departure_time = list(map2(
      snapshot_int,
      data,
      ~ filter(.y, arrival_dt %within% .x) %>%
        select(age = age,
               sex = sex,
               imd,
               departure_time = departure_int)
    )))  %>%
    mutate(departure_df = map(departure_time, function(departure_time)
      map2_df(departure_time, snapshot_seq, ~ bind_cols(tibble(dt = .y), .x)))) %>%
    select(site, departure_df) %>%
    unnest(departure_df) %>%
    arrange(dt) 
  
  
  dat_trt
})

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
    ,c.[Patient_Gender]
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
    mutate(handover_time = lubridate::parse_date_time(Time_of_Handover_at_First_Hospital_Attended, orders = "ymd HMS")) %>%
    filter(handover_time <= report_end)
  
  
  handover_df <- actv_swafst %>%
    mutate(Derived_Pseudo_NHSNumber = as.character(Derived_Pseudo_NHSNumber)) %>%
    left_join(imd_df, by = join_by(Derived_Pseudo_NHSNumber == nhs_number)) %>%
    left_join(sex_df, by = join_by(Derived_Pseudo_NHSNumber == nhs_number)) %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(handover_time = list(map2(
      snapshot_int,
      data,
      ~ filter(.y, handover_time %within% .x) %>%
        select(nhs_number = Derived_Pseudo_NHSNumber,
               age = Age_Bracket,
               sex,
               imd,
               handover_time = Handover_Duration__seconds_)
    ))) %>%
    mutate(handover_df = map(handover_time, function(handover_time)
      map2_df(handover_time, snapshot_seq, ~ bind_cols(tibble(dt = .y), .x)))) %>%
    select(site, handover_df) %>%
    unnest(handover_df) %>%
    arrange(dt) 
  
  handover_df %>%
    filter(sex %in% c("Female", "Male")) 
})


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
    ,c.[Patient_Gender]
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
LEFT JOIN [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_SWAST_AR119] c on a.Incident_Number=c.Call_Number
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
    mutate(nhs_number = as.character(Derived_Pseudo_NHSNumber)) %>%
    select(nhs_number, dt, age = Age_Bracket, outcome = Outcome, call_category, response) %>%
    left_join(imd_df) %>%
    left_join(sex_df)
  
  response_time 
})

# saveRDS(dta_df, "data/upstream_dta_df.RDS")
# saveRDS(imd_df, "data/upstream_imd_df.RDS")
# saveRDS(ed_df, "data/upstream_ed_df.RDS")
# saveRDS(trt_df, "data/upstream_trt_df.RDS")
# saveRDS(handover_df, "data/upstream_handover_df.RDS")
# saveRDS(resp_time_df, "data/upstream_resp_time_df.RDS")


dta_df <-  readRDS("data/upstream_dta_df.RDS")
imd_df <- readRDS("data/upstream_imd_df.RDS")
ed_df <-  readRDS("data/upstream_ed_df.RDS")
trt_df <-  readRDS("data/upstream_trt_df.RDS")
handover_df <-  readRDS("data/upstream_handover_df.RDS")
resp_time_df <-  readRDS("data/upstream_resp_time_df.RDS")


dta_df <- dta_df %>%
  filter(!(site == "WESTON GENERAL HOSPITAL" & hour(dt) %in% c(10:12, 1:7))) 

handover_df <- handover_df %>%
  na.omit() %>%
  ungroup() %>%
  mutate(imd = factor(imd, levels = 1:10, labels = c("1 (least deprived)",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "5",
                                                     "6",
                                                     "7",
                                                     "8",
                                                     "9",
                                                     "10 (most deprived)"))
  ) %>%
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)")) %>%
  mutate(imd = fct_relevel(imd, "(Missing)", after = Inf)) %>%
  mutate(age = fct_recode(age, 
                          "29 years old or under" = "0-4 Years Old",
                          "29 years old or under" = "5-9 Years Old",
                          "29 years old or under" = "10-14 Years Old",
                          "29 years old or under" = "15-19 Years Old",
                          "29 years old or under" = "20-24 Years Old",
                          "29 years old or under" = "25-29 Years Old",
                          "30-39 years old" = "30-34 Years Old",
                          "30-39 years old" = "35-39 Years Old",
                          "40-49 years old" = "40-44 Years Old",
                          "40-49 years old" = "45-49 Years Old",
                          "50-59 years old" = "50-54 Years Old",
                          "50-59 years old" = "55-59 Years Old",
                          "60-69 years old" = "60-64 Years Old",
                          "60-69 years old" = "65-69 Years Old",
                          "70-79 years old" = "70-74 Years Old",
                          "70-79 years old" = "75-79 Years Old",
                          "80-89 years old" = "80-84 Years Old",
                          "80-89 years old" = "85-89 Years Old",
                          "90-99 years old" = "90-94 Years Old",
                          "90-99 years old" = "95-99 Years Old",
                          "100 years old or over" = "100 Years Old Or Over"
  )) %>%
  mutate(age = fct_relevel(age, "100 years old or over", after = 10)) 

resp_time_df <- resp_time_df %>%
  na.omit() %>%
  filter(sex %in% c("Male", "Female")) %>%
  mutate(imd = factor(imd, levels = 1:10, labels = c("1 (least deprived)",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "5",
                                                     "6",
                                                     "7",
                                                     "8",
                                                     "9",
                                                     "10 (most deprived)"))
  ) %>%
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)")) %>%
  mutate(age = fct_recode(age, 
                          "29 years old or under" = "0-4 Years Old",
                          "29 years old or under" = "5-9 Years Old",
                          "29 years old or under" = "10-14 Years Old",
                          "29 years old or under" = "15-19 Years Old",
                          "29 years old or under" = "20-24 Years Old",
                          "29 years old or under" = "25-29 Years Old",
                          "30-39 years old" = "30-34 Years Old",
                          "30-39 years old" = "35-39 Years Old",
                          "40-49 years old" = "40-44 Years Old",
                          "40-49 years old" = "45-49 Years Old",
                          "50-59 years old" = "50-54 Years Old",
                          "50-59 years old" = "55-59 Years Old",
                          "60-69 years old" = "60-64 Years Old",
                          "60-69 years old" = "65-69 Years Old",
                          "70-79 years old" = "70-74 Years Old",
                          "70-79 years old" = "75-79 Years Old",
                          "80-89 years old" = "80-84 Years Old",
                          "80-89 years old" = "85-89 Years Old",
                          "90-99 years old" = "90-94 Years Old",
                          "90-99 years old" = "95-99 Years Old",
                          "100 years old or over" = "100 Years Old Or Over"
  )) %>%
  mutate(age = fct_relevel(age, "100 years old or over", after = 10)) 


trt_df <- trt_df %>%
  na.omit() %>%
  mutate(imd = factor(
  imd,
  levels = 1:10,
  labels = c(
    "1 (least deprived)",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10 (most deprived)"
  )
)) %>%
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)"))  %>%
  mutate(sex = factor(ifelse(sex == "1", "Male", "Female"))) %>%
  filter(age < 120) %>%
  mutate(age = cut(age, breaks = c(0, seq(30, 100, 10), Inf), right = FALSE, labels = c(
                   "29 years old or under" ,
                   "30-39 years old" ,
                   "40-49 years old" ,
                   "50-59 years old" ,
                   "60-69 years old" ,
                   "70-79 years old" ,
                   "80-89 years old" ,
                   "90-99 years old" ,
                   "100 years old or over" 
                   )))
  


# Normalisation and standardisation

n_dta_cutoff <- 1.1

handover_outputs <- local({
  
  handover_df <- handover_df %>% select(-nhs_number)
  
  bin_acc <- 0.1
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Standardised ambulance handover\ntime (min)"

  # Non standardised plot
  p_raw <-   dta_df %>%
    left_join(handover_df) %>%
    left_join(site_resus_maj) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    mutate(n_dta_norm = n_dta/total,
           standard = 30) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    # distinct() %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    ggplot(aes(x = n_dta_norm, y = handover_time/60)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
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
    labs(title = "",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))
  

  
# Standardised plot  
mod_dat <- dta_df %>% 
  left_join(handover_df) %>%
  left_join(site_resus_maj) %>%
  left_join(ed_df) %>%
  mutate(n_ed_norm = (n_ed - n_dta)/total,
         n_dta_norm = n_dta / total) %>%
  mutate(n_dta_norm = n_dta/total) %>%
    mutate(age = factor(age)) %>%
    ungroup() %>%
    na.omit() %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    filter(handover_time > 0) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
  mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                           "06:00-11:59" = "06:00:00",
                           "12:00-17:59" = "12:00:00",
                           "18:00-23:59" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
  # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt, -maj, -resus, -total) %>%
    ungroup() 

handover_mod <- lm(handover_time ~ . -n_dta -n_dta_norm -n_ed -site, data = mod_dat)
handover_pred <- predict(handover_mod, mod_dat)

std_mod <- handover_mod

std_mod_tab <- gtsummary::tbl_regression(
  std_mod,
  label = list(
    age = "Age",
    imd = "Index of multiple deprivation decile",
    wkend = "Weekend",
    hour = "Time of arrival",
    arrival_month = "Month of arrival"#,
    # arrival_year = "Year of Arrival"
  )
) %>%
  gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Ambulance Handover Time (min)**')))#,

full_mod <- lm(handover_time/60 ~ age + sex + imd + n_ed_norm + n_dta_norm + wkend + hour + arrival_month -site, data = mod_dat)

# This code assesses which transform is best?


full_mod_tab <-  full_mod %>%
  gtsummary::tbl_regression(
    label = list(
      n_dta_norm = x_lab,
      n_ed_norm = "Number in ED (excluding DTA) as a percentage of cubicle capacity (Majors + Resus)",
      sex = "Sex",
      age = "Age",
      imd = "Index of multiple deprivation decile",
      wkend = "Weekend",
      hour = "Time of arrival",
      arrival_month = "Month of arrival"#,
      # arrival_year = "Year of Arrival"
    )
  ) %>%
  gtsummary::add_glance_table(include = c(nobs, r.squared)) # |>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Ambulance handover time (min)**')))



mod_dat <- mod_dat %>%
  mutate(handover_standard = handover_pred,
         handover_adj = handover_time - handover_standard,
         handover_adj_inv = handover_adj + mean(handover_time)) 

plot_df <- mod_dat %>%
  # filter(n_dta <= 35) 
  filter(n_dta_norm <= n_dta_cutoff)


p_std <- plot_df %>%
  # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
  mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
  mutate(standard = 30) %>%
  filter(n_dta_norm >= 0.1) %>%
  ggplot(aes(
    x = n_dta_norm,
    y = handover_adj_inv / 60,
    group = cut_width(n_dta_norm, width = bin_acc)
  )) +
  # geom_boxplot(outliers = FALSE) +
  geom_boxplot(outliers = FALSE, fatten = NULL) +
  # scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
  # geom_point(aes(y = standard), col = "red") +
  geom_hline(aes(yintercept = standard),
             col = "red",
             linetype = 2) +
  geom_point(aes(y = standard, col = "National target")) +
  stat_summary(
    fun = ~ mean(.x),
    geom = "point",
    col = "black",
    aes(col = "Mean (standardised data)", shape = "Mean (standardised data)")
  ) +
  stat_summary(
    fun = ~ quantile(.x, 0.95),
    geom = "point",
    col = "black",
    aes(col = "Comparator quantile (standardised data)", shape = "Comparator quantile (standardised data)")
  ) +
  # bnssgtheme() +
  theme_minimal() +
  scale_colour_manual(
    # breaks = c("Mean", "Comparator quantile (standardised data)", "National target"),
    values = c("Mean" = "black", "Comparator quantile (standardised data)"= "black", "National target" = "red"),
    # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  scale_x_continuous(labels = scales::percent,
                     breaks = scales::breaks_width(0.5)) +
  coord_cartesian(xlim = c(0.1, NA)) +
  scale_shape_manual(values = c(17, 19),
                     breaks = c("Mean (standardised data)", "Comparator quantile (standardised data)"),
                     # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
                     ) +
  labs(title = "", 
       y = y_lab,
       x = x_lab,
       shape = "",
       colour = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.spacing.x = unit(0, "cm")) +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2))



list(
  p_raw = p_raw,
  p_std = p_std,
  mod_dat = mod_dat,
  plot_df = plot_df,
  std_mod = std_mod,
  full_mod = full_mod,
  std_mod_tab = std_mod_tab,
  full_mod_tab = full_mod_tab
)

})

saveRDS(handover_outputs, "data/handover_outputs.RDS")

resp_outputs <- local({
  
  resp_time_df <- resp_time_df %>% select(-nhs_number)
  
  bin_acc <- 0.1
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Standardised ambulance response time\n(min)"
  
  labels <- c(cat1 = "999 Call Category 1",
              cat2 = "999 Call Category 2",
              cat3 = "999 Call Category 3")
  
  time_std <- tibble(call_category = c("cat1", "cat2", "cat3"),
                     std = c(15, 40, 120))
  
  # non normalised x-axis
  p_raw_raw <- dta_df %>%
    left_join(resp_time_df) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    filter(n_dta <= 35) %>%
    distinct() %>%
    # mutate(n_dta = factor(n_dta)) %>%
    ggplot(aes(x = n_dta, y = response)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta, width = 1)), outliers = FALSE, fatten = NULL) +
    geom_point(
      data = {time_std %>% expand_grid(n_dta = 0:35)},
      aes(y = std, col = "National target"),
    ) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(col = "Mean", shape = "Mean")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.9),
      geom = "point",
      col = "black",
      aes(col = "Mean", shape = "90th percentile")
    ) +
    facet_wrap(
      vars(call_category),
      ncol = 1,
      scales = "free",
      labeller = as_labeller(labels)
    ) +
    # scale_x_continuous(limits = c(0, 35)) +
    theme_minimal() +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    guides(alpha = "none") +
    labs(
      title = "Raw outcome, un-normalised dependant",
      y = y_lab,
      x = "DTA queue",
      shape = "",
      col = ""
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.spacing.x = unit(0, "cm")
    )
  
  p_norm_raw <- dta_df %>%
    ungroup() %>%
    # filter(n_dta <= 35) %>%
    # na.omit() %>%
    left_join(site_resus_maj) %>%
    mutate(n_dta_norm = n_dta / total) %>%
    group_by(dt) %>%
    summarise(n_dta_norm = mean(n_dta_norm, na.rm = TRUE)) %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    filter(n_dta_norm > 0, n_dta_norm <= n_dta_cutoff) %>%
    left_join(resp_time_df)  %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    distinct() %>%
    # mutate(n_dta = factor(n_dta)) %>%
    ggplot(aes(x = n_dta_norm, y = response)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(
      mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)),
      outliers = FALSE,
      fatten = NULL
    ) +
    geom_point(
      data = {time_std %>% expand_grid(n_dta_norm = seq(0, 1.75, bin_acc))},
      aes(y = std, col = "National target"),
    ) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(shape = "Mean")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.9),
      geom = "point",
      col = "black",
      aes(shape = "90th percentile")
    ) +
    facet_wrap(
      vars(call_category),
      ncol = 1,
      scales = "free",
      labeller = as_labeller(labels)
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal()  +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    guides(alpha = "none") +
    labs(
      title = "Raw outcome, normalised dependant",
      y = y_lab,
      x = x_lab,
      shape = "",
      col = ""
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text()
    )
  
  
  # y standardisation
  
  mod_dat <- dta_df %>%
    ungroup() %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    left_join(site_resus_maj) %>%
    left_join(ed_df) %>%
    # filter(n_dta <= 35) %>%
    mutate(n_ed_norm = (n_ed - n_dta)/total,
           n_dta_norm = n_dta / total
    ) %>%
    group_by(dt) %>%
    summarise(
      n_dta_norm = mean(n_dta_norm, na.rm = TRUE),
      n_ed_norm = mean(n_ed_norm, na.rm = TRUE),
    ) %>%
    left_join(resp_time_df) %>%
    na.omit() %>%
    filter(n_dta_norm > 0, n_dta_norm <= n_dta_cutoff) %>%
    distinct() %>%
    ungroup() %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
    mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                             "06:00-11:59" = "06:00:00",
                             "12:00-17:59" = "12:00:00",
                             "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
    # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt) %>%
    ungroup()
  
  resp_mods <- 
    mod_dat %>%
    group_by(call_category) %>%
    nest() %>%
    mutate(mod = map(data, ~ lm(response ~ . -n_dta_norm -outcome,  data = .x))) %>%
    mutate(mod_full = map(data, ~ lm(response ~ . -outcome, data = .x))) %>%
    mutate(pred = map2(mod, data, predict)) %>%
    mutate(data = pmap(list(data, call_category, pred), ~bind_cols(..1, call_category = ..2, response_pred = ..3)))
  
  # resp_mod <- lm(response ~ . - n_dta_norm - outcome, data = mod_dat)
  # resp_pred <- predict(resp_mod, mod_dat)
  
  
  std_mod_tabs <- resp_mods %>%
    ungroup() %>%
    mutate(call_category = recode(call_category, cat1 = "Category 1", cat2 = "Category 2", cat3 = "Category 3")) %>%
    select(call_category, mod) %>%
    mutate(tab = map2(mod, call_category, ~{
      gtsummary::tbl_regression(.x,
                                label = list(
                                  age = "Age",
                                  sex = "Sex",
                                  imd = "Index of multiple deprivation decile",
                                  wkend = "Weekend",
                                  hour = "Time of arrival",
                                  arrival_month = "Month of arrival"
                                )) %>%
        gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
        # gtsummary::as_gt() %>%
        # gt::tab_header(title = gt::md(glue::glue('**{.y} Ambulance response time (min)**')))
    })) %>%
    ungroup() %>%
    select(call_category, tab) %>%
    as.list() 
  
  full_mod_tabs <- resp_mods %>%
    ungroup() %>%
    mutate(call_category = recode(call_category, cat1 = "Category 1", cat2 = "Category 2", cat3 = "Category 3")) %>%
    select(call_category, mod_full) %>%
    mutate(tab = map2(mod_full, call_category, ~{
      gtsummary::tbl_regression(.x,
                                label = list(
                                  n_dta_norm = x_lab,
                                  n_ed_norm = "Number in ED (excluding DTA) as a percentage of cubicle capacity (Majors + Resus)",
                                  age = "Age",
                                  sex = "Sex",
                                  imd = "Index of multiple deprivation decile",
                                  wkend = "Weekend",
                                  hour = "Time of arrival",
                                  arrival_month = "Month of arrival"
                                )) %>%
        gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
        # gtsummary::as_gt() %>%
        # gt::tab_header(title = gt::md(glue::glue('**{.y} Ambulance response time (min)**')))
    })) %>%
    ungroup() %>%
    select(call_category, tab) %>%
    as.list() 
  
  
  plot_dat <- resp_mods %>% 
    pull(data) %>%
    bind_rows() %>%
    mutate(response_adj = response - response_pred) %>%
    mutate(response_adj_inv = response_adj + mean(response), .by = call_category)
  
  p_norm_std <- plot_dat %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    filter(n_dta_norm >= 0.1) %>%
    group_by(call_category) %>%
    nest() %>%
    mutate(plot = map2(data, call_category, \(x, y) {x %>%
    ggplot(aes(x = n_dta_norm, y = response_adj_inv)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(
      mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)),
      outliers = FALSE,
      fatten = NULL
    ) +
    geom_hline(data = filter(time_std, call_category == y), aes(yintercept = std), linetype = 2, col = "red") +
    # stat_summary(
    #   fun = ~ mean(.x),
    #   geom = "errorbar",
    #   col = "black",
    #   width = 0.035,
    #   aes(ymax = ..y.., ymin = ..y..)
    # ) +
    geom_point(
      data = {filter(time_std, call_category == y) %>% expand_grid(n_dta_norm = seq(bin_acc, n_dta_cutoff, bin_acc))},
      aes(y = std, col = "National target"),
    ) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(shape = "Mean (standardised data)")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.9),
      geom = "point",
      col = "black",
      aes(shape = "Comparator quantile (standardised data)")
    ) +
    # facet_wrap(
    #   vars(call_category),
    #   ncol = 1,
    #   scales = "free",
    #   labeller = as_labeller(labels)
    # ) +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_width(0.5)) +
    coord_cartesian(xlim = c(0.1, NA)) +
    theme_minimal()  +
    scale_colour_manual(values = c("red", "black"),
                        # guide = guide_legend(override.aes = list(size = 3,alpha = 1))
                        ) +
    scale_shape_manual(breaks = c("Mean (standardised data)", "Comparator quantile (standardised data)"),
                       values = c(17, 19),
                       # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
                       ) +
    facet_grid(str_c("999 Category ", str_replace(y, "cat", ""), " call")~.) +
    guides(alpha = "none") +
    labs(
      title = "",
      y = y_lab,
      x = x_lab,
      shape = "",
      col = ""
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.text = element_text(size = 11),
      legend.title = element_blank()
    )
    })) %>%
    pull(plot)
  
  
  list(
    p_raw_raw = p_raw_raw,
    p_norm_raw = p_norm_raw,
    p_norm_std = p_norm_std,
    plot_dat = plot_dat,
    std_mod = set_names(resp_mods$mod, resp_mods$call_category),
    full_mod = set_names(resp_mods$mod_full, resp_mods$call_category),
    mod_dat = mod_dat,
    std_mod_tabs = std_mod_tabs,
    full_mod_tabs = full_mod_tabs
  )
  
})

saveRDS(resp_outputs, "data/resp_outputs.RDS")

time_ed_outputs <- local({
  
  bin_acc <- 0.1
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Standardised time in ED \n(min)"
  
  # Non standardised plot
  p_raw <-   dta_df %>%
    left_join(trt_df) %>%
    left_join(site_resus_maj) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    mutate(n_dta_norm = n_dta/total,
           standard = 60) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    # distinct() %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    ggplot(aes(x = n_dta_norm, y = departure_time)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    scale_y_continuous(breaks = breaks_width(60)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
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
    labs(title = "Raw outcome, normalised dependant",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))
  
  
  # Standardised plot  
  mod_dat <- dta_df %>% 
    left_join(trt_df) %>%
    left_join(site_resus_maj) %>%
    left_join(ed_df) %>%
    mutate(n_ed_norm = (n_ed - n_dta)/total,
           n_dta_norm = n_dta / total) %>%
    mutate(n_dta_norm = n_dta/total) %>%
    ungroup() %>%
    na.omit() %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    filter(departure_time >= 0) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
    mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                             "06:00-11:59" = "06:00:00",
                             "12:00-17:59" = "12:00:00",
                             "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
    # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt, -maj, -resus, -total) %>%
    ungroup() 
  
  trt_mod <- lm(departure_time ~ . -n_dta -n_dta_norm -n_ed -site, data = mod_dat)
  trt_pred <- predict(trt_mod, mod_dat)
  
  std_mod <- trt_mod
  
  std_mod_tab <- gtsummary::tbl_regression(
    std_mod,
    label = list(
      age = "Age",
      sex = "Sex",
      imd = "Index of multiple deprivation decile",
      wkend = "Weekend",
      hour = "Time of arrival",
      arrival_month = "Month of arrival"#,
      # arrival_year = "Year of Arrival"
    )
  ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
    # gtsummary::as_gt() %>%
    # gt::tab_header(title = gt::md(glue::glue('**Time in ED (min)**')))#,
  
  full_mod <- lm(departure_time ~ age + sex + imd + n_ed_norm + n_dta_norm + wkend + hour + arrival_month -site, data = mod_dat)
  
  # This code assesses which transform is best?
  
  
  
  full_mod_tab <-  full_mod %>%
    gtsummary::tbl_regression(
      label = list(
        n_dta_norm = x_lab,
        n_ed_norm = "Number in ED (excluding DTA) as a percentage of cubicle capacity (Majors + Resus)",
        age = "Age",
        sex = "Sex",
        imd = "Index of multiple deprivation decile",
        wkend = "Weekend",
        hour = "Time of arrival",
        arrival_month = "Month of arrival"#,
        # arrival_year = "Year of Arrival"
      )
    ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
    # gtsummary::as_gt() %>%
    # gt::tab_header(title = gt::md(glue::glue('**Time in ED (min)**')))
  
  
  
  mod_dat <- mod_dat %>%
    mutate(trt_standard = trt_pred,
           trt_adj = departure_time - trt_standard,
           trt_adj_inv = trt_adj + mean(departure_time)) 
  
  plot_df <- mod_dat %>%
    # filter(n_dta <= 35) 
    filter(n_dta_norm <= n_dta_cutoff)
  
  
  
  p_std <- plot_df %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(standard = 4*60) %>%
    filter(n_dta_norm >= 0.1) %>%
    ggplot(aes(x = n_dta_norm, y = trt_adj_inv, group = cut_width(n_dta_norm, width = bin_acc))) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(outliers = FALSE, fatten = NULL) +
    # scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_point(aes(y = standard), col = "red") +
    geom_hline(aes(yintercept = standard), col = "red", linetype = 2) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_y_continuous(breaks = breaks_width(120)) +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_width(0.5)) +
    coord_cartesian(xlim = c(0.1, NA)) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    labs(title = "",
         y = y_lab,
         x = x_lab,
         shape = "",
         colour = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.spacing.x = unit(0, "cm"))
  
  
  
  list(
    p_raw = p_raw,
    p_std = p_std,
    plot_df = plot_df,
    full_mod = full_mod,
    mod_dat = mod_dat,
    std_mod = std_mod,
    std_mod_tab = std_mod_tab,
    full_mod_tab = full_mod_tab
  )
  
})

saveRDS(time_ed_outputs, "data/time_ed_outputs.RDS")

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
  mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
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

