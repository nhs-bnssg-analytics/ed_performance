require(tidyverse)
require(RODBC)
require(lubridate)
library(tidymodels)
library(patchwork)
library(ggtext)
library(marquee)

# upstream data
con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

report_start <- '2025-01-20'
report_end <- '2025-01-27'
# report_start <- '2024-01-01'
# report_end <- '2025-01-31'
n_hour <- 2
pre_window_days <- 60

bin_acc <- 0.01 # this is a legacy variable which is needed because it's baked into plot outputs (silly really..)
# Site normalisation data
site_resus_maj <- tibble(site = c("BRISTOL ROYAL INFIRMARY", "WESTON GENERAL HOSPITAL", "SOUTHMEAD HOSPITAL"),
                         maj = c(16, 9, 14),
                         resus = c(8, 4, 6)) %>%
  mutate(total = maj + resus)

# sequence of time snapshots (i.e. when we measure DTA queue)
snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                           to = as.POSIXct(lubridate::ymd(report_end)),
                           by = glue::glue("{n_hour} hours"))

# sequence of time intervals (i.e. over which we measure mean handover delay)
snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
                                                       .x + dminutes((60*n_hour)/2)))


## DOWNSTREAM IMPACTS

dat_downstream <- local({
  string_ecds<-glue::glue("select 
                        nhs_number,
                        Index_Of_Multiple_Deprivation,
                        Stated_Gender,
                        HES_Age_At_Arrival,
                        Attendance_Unique_Identifier,
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
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
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
    select(nhs_number,
           imd = Index_Of_Multiple_Deprivation,
           sex = Stated_Gender,
           age = HES_Age_At_Arrival,
           Attendance_Unique_Identifier,
           tfc = Treatment_Function_Code,
           Organisation_Code_Commissioner,
           arrival_dt,
           initial_dt,
           treatment_dt,
           dta_dt,
           departure_dt,
           site = Site_Name,
           Acuity_Desc) %>%
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
  
  string_ecds_full <-glue::glue("select 
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
                        Organisation_Code_Commissioner,
                        Arrival_Mode_Desc,
                        Arrival_Category_Desc,
                        Attendance_Source_Desc,
                        destination_desc,
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start) - ddays(pre_window_days)}' and '{report_end}'
      ")
  
  dat_ecds_full_raw<-sqlQuery(con,string_ecds_full)
  
  dat_ecds_full <- dat_ecds_full_raw %>%
    filter(Organisation_Code_Commissioner == '15C') %>% # filter for 15C as we can not pick up deaths for non BNSSG patients
    mutate(NHS_Number = as.character(NHS_Number)) %>%
    mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(
      nhs_number = NHS_Number,
      tfc = Treatment_Function_Code,
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
  
  # arrivals that become dta
  arrivals <- 
    dat_ecds %>%
    filter(!is.na(dta_time),
           arrival_dt >= report_start,
           # nhs_number %in% unique(dat_attr$nhs_number),
           !is.na(nhs_number)) %>%
    select(nhs_number, arr_id, tfc, arrival_dt, departure_dt) %>%
    distinct() %>%
    group_by(nhs_number, arr_id) %>%
    summarise(
      tfc = tfc[1],
      arrival_dt = arrival_dt, 
      departure_dt = as.Date(departure_dt)[1]
    ) %>%
    ungroup()
  
  dat_spec <- arrivals %>%
    select(nhs_number, arr_id, tfc) %>%
    mutate(spec = ifelse(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
    #mutate(MainSpecialtyCode = ifelse(MainSpecialtyCode %in% c(300, 326, 430), "medical", "not medical")) %>% 
    #mutate(spec = ifelse(tfc == "medical" | MainSpecialtyCode == "medical", "medical", "not medical")) %>%
    select(nhs_number, arr_id, spec)
  
  dat_ecds_perf <- glue::glue(
                        "select
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
                        Organisation_Code_Commissioner,
                        Arrival_Mode_Desc,
                        Arrival_Category_Desc,
                        Attendance_Source_Desc,
                        destination_desc,
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start)}' and '{report_end}'
      "
  ) %>%
    sqlQuery(con, .) %>% mutate(NHS_Number = as.character(NHS_Number)) %>%
    mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
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
      site = Site_Name
    ) %>%
    rowwise() %>%
    mutate(departure_dt=max(dta_dt,departure_dt, conclusion_dt, na.rm=TRUE)) %>%
    # mutate(departure_dt=max(departure_dt, conclusion_dt, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dta_time = interval(dta_dt, departure_dt)/dminutes(1)) %>%
    mutate(ed_time = interval(arrival_dt, departure_dt)/dminutes(1)) %>%
    mutate(breach = (interval(arrival_dt, departure_dt)/dminutes(1)) > 4*60) %>%
    mutate(arrival_hour = ceiling_date(arrival_dt, unit = "4 hour")) %>%
    filter(!between(arrival_hour, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    # Remove rows for weston out of hours
    filter(!(site == "WESTON GENERAL HOSPITAL" & hour(arrival_dt) %in% c(10:12, 1:7))) %>%
    group_by(site, arrival_hour) %>%
    summarise(perf = 1-mean(breach)) %>%
    ungroup()
  
  dat_dtas <- 
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
    slice_min(dta_dt) %>%
    group_by(nhs_number, dta_day) %>%
    mutate(id = cur_group_id()) %>%
    ungroup()
  
  dat_ed_attends <- 
    dat_ecds_full %>%
    select(nhs_number, pre_arrival_dt = arrival_dt) %>%
    distinct()
  
  dat_pre_attends <- dat_dtas %>%
    left_join(dat_ed_attends) %>%
    filter(pre_arrival_dt < arrival_dt) %>%
    filter(between(pre_arrival_dt, arrival_dt - ddays(pre_window_days), arrival_dt)) %>%
    group_by(id) %>%
    count(name = "n_ed")
  
  dat_ecds_n_ed <- dat_dtas %>%
    left_join(dat_pre_attends) %>%
    mutate(n_ed = coalesce(n_ed, 0)) %>%
    select(nhs_number, n_ed) %>%
    distinct()

  dat <- dat_ecds %>%
    left_join(dat_spec) %>%
    filter(spec == "medical", !is.na(dta_dt)) %>%
    mutate(dta_time = dta_time/60) %>% # convert minutes to hours
    # filter(ed_time < 60*30) %>%
    mutate(age = cut(age, breaks = c(0 , seq(30, 100, 10), Inf), right = FALSE, labels = c(
      "29 years old or under",
      # "20-29 years old" ,
      "30-39 years old" ,
      "40-49 years old" ,
      "50-59 years old" ,
      "60-69 years old" ,
      "70-79 years old" ,
      "80-89 years old" ,
      "90-99 years old" ,
      "100 years old or over" 
    ))
    )  %>%
    mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
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
    mutate(wkend = factor(ifelse(wday(arrival_dt, week_start = 1) < 6, "No", "Yes"))) %>%
    mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
    mutate(arrival_time = fct_recode(arrival_time,
                                     "00:00-05:59" = "00:00:00",
                                     "06:00-11:59" = "06:00:00",
                                     "12:00-17:59" = "12:00:00",
                                     "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month.name[month(arrival_dt)])) %>%
    mutate(arrival_month = fct_expand(arrival_month, month.name)) %>%
    # mutate(arrival_year = factor(year(arrival_dt))) %>% # we can't use this because 2025 hasn't been trained on
    mutate(arrival_year = factor(2024)) %>%
    mutate(arrival_hour = ceiling_date(arrival_dt, "4 hour")) %>% 
    left_join(dat_ecds_perf) %>%
    left_join(dat_ecds_n_ed) %>%
    select(
      nhs_number,
      wkend,
      arrival_time,
      arrival_month,
      arrival_year,
      perf,
      n_ed,
      age,
      sex,
      imd,
      dta_time,
      site
    )
  
  dat
  
  })

mort_mod <- readRDS("data/mort_effects_output.RDS") %>% `[[`("mort_mod_raw")
readm_mod <- readRDS("data/readm_effects_output.RDS") %>% `[[`("readm_mod_raw")
los_mod <- readRDS("data/los_effect_output.RDS") %>% `[[`("los_mod")

vw_impute_rec <- readRDS("data/vw_imput_rec.RDS")

dat_downstream_imputed <- vw_impute_rec %>%
  bake(new_data = mutate(dat_downstream, vw_score = NA_real_)) 


downstream_impacts <- dat_downstream %>%
  nest(.by = site) %>%
  mutate(data = map(data, \(d) bake(vw_impute_rec, new_data = mutate(d, vw_score = NA_real_)))) %>%
  mutate(pred_mort = map(data, \(d) predict(mort_mod, newdata = d, type = "response"))) %>%
  mutate(pred_mort_cf = map(data, \(d) predict(mort_mod, newdata = mutate(d, dta_time = 0), type = "response"))) %>%
  mutate(pred_readm = map(data, \(d) predict(readm_mod, newdata = d, type = "response"))) %>%
  mutate(pred_readm_cf = map(data, \(d) predict(readm_mod, newdata = mutate(d, dta_time = 0), type = "response"))) %>%
  mutate(om6_impact = map2_dbl(pred_mort, pred_mort_cf, ~sum(.x-.y, na.rm = TRUE))) %>%
  mutate(om5_impact = map2_dbl(pred_readm, pred_readm_cf, ~sum(.x-.y, na.rm = TRUE))) %>%
  mutate(om4_impact = map_dbl(data, \(d) los_mod %>% coefficients %>% pluck("dta_time") %>% `*`(d$dta_time/60) %>% sum)) %>%
  select(grp = site, om6_impact, om5_impact, om4_impact) %>%
  pivot_longer(-grp, names_to = "impact") %>%
  mutate(impact = str_remove(impact, "_impact"),
         label = case_when(
           impact == "om4" ~ "bed days in the acutes",
           impact == "om5" ~ "readmissions in the next 30 days",
           impact == "om6" ~ "deaths in the next 30 days"
           ),
         colour = case_when(
           impact == "om4" ~ "#006d77",
           impact == "om5" ~ "#f07d24",
           impact == "om6" ~ "#ef5546"
           )
         )




## UPSTREAM IMPACTS

handover_coef <- readRDS("data/avoidable_impacts_handover_coef.RDS")
ed_time_coef <- readRDS("data/avoidable_impacts_ed_time_coef.RDS")
resp_coef <- readRDS("data/avoidable_impacts_resp_coef.RDS")


handover_arrivals <- local({
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
    group_by(site = Site_Name, Outcome) %>%
    nest() %>%
    mutate(handover_time = list(map2(
      snapshot_int,
      data,
      ~ filter(.y, handover_time %within% .x) %>%
        select(nhs_number = Derived_Pseudo_NHSNumber,
               handover_time = Handover_Duration__seconds_)
    ))) %>%
    mutate(handover_df = map(handover_time, function(handover_time)
      map2_df(handover_time, snapshot_seq, ~ bind_cols(tibble(dt = .y), .x)))) %>%
    select(site, handover_df) %>%
    unnest(handover_df) %>%
    arrange(dt) %>%
    group_by(site, dt) %>%
    count() %>%
    ungroup()
    
})

dta_dats <- local({
  string_ecds<-glue::glue("select 
                        nhs_number,
                        Attendance_Unique_Identifier,
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
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
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
    select(nhs_number, Attendance_Unique_Identifier,  tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
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
  
  
  string_ecds_full<-glue::glue("select 
                        nhs_number,
                        Attendance_Unique_Identifier,
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
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      ")
  
  dat_ecds_full_raw<-sqlQuery(con,string_ecds_full)
  
  dat_ecds_full <- dat_ecds_full_raw %>%
    mutate(nhs_number = as.character(nhs_number)) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(nhs_number, Attendance_Unique_Identifier,  tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
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
  
  
  # arrivals that become dta
  arrivals <- 
    dat_ecds %>%
    filter(!is.na(dta_time),
           arrival_dt >= report_start,
           # nhs_number %in% unique(dat_attr$nhs_number),
           !is.na(nhs_number)) %>%
    group_by(nhs_number, arr_id) %>%
    summarise(
      tfc = tfc[1],
      arrival_dt = arrival_dt, 
      departure_dt = as.Date(departure_dt)[1]
    ) %>%
    ungroup()
  
  # all arrivals
  arrivals_full <- 
    dat_ecds_full %>%
    filter(arrival_dt >= report_start) %>%
    group_by() %>%
    summarise(
      id = Attendance_Unique_Identifier,
      site = Site_Name,
      arrival_dt = arrival_dt, 
    ) %>%
    distinct()
  
  
  dat_spec <- arrivals %>%
    select(nhs_number, arr_id, tfc) %>%
    mutate(spec = ifelse(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
    #mutate(MainSpecialtyCode = ifelse(MainSpecialtyCode %in% c(300, 326, 430), "medical", "not medical")) %>% 
    #mutate(spec = ifelse(tfc == "medical" | MainSpecialtyCode == "medical", "medical", "not medical")) %>%
    select(nhs_number, arr_id, spec)

  
  dat_dtas <- dat_ecds %>%
    left_join(dat_spec) %>%
    filter(spec == "medical") %>%
    group_by(site = Site_Name) %>%
    nest() %>%
    mutate(dtas = list(map2_dbl(snapshot_seq, data, \(snapshot, data) sum(
      snapshot %within% na.omit(data$dta_int)
    ))))  %>%
    mutate(arrivals = list(map2_dbl(snapshot_int,
                                    site,
                                    \(snapshot_int, site_id)
                                    nrow(filter(arrivals_full, arrival_dt %within% snapshot_int, site == site_id))
                                    )
                           )
           ) %>%
    mutate(dta_df = map(dtas, function(n_dta) map2_df(n_dta, snapshot_seq, ~tibble(dt = .y, n_dta = .x))))  %>%
    mutate(arrival_df = map(arrivals, function(n_arrival_all) map_df(n_arrival_all, ~tibble(n_arrival_all = .x)))) %>%
    select(site, dta_df, arrival_df) %>%
    unnest(cols = c(dta_df, arrival_df)) %>%
    left_join(site_resus_maj) %>%
    mutate(n_dta_norm = n_dta/total) %>%
    ungroup()
  
 dat_top_stats <- dat_ecds %>%
    left_join(dat_spec) %>%
    filter(spec == "medical", !is.na(dta_dt)) %>%
    summarise(n_dta = n(), dta_time = sum(dta_time), .by = Site_Name)
  
  list(dta_df = dat_dtas, dta_top_stats = dat_top_stats)
})

dta_df <- dta_dats$dta_df
dta_top_stats <- dta_dats$dta_top_stats

resp_df <- local({
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
where Outcome in ('See & Convey', 'See & Treat') 
AND CCG = '15C'
AND
  CONVERT(DATE, [T0_Time_of_Call]) >= '{report_start}'"
  )
  
  actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst)
  
  
  dat <- actv_swafst %>%
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
    group_by(dt = floor_date(dt, "2 hours"), call_category) %>%
    summarise(n = n(), response = sum(response)) %>%
    ungroup() %>%
    na.omit()
  
  dat 
})

ed_attend_impact <- dta_df %>%
  select(site, n_arrival_all, n_dta_norm) %>%
  mutate(ed_time_impact = (n_dta_norm * ed_time_coef * n_arrival_all)/60) %>%
  select(-n_dta_norm, -n_arrival_all) %>%
  summarise(across(everything(), sum),
            .by = site) %>%
  pivot_longer(-site, names_to = "impact") %>%
  mutate(impact = "om3",
         label = "hours to be seen for treatment",
         colour = "#A89532") %>%
  rename(grp = site)



handover_impact <- dta_df %>%
  select(site, dt, n_arrival_all, n_dta_norm) %>%
  left_join(handover_arrivals) %>%
  mutate(n_handover = coalesce(n, 0)) %>%
  select(-n, -dt) %>%
  mutate(
         handover_impact = (n_dta_norm * handover_coef * n_handover)/60,
         ) %>%
  select(-n_dta_norm) %>%
  summarise(across(everything(), sum),
            .by = site) %>%
  select(grp = site, handover_impact)  %>%
  pivot_longer(-grp, names_to = "impact") %>%
  mutate(impact = "om2",
         label = "hours for ambulance handover",
         colour = "#9532a8") 


resp_impact <- dta_df %>%
  select(site, dt, n_dta_norm) %>%
  summarise(n_dta_norm = mean(n_dta_norm), .by = dt) %>% 
  # expand_grid(call_category = unique(resp_df$call_category)) %>%
  left_join(complete(resp_df, dt = snapshot_seq, call_category, fill = list(n = 0, response = 0))) %>%
  select(-dt) %>%
  left_join(unnest(enframe(resp_coef, name = "call_category", value = "coef"), cols = coef)) %>% 
  mutate(
    resp_impact = (n_dta_norm * coef * n)/60
  ) %>% 
  select(-c(n_dta_norm, coef, n)) %>%
  summarise(across(everything(), sum),
            .by = call_category) %>%
  select(grp = call_category, resp_impact) %>%
  pivot_longer(-grp, names_to = "impact") %>%
  mutate(impact = str_remove(impact, "_impact"),
         label = "hours for ambulance response time",
         impact = case_when(
           grp == "cat1" ~ "om1a",
           grp == "cat2" ~ "om1b",
           grp == "cat3" ~ "om1c"
           ),
         colour = "#32A895") 

impacts_output <- bind_rows(list(
  downstream_impacts,
  ed_attend_impact,
  handover_impact,
  resp_impact
))

saveRDS(impacts_output, "data/avoidable_impacts_output.RDS")


saveRDS(handover_impact, "data/avoidable_impacts_handover_impact.RDS")
saveRDS(ed_attend_impact, "data/avoidable_impacts_ed_attend_impact.RDS")
saveRDS(resp_impact, "data/avoidable_impacts_resp_impact.RDS")

handover_impact <- readRDS("data/avoidable_impacts_handover_impact.RDS")
ed_attend_impact <- readRDS("data/avoidable_impacts_ed_attend_impact.RDS")
resp_impact <- readRDS("data/avoidable_impacts_resp_impact.RDS")



"#32A895"
"#9532a8"
"#A89532"



handover_plots <- handover_impact %>%
  mutate(label = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#9532a8 **-{round(handover_impact/60)}-**}')) %>%
  nest(.by = site) %>%
  mutate(handover_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = label),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
    })) %>%
  select(-data)


ed_plots <- ed_attend_impact %>%
  mutate(label = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#A89532 **-{round(ed_time_impact/60)}-**}')) %>%
  nest(.by = site) %>%
  mutate(ed_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = label),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
  })) %>%
  select(-data)


om_margin_plot <- tibble(
  om = c(
'hours for {.#9532a8 **ambulance handover**}',
'hours to be {.#A89532 **seen for treatment**}'
),
label = om) %>%
  nest(.by = om) %>%
  mutate(om_margin_plot = map(data,
                                \(d) d %>%
                                  mutate(x = 1, y = 1)  %>%
                                  ggplot(mapping = aes(x, y)) +
                                  # geom_text(aes(label = label))
                                  geom_marquee(
                                    aes(label = label),
                                    size = 8,
                                    width = 0.85,
                                    style = classic_style(align = "center"),
                                    #family = 'Comic Sans MS'
                                  ) +
                                  theme_void())) %>%
  select(-data)


site_margin_plot <- tibble(
  site = c(
    "SOUTHMEAD HOSPITAL",
    "WESTON GENERAL HOSPITAL",
    "BRISTOL ROYAL INFIRMARY"
  ),
  label = c(
    "at Southmead hospital.",
    "at Weston general hospital.",
    "at Bristol royal infirmary."
    )
) %>%
nest(.by = site) %>%
mutate(site_margin_plot = map(data,
                  \(d) d %>%
                    mutate(x = 1, y = 1)  %>%
  ggplot(mapping = aes(x, y)) +
  # geom_text(aes(label = label))
    geom_marquee(
      aes(label = label),
      size = 8,
      width = 0.85,
      style = classic_style(align = "center"),
      #family = 'Comic Sans MS'
    ) +
  theme_void())) %>%
  select(-data)


wi <- c(3/8, 3/8, 2/8)
hi <- c(4/5, 1/5)

p1 <- left_join(handover_plots, ed_plots) %>%
  left_join(site_margin_plot) %>%
  pivot_longer(-site, names_to = "om") %>%
  pull(value) %>%
  wrap_plots(ncol = 3, widths = wi)
  
p2 <- wrap_plots(list(om_margin_plot$om_margin_plot[[1]],
                      om_margin_plot$om_margin_plot[[2]],
                      plot_spacer()), widths = wi)

wrap_plots(list(p1, p2), ncol = 1, heights = hi) +
  plot_annotation("Patients arriving at the emergency department waited an additional..") &
  theme(plot.title = element_marquee(size = 34, width = 1))

  
handover_impact$plot


%>%
  mutate(across(-c(site, n_arrival_all, n_handover), .fns = list(per_arrival = \(x) x/n_arrival)))
  
  %>%
  pivot_longer(cols = -c(site, n_arrival),
               names_to = c("metric", "grp"),
               names_sep = "_per_") %>%
  mutate(grp = coalesce(grp, "full"))

handover_arrivals %>%
  select(site, n_arrival = n, n_dta_norm)





%>%
  ggplot(aes(y = str_remove(metric, "_impact"), x = value/60)) +
  geom_col() +
  ggh4x::facet_grid2(site~grp, scales = "free", independent = "x") +
  labs(x = "Hours additional", y = "Impact")



# handover_plots <- handover_impact %>%
#   mutate(label = glue::glue(#"<span style='color:black'>Over the last 7 days patients waited an additional</span><br><br>
#                             "<span style='color:#9532a8; font-size:32pt'>**{round(handover_impact/60)}**</span><br><br>
#                             <span style='color:black'>hours for</span> <span style='color:#9532a8'>**ambulance handover**</span><br>")) %>%
#   nest(.by = site) %>%
#   mutate(handover_plot = map(data, \(d) {
#   ggplot(data = d, aes(x = 1, y = 1, label = label)) +
#   geom_richtext(col = NA)  +
#   theme_void(base_family = "mono")})) %>%
#   select(-data)
#   
# 
# ed_plots <- ed_attend_impact %>%
#   mutate(label = glue::glue(#"<span style='color:black'>Over the last 7 days patients waited an additional</span><br><br>
#                             "<span style='color:#A89532; font-size:32pt'>**{round(ed_time_impact/60)}**</span><br><br>
#                             <span style='color:black'>hours to be</span> <span style='color:#A89532'>**seen for treatment**</span><br>")) %>%
#   nest(.by = site) %>%
#   mutate(ed_plot = map(data, \(d) {
#     ggplot(data = d, aes(x = 1, y = 1, label = label)) +
#       geom_richtext(col = NA)  +
#       theme_void(base_family = "mono")})) %>%
#   select(-data)


                          