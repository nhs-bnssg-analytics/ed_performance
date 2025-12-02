perf_df <- local({
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
      --where Organisation_Code_Provider = 'RVJ'
      where Arrival_Date >= '{report_start}'
      and Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  close(con)
  
  dat_ecds<-dat_ecds_raw %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care",
                                                    "Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    select(-Acuity_Desc) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    select(-c(Arrival_Date,
              Arrival_Time,
              Decision_To_Admit_Date,
              Decision_To_Admit_Time,
              Departure_Date,Departure_Time)) %>%
    rowwise() %>%
    mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(time_arr_to_dep=difftime(departure_dt,arrival_dt,units="mins")) %>%
    mutate(time_dta_to_dep=difftime(departure_dt,dta_dt,units="mins")) %>%
    mutate(departure_day=floor_date(departure_dt,"day")) 
  
  
  dat_ecds1<-dat_ecds %>%
    mutate(reduce1=ifelse(time_dta_to_dep>1*60,time_dta_to_dep-1*60,0)) %>%
    mutate(reduce2=ifelse(time_dta_to_dep>2*60,time_dta_to_dep-2*60,0)) %>%
    mutate(reduce3=ifelse(time_dta_to_dep>3*60,time_dta_to_dep-3*60,0)) %>%
    mutate(reduce4=ifelse(time_dta_to_dep>4*60,time_dta_to_dep-4*60,0)) %>%
    mutate(reduce1=replace_na(reduce1,0)) %>%
    mutate(reduce2=replace_na(reduce2,0)) %>%
    mutate(reduce3=replace_na(reduce3,0)) %>%
    mutate(reduce4=replace_na(reduce4,0)) %>%
    mutate(time_arr_to_dep1=time_arr_to_dep-reduce1) %>%
    mutate(time_arr_to_dep2=time_arr_to_dep-reduce2) %>%
    mutate(time_arr_to_dep3=time_arr_to_dep-reduce3) %>%
    mutate(time_arr_to_dep4=time_arr_to_dep-reduce4)
  
  
  perf_sum <- dat_ecds1 %>%
    pivot_longer(cols=c(time_arr_to_dep,time_arr_to_dep1,time_arr_to_dep2,time_arr_to_dep3,time_arr_to_dep4),names_to="type",values_to="value") %>%
    mutate(type=recode(type,time_arr_to_dep="Unmodified",
                       time_arr_to_dep1="DTA wait capped at 1 hour",
                       time_arr_to_dep2="DTA wait capped at 2 hours",
                       time_arr_to_dep3="DTA wait capped at 3 hours",
                       time_arr_to_dep4="DTA wait capped at 4 hours")) %>%
    group_by(departure_day,type, ed_category, site = Site_Name) %>%
    summarise(perf=sum(value<4*60)/n()) 
  
  
  perf_sum 
})



# explanatory variables

#  SWASFT

arr_df <- local({
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
Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') AND
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
    mutate(major_acuity = dplyr::case_when(Incident_Colour %in% c(
      "Category 1",
      "Category 2"
    ) ~ 1,
    Incident_Colour %in% c(
      "Category 3",
      "Category 4",
      "Category 5") ~ 5,
    is.na(Incident_Colour) ~ NA_real_)) %>%
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
    dplyr::filter(as.Date(timestamp) == report_date) %>%
    dplyr::group_by(Derived_Pseudo_NHSNumber) %>%
    dplyr::select(Site_Name, NHS_Number = Derived_Pseudo_NHSNumber, activity_id, timestamp, resource_id, major_acuity)
  
})
