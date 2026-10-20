require(tidyverse)
require(RODBC)
require(lubridate)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"

report_start <- '2024-02-01'

con<-odbcDriverConnect(con_string)


dat_perf <- local({
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



# - 4h performance for all ED departures in day
# - 4h performance for all ED departures in day, for admitted
# - 4h performance for all ED departures in day, for non-admitted

dat_perf<-dat_ecds %>%
  mutate(time_arr_to_initial=difftime(initial_dt,arrival_dt,units="mins")) %>%
  mutate(time_initial_to_treatment=difftime(treatment_dt,initial_dt,units="mins")) %>%
  mutate(time_arr_to_dep=difftime(departure_dt,arrival_dt,units="mins")) %>%
  mutate(admitted=case_when(is.na(dta_dt)==TRUE ~"NO",.default="YES")) %>%
  rbind(mutate(., ed_category = "all")) %>%
  group_by(site = Site_Name, date = as.Date(date), ed_category) %>%
  summarise(perf_all=sum(time_arr_to_dep<4*60)/n()) %>%
  ungroup()

#,
            #perf_admitted=sum(time_arr_to_dep[admitted=="YES"]<4*60)/sum(admitted=="YES"),
            #perf_notadmitted=sum(time_arr_to_dep[admitted=="NO"]<4*60)/sum(admitted=="NO"),
            #mean_time_arr_to_initial=as.numeric(mean(time_arr_to_initial,na.rm=TRUE)),
            #mean_time_initial_to_treatment=as.numeric(mean(time_initial_to_treatment,na.rm=TRUE)))

dat_perf
})

dat_breach <- local({
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



# - 4h performance for all ED departures in day
# - 4h performance for all ED departures in day, for admitted
# - 4h performance for all ED departures in day, for non-admitted

dat_breach<-dat_ecds %>%
  mutate(time_arr_to_initial=difftime(initial_dt,arrival_dt,units="mins")) %>%
  mutate(time_initial_to_treatment=difftime(treatment_dt,initial_dt,units="mins")) %>%
  mutate(time_arr_to_dep=difftime(departure_dt,arrival_dt,units="mins")) %>%
  mutate(admitted=case_when(is.na(dta_dt)==TRUE ~"NO",.default="YES")) %>%
  rbind(mutate(., ed_category = "all")) %>%
  group_by(site = Site_Name, date = as.Date(date), ed_category) %>%
  summarise(breach_all=sum(time_arr_to_dep<4*60)) %>%
  ungroup()

dat_breach
})


## expln. vars
# SWASFT

amb_conv <- local({
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
  group_by(site = Site_Name, ed_category, date = as.Date(timestamp)) %>%
  count(name = "n_conv") %>%
  na.omit() %>%
  ungroup()
})

dat_perf %>%
  left_join(amb_conv) %>%
  filter(ed_category == "all",
         # filter strange DQ issues
         date != lubridate::ymd("2024-04-21"),
         date != lubridate::ymd("2024-04-19"),
         date != lubridate::ymd("2024-02-01")
  ) %>%
  ggplot(aes(x = n_conv, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(vars(site))

dat_breach %>%
  left_join(amb_conv) %>%
  ggplot(aes(x = n_conv, y = breach_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(site~ed_category)

dat_perf %>%
  left_join(amb_conv) %>%
  ggplot(aes(x = n_conv, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(site~ed_category)

dat_breach %>%
  left_join(amb_conv) %>%
  filter(n_conv > 5) %>%
  na.omit() %>%
  ggplot(aes(x = n_conv, y = breach_all, col = site)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(vars(ed_category), scales = "free_x") +
  bnssgtheme() +
  scale_colour_bnssg()

# alamac

bed_occ <- local({
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



dat_perf %>%
left_join(bed_occ) %>%
  ggplot(aes(x = bed_occ, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")

dat_breach %>%
left_join(bed_occ) %>%
  ggplot(aes(x = bed_occ, y = breach_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")

dat_perf %>%
left_join(bed_occ) %>%
  ggplot(aes(x = bed_occ, y = perf_all, col = site)) +
  geom_point() +
  geom_smooth(method = lm) 

dat_breach %>%
left_join(bed_occ) %>%
  # na.omit() %>%
  # group_by(site) %>%
  # mutate(bed_occ = scale(bed_occ),
  #        breach_all = scale(breach_all)) %>%
  ggplot(aes(x = bed_occ, y = breach_all, col = site)) +
  geom_point() +
  geom_smooth(method = lm) 


calls_aband <- local({
  RODBC::sqlQuery(con, glue::glue("
        SELECT * FROM
        [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_UrgentCare_Daily]
        WHERE METRIC_ID IN (320627, 320626)
        AND report_date > '{report_start}'
                      ")) %>%
    mutate(date = ymd(Report_Date)) %>%
    pivot_wider(id_cols = date, names_from = METRIC_NAME, values_from = Value) %>%
    mutate(aband_rate = `(Severnside) Calls Abandoned after 30 Seconds`/`(Severnside) Calls Received`) %>%
    dplyr::select(date, aband_rate)
  })



# ecds

walk_ins <- local({
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
    mutate(arrival_date=ymd(Arrival_Date)) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    rbind(mutate(., ed_category = "all")) %>%
    group_by(site = Site_Name, date = arrival_date, ed_category) %>%
    count(name = "walk_ins") %>%
    ungroup()
  
})

dat_perf %>%
  left_join(walk_ins) %>%
  ggplot(aes(x = walk_ins, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(ed_category~site, scales = "free_x")

dat_breach %>%
  left_join(walk_ins) %>%
  ggplot(aes(x = walk_ins, y = breach_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(ed_category~site, scales = "free_x")

n_major <- local({
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
      and Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  
  dat_ecds_raw %>%
    mutate(arrival_date=ymd(Arrival_Date)) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    group_by(site = Site_Name, date = arrival_date, ed_category) %>%
    count(name = "n") %>%
    pivot_wider(names_from = ed_category, values_from = n) %>%
    # mutate(minor_pct = minor/(major+minor)) %>%
    select(n_major = major) %>%
    ungroup()
})


dat_perf %>%
  left_join(n_major) %>%
  ggplot(aes(x = n_major, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")

dat_breach %>%
  left_join(n_major) %>%
  ggplot(aes(x = n_major, y = breach_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")


tot_arr <- local({
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
      and Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      ")
  dat_ecds_raw<-sqlQuery(con,string_ecds)
  
  dat_ecds_raw %>%
    mutate(arrival_date=ymd(Arrival_Date)) %>%
    mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                 TRUE~"major")) %>%
    rbind(mutate(., ed_category = "all")) %>%
    group_by(site = Site_Name, date = arrival_date) %>%
    count(name = "tot_arr") %>%
    ungroup()
})

dat_perf %>%
  left_join(tot_arr) %>%
  ggplot(aes(x = tot_arr, y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")

dat_breach %>%
  left_join(tot_arr) %>%
  ggplot(aes(x = tot_arr, y = breach_all)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(~site, scales = "free_x")

dat_breach %>%
  left_join(tot_arr) %>%
  ggplot(aes(x = tot_arr, y = breach_all, col = site)) +
  geom_point() +
  geom_smooth(method = lm)

dat_perf %>%
  left_join(tot_arr) %>%
  ggplot(aes(x = tot_arr, y = perf_all, col = site)) +
  geom_point() +
  geom_smooth(method = lm)

dat_perf %>%
  filter(ed_category == "all") %>%
  select(site, date, perf_all) %>%
  full_join(bed_occ) %>%
  full_join(walk_ins %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(tot_arr) %>%
  full_join(n_major) %>%
  full_join(amb_conv %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  na.omit() %>%
  pivot_longer(cols = -c(site, date, perf_all),
               names_to = "metric",
               values_to = "value") %>%
  group_by(site, metric) %>%
  nest() %>%
  # centre and scale
  mutate(data = map(data, ~.x %>% mutate(value = scale(value)[,1]))) %>%
  mutate(cor = map(data, ~cor.test(.x$perf_all, .x$value) %>% broom::tidy())) %>%
  unnest(cor) %>%
  arrange(desc(abs(estimate))) %>%
  filter(p.value <= 0.05) %>%
  ggplot(aes(x = metric, y = estimate)) + geom_col() + facet_wrap(vars(site))

dat_breach %>%
  filter(ed_category == "all") %>%
  select(site, date, breach_all) %>%
  full_join(bed_occ) %>%
  full_join(walk_ins %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(tot_arr) %>%
  full_join(n_major) %>%
  full_join(amb_conv %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  na.omit() %>%
  pivot_longer(cols = -c(site, date, breach_all),
               names_to = "metric",
               values_to = "value") %>%
  group_by(site, metric) %>%
  nest() %>%
  # centre and scale
  # mutate(data = map(data, ~.x %>% mutate(value = scale(value)[,1]))) %>%
  mutate(cor = map(data, ~cor.test(.x$breach_all, .x$value) %>% broom::tidy())) %>%
  unnest(cor) %>%
  arrange(desc(abs(estimate))) %>% 
  filter(p.value <= 0.05) %>%
  ggplot(aes(x = metric, y = estimate)) + geom_col() + facet_wrap(vars(site))


# NCTR

n_nctr <- local({
  nctr_df <-
    RODBC::sqlQuery(
      con,
      "SELECT
       [RN]
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
  
  # max census date
  
  max_date <- nctr_df %>%
    filter(!is.na(NHS_Number)) %>%
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    pull(Census_Date) %>%
    max()
  
  # NCTR data summary
  
  nctr_sum <- nctr_df %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
           sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
    # filter for our main sites / perhaps I shouldn't do this?
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
    # filter(Criteria_To_Reside == "N") %>%
    mutate(
      site = case_when(
        Organisation_Site_Code == 'RVJ01' ~ 'SOUTHMEAD HOSPITAL',
        Organisation_Site_Code == 'RA701' ~ 'BRISTOL ROYAL INFIRMARY',
        Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'WESTON GENERAL HOSPITAL',
        TRUE ~ 'other'
      ),
      Date_Of_Admission = as.Date(Date_Of_Admission)
    ) %>%
    group_by(Organisation_Site_Code) %>%
    filter(Census_Date > report_start) %>%
    ungroup() %>%
    mutate(
      der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
      der_ctr = case_when(
        Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
        !is.na(Days_NCTR) ~ FALSE,
        !is.na(Date_NCTR) ~ FALSE,
        Criteria_To_Reside == "N" ~ FALSE
      )) %>%
    mutate(
      pathway = recode(
        Current_Delay_Code_Standard,
        "P3 / Other Complex Discharge" = "P3",
        "Uncoded" = "Other",
        "Repatriation" = "Other",
        "NCTR Null" = "Other",
        "Not Set" = "Other",
        "18a  Infection  bxviii  Standard" = "Other",
        "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
        "15b  Repat  bxv  WGH" = "Other"
      ),
      pathway = coalesce(pathway, "Other")
    ) %>%
    mutate(pathway = if_else(
      !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
      "Other",
      pathway
    )) %>%
    dplyr::select(
      Census_Date,
      nhs_number,
      sex,
      age = Person_Age,
      ctr = der_ctr,
      site,
      bed_type = Bed_Type,
      pathway
    ) %>%
    ungroup()
  
nctr_sum %>%
    filter(!is.na(nhs_number), !is.na(ctr)) %>%
    filter(pathway != "Other", !ctr) %>% 
    select(-ctr) %>%
    group_by(site, pathway, date = as.Date(Census_Date)) %>%
    count() %>%
    pivot_longer(cols = c(n),
                 names_to = "metric",
                 values_to = "value") %>%
  group_by(site, date) %>%
  summarise(n_nctr = sum(value))
  
  })

ccf_util <- function(dat, col_name){
  col_name_sym <- sym(col_name)
  dat %>%
    tsibble::as_tsibble(index = date) %>%
    tsibble::fill_gaps() %>%
    feasts::CCF(!!col_name_sym, Breaches, lag_max = 7) %>%
    autoplot() +
    labs(title = str_replace(col_name, r"(\n)", " "))
}


var_rn <- c(
  "Breaches" = "breach_all",
  "Beds occupied" = "bed_occ",
  "Walk ins" = "walk_ins",
  "Total arrivals" = "tot_arr",
  "N majors" = "n_major",
  "Amb.\nconveyances" = "n_conv",
  "Patients NCtR" = "n_nctr",
  "111 Call\nabandonment rate" = "call_aband"
)

foo <- dat_breach %>%
  filter(ed_category == "all") %>%
  select(site, date, breach_all) %>%
  full_join(bed_occ) %>%
  full_join(walk_ins %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(tot_arr) %>%
  full_join(n_major) %>%
  full_join(amb_conv %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(n_nctr) %>%
  full_join(calls_aband) %>%
  na.omit() %>% 
  group_by(site) %>%
  # remove outlier
  filter(breach_all > quantile(breach_all, 0.0275),
         breach_all < quantile(breach_all, 0.925)) %>% # remove some (presumably) DQ records
  nest() %>%
  mutate(data = map(data, ~rename(.x, !!!var_rn))) %>%
  mutate(pairs = map(data, ~.x %>% select(where(is.numeric)) %>% GGally::ggpairs())) %>%
  mutate(pairs = map(pairs, ~.x +
                       theme_bw() +
                       labs(title = glue::glue("{str_to_title(site)}")) +
                       scale_x_continuous(guide = guide_axis(check.overlap = TRUE)))) %>%
  # mutate(cor = map(data, ~ select(.x, where(is.numeric)) %>% rstatix::cor_mat())) %>%
  # mutate(cor = map(cor, rstatix::cor_gather)) %>%
  mutate(lags = map(data, function(dat){
    map(setdiff(names(dat), c("date", "Breaches")),
        function(col_name) {
          ccf_util(dat, col_name)
          }
    )
    })) %>%
  mutate(lags = map(lags, ~patchwork::wrap_plots(.x, ncol = 2) + patchwork::plot_annotation(title = str_to_title(site))))



walk2(
  foo$pairs,
  foo$site,
  ~ ggsave(
    filename = glue::glue("materials/pairs_{str_to_title(.y)}.png"),
    .x,
    height = 10,
    width = 10,
    scale = 0.8
  )
)

walk2(
  foo$lags,
  foo$site,
  ~ ggsave(
    filename = glue::glue("materials/ccf_{str_to_title(.y)}.png"),
    .x,
    height = 10,
    width = 15,
    scale = 0.8
  )
)


patchwork::wrap_plots(foo$lags[[1]], ncol = 3)


foo$cor[[2]] %>%
  group_by(var1) %>%
  mutate(cor_target = cor[var2 == "breach_all"]) %>%
  mutate(cor = ifelse(var1 == var2, cor_target, cor)) %>%
  filter(var1 != "breach_all", var2 != "breach_all") %>%
  ungroup() %>%
  arrange(var1, var2) %>%
  mutate(id = group_indices(., pmax(var1, var2), 
                pmin(var1, var2))) %>% 
  mutate(var1 = factor(var1, unique(var1)),
         var2 = factor(var2, unique(var2)),
         var2 = fct_rev(var2),
         col = if_else(cor < 0, "neg", "pos")) %>%
  group_by(id) %>%
  slice(1) %>%
  ggplot(aes(x = var1, y = var2)) + geom_tile(aes(fill = cor)) + scale_fill_distiller(palette = "RdYlGn")


ccf_util_2 <- function(dat, col_name){
  col_name_sym <- sym(col_name)
  dat %>%
    tsibble::as_tsibble(index = date) %>%
    tsibble::fill_gaps() %>%
    feasts::CCF(!!col_name_sym, `4hr\nperformance`, lag_max = 7) %>%
    autoplot() +
    labs(title = str_replace(col_name, r"(\n)", " "))
}

var_rn_2 <- c(
  "4hr\nperformance" = "perf_all",
  "Beds occupied" = "bed_occ",
  "Walk ins" = "walk_ins",
  "Total arrivals" = "tot_arr",
  "N majors" = "n_major",
  "Amb.\nconveyances" = "n_conv",
  "Patients NCtR" = "n_nctr",
  "111 Call\nabandonment rate" = "aband_rate"
)


foo_2 <- dat_perf %>%
  filter(ed_category == "all",
         # filter strange DQ issues
         date != lubridate::ymd("2024-04-21"),
         date != lubridate::ymd("2024-04-19"),
         date != lubridate::ymd("2024-02-01")
         ) %>%
  select(site, date, perf_all) %>%
  full_join(bed_occ) %>%
  full_join(walk_ins %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(tot_arr) %>%
  full_join(n_major) %>%
  full_join(amb_conv %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(n_nctr) %>%
  full_join(calls_aband) %>%
  na.omit() %>% 
  group_by(site) %>%
  # remove outlier
  # filter(perf_all > quantile(perf_all, 0.0275),
  #        perf_all < quantile(perf_all, 0.925)) %>% # remove some (presumably) DQ records
  nest() %>%
  mutate(data = map(data, ~rename(.x, !!!var_rn_2))) %>%
  mutate(pairs = map(data, ~.x %>% select(where(is.numeric)) %>% GGally::ggpairs())) %>%
  mutate(pairs = map(pairs, ~.x +
                       theme_bw() +
                       labs(title = glue::glue("{str_to_title(site)}")) +
                       scale_x_continuous(guide = guide_axis(check.overlap = TRUE)))) %>%
  # mutate(cor = map(data, ~ select(.x, where(is.numeric)) %>% rstatix::cor_mat())) %>%
  # mutate(cor = map(cor, rstatix::cor_gather)) %>%
  mutate(lags = map(data, function(dat){
    map(setdiff(names(dat), c("date", "4hr\nperformance")),
        function(col_name) {
          ccf_util_2(dat, col_name)
          }
    )
    })) %>%
  mutate(lags = map(lags, ~patchwork::wrap_plots(.x, ncol = 2) + patchwork::plot_annotation(title = str_to_title(site))))



walk2(
  foo_2$pairs,
  foo_2$site,
  ~ ggsave(
    filename = glue::glue("materials/pairs_{str_to_title(.y)}.png"),
    .x,
    height = 10,
    width = 10,
    scale = 0.8
  )
)

walk2(
  foo_2$lags,
  foo_2$site,
  ~ ggsave(
    filename = glue::glue("materials/ccf_{str_to_title(.y)}.png"),
    .x,
    height = 10,
    width = 15,
    scale = 0.8
  )
)
