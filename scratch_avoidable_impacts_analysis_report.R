require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)
library(patchwork)
library(ggtext)
library(marquee)
library(extrafont)

# upstream data
con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

report_start <- '2025-01-20'
report_end <- '2025-01-27'
# report_start <- '2024-01-01'
# report_end <- '2025-01-31'
n_hour <- 2

bin_acc <- 0.01


handover_coef <- readRDS("data/handover_outputs.RDS") %>%
  `[[`("full_mod") %>%
  `[[`("coefficients") %>%
  `[`("n_dta_norm")

ed_time_coef <- readRDS("data/time_ed_outputs.RDS") %>%
  `[[`("full_mod") %>%
  `[[`("coefficients") %>%
  `[`("n_dta_norm")


resp_coef <- readRDS("data/resp_outputs.RDS")  %>%
  `[[`("full_mod") %>%
  map(~.x %>%  `[[`("coefficients") %>%
        `[`("n_dta_norm"))



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


sex_df <- readRDS("data/dat_attr_demog.RDS") %>%
  select(nhs_number, sex) %>%
  distinct()

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

dta_df <- local({
  
  
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
  
  dat_ecds_full<-dat_ecds_full_raw %>%
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
    ))))   %>%
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
  
  dat_dtas
})

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
  mutate(ed_time_impact = n_dta_norm * ed_time_coef * n_arrival_all) %>%
  select(-n_dta_norm, -n_arrival_all) %>%
  summarise(across(everything(), sum),
            .by = site)



handover_impact <- dta_df %>%
  select(site, dt, n_arrival_all, n_dta_norm) %>%
  left_join(handover_arrivals) %>%
  mutate(n_handover = coalesce(n, 0)) %>%
  select(-n, -dt) %>%
  mutate(
         handover_impact = n_dta_norm * handover_coef * n_handover,
         # ed_time_impact = n_dta_norm * ed_time_coef * n_arrival_all,
         # cat1_resp_impact = n_dta_norm * resp_coef$cat1 * n_arrival,
         # cat2_resp_impact = n_dta_norm * resp_coef$cat2 * n_arrival,
         # cat3_resp_impact = n_dta_norm * resp_coef$cat3 * n_arrival
         ) %>%
  select(-n_dta_norm) %>%
  summarise(across(everything(), sum),
            .by = site) %>%
  select(site, handover_impact) 


resp_impact <- dta_df %>%
  select(site, dt, n_dta_norm) %>%
  summarise(n_dta_norm = mean(n_dta_norm), .by = dt) %>% 
  # expand_grid(call_category = unique(resp_df$call_category)) %>%
  left_join(complete(resp_df, dt = snapshot_seq, call_category, fill = list(n = 0, response = 0))) %>%
  select(-dt) %>%
  left_join(unnest(enframe(resp_coef, name = "call_category", value = "coef"), cols = coef)) %>% 
  mutate(
    resp_impact = n_dta_norm * coef * n
    # ed_time_impact = n_dta_norm * ed_time_coef * n_arrival_all,
    # cat1_resp_impact = n_dta_norm * resp_coef$cat1 * n_arrival,
    # cat2_resp_impact = n_dta_norm * resp_coef$cat2 * n_arrival,
    # cat3_resp_impact = n_dta_norm * resp_coef$cat3 * n_arrival
  ) %>% 
  select(-c(n_dta_norm, coef, n)) %>%
  summarise(across(everything(), sum),
            .by = call_category)


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
  mutate(label = glue::glue(#"<span style='color:black'>Over the last 7 days patients waited an additional</span><br><br>
                            "<span style='color:#9532a8; font-size:32pt'>**{round(handover_impact/60)}**</span><br><br>
                            <span style='color:black'>hours for</span> <span style='color:#9532a8'>**ambulance handover**</span><br>")) %>%
  nest(.by = site) %>%
  mutate(handover_plot = map(data, \(d) {
  ggplot(data = d, aes(x = 1, y = 1, label = label)) +
  geom_richtext(col = NA)  +
  theme_void(base_family = "mono")})) %>%
  select(-data)
  

ed_plots <- ed_attend_impact %>%
  mutate(label = glue::glue(#"<span style='color:black'>Over the last 7 days patients waited an additional</span><br><br>
                            "<span style='color:#A89532; font-size:32pt'>**{round(ed_time_impact/60)}**</span><br><br>
                            <span style='color:black'>hours to be</span> <span style='color:#A89532'>**seen for treatment**</span><br>")) %>%
  nest(.by = site) %>%
  mutate(ed_plot = map(data, \(d) {
    ggplot(data = d, aes(x = 1, y = 1, label = label)) +
      geom_richtext(col = NA)  +
      theme_void(base_family = "mono")})) %>%
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
                  \(d) ggplot(d, aes(x = 1, y = 1, label = label)) +
                    geom_text() +
                    theme_void(base_family = "mono")
                  )) %>%
  select(-data)


left_join(handover_plots, ed_plots) %>%
  left_join(site_margin_plot) %>%
  pivot_longer(-site) %>%
  pull(value) %>%
  wrap_plots(ncol = 3, widths = c(3/7, 3/7, 1/7))
  


foo <- handover_impact %>%
  mutate(label = glue::glue(.open = '-{',
                            .close = '}-',
                            ' Over the last 7 days patients waited an additional
                             ## {.#9532a8  **-{round(handover_impact/60)}-**}
                             hours for {.#9532a8 ambulance handover}')) %>%
  # mutate(label = glue::glue("<span style='color:blue'>{round(impact/60)}</span>"))%>%
  nest(.by = site)
# 
# %>%
#   mutate(plot = map(data, \(d) {
#     ggplot(d, aes(x = 0, y = 0, label = label)) +
#       annotate(geom = "marquee", size = 10, width = 0.4, hjust = 0) +
#       theme_void()})) %>%
#   pull(plot)
# 
# 
# 
foo$data[[1]] %>%
  mutate(x = 1, y = 1) %>%
  ggplot(mapping = aes(x, y)) +
  # geom_text(aes(label = label))
  geom_marquee(
    aes(label = label),
    size = 13,
    width = 0.85,
    style = classic_style(align = "justified-center"),
    family = 'Comic Sans MS'
  ) +
  theme_void()


  
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





                          