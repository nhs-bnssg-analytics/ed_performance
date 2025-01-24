require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)

# upstream data
con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

report_start <- '2025-01-13'
report_end <- '2025-01-20'
n_hour <- 2

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
    select(nhs_number, tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
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
  
  arrivals <- 
    dat_ecds %>%
    filter(!is.na(dta_time),
           arrival_dt >= report_start,
           # nhs_number %in% unique(dat_attr$nhs_number),
           !is.na(nhs_number)) %>%
    group_by(nhs_number, arr_id) %>%
    summarise(
      tfc = tfc[1],
      departure_dt = as.Date(departure_dt)[1]
    )
  
  
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
    mutate(dtas = list(map2_dbl(snapshot_seq, data, ~ sum(
      .x %within% na.omit(.y$dta_int)
    )))) %>%
    mutate(dta_df = map(dtas, function(n_dta) map2_df(n_dta, snapshot_seq, ~tibble(dt = .y, n_dta = .x))))  %>%
    select(site, dta_df) %>%
    unnest(dta_df)
  dat_dtas
})




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