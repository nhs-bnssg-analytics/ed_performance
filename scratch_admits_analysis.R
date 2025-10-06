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
                        coalesce(destination_desc, 'Missing') as 'destination_desc',
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
  
  dat_ecds_raw %>%
    mutate(nhs_number = as.character(nhs_number)) %>%
    mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code), "Missing")) %>%
    mutate(admitted = ifelse(!is.na(Decision_To_Admit_Date), "Yes", "No")) %>%
    mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
    mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(site = Site_Name, nhs_number, ccg = Organisation_Code_Commissioner, date = floor_date(arrival_dt, "month"), admitted, tfc = Treatment_Function_Code, destination_desc) %>%
    count()
})


dta_df %>%
  filter(tfc %in% c(300, 326, 430)) %>%
  group_by(site, date, admitted) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = date, y = n, col = admitted)) + geom_line() + facet_wrap(vars(site))

dta_df %>%
  filter(tfc %in% c(300, 326, 430)) %>%
  filter(ccg == '15C') %>%
  group_by(site, admitted, nhs_number) %>%
  summarise(n = sum(n)) %>%
  group_by(site, admitted) %>%
  summarise(n = sum(n), n_pat = n_distinct(nhs_number))

