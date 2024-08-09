# refs: van Walraven paper: https://pubmed.ncbi.nlm.nih.gov/19433995/

require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)
library(comorbidity)

source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=xsw-000-sp09;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)


report_start <- '2021-06-01'
report_end <- '2024-06-01'
pre_window_days <- 60


n_fun <- function(x){
  return(data.frame(y = x/2, label = paste0(scales::number(x, big.mark = ","))))
}

mod_df <- readRDS("data/smr_mod_df_new.RDS")


p_demog_downstr_1 <- mod_df %>%
# mutate(imd = fct_recode(imd,
#                         "1\n(least deprived)" = "1 (least deprived)",
#                         "10\n(least deprived)" = "10 (most deprived)"
#                         )) %>%
select(Sex = sex,
       "Index of Multiple Deprivation\n(IMD) Decile" = imd,
       `Arrival Time` = arrival_time,
       `Arrival Month` = arrival_month, 
       `Arrival Year` = arrival_year) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~ ggplot(.x, aes(y = value, x = n)) +
                      geom_col() +
                       stat_summary(fun.data = n_fun,
                                    geom = "text",
                                    size = 3,
                                    col = "white"
                       ) +
                      theme_minimal() +
                      scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
                      guides(alpha = "none") +
                      theme(legend.position = "bottom",
                            axis.text.x = element_blank(),
                            legend.justification = "center",
                            legend.title = element_text()) +
                      labs(title = str_wrap(.y, 25),
                           x = "",
                           y = "")))



p_demog_downstr_2 <- mod_df %>%
  select(Age = age,
         `4-Hour ED Performance` =perf,
         `Elixhauser Comorbidity Index (vWs)` = vw_score,
         `Previous ED attendances` = n_ed) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~
  ggplot(.x, aes(x = value, y = "")) +
  geom_boxplot() +
  theme_minimal() +
  guides(alpha = "none") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        legend.title = element_text()) +
  labs(title = str_wrap(.y, 20),
       x = "",
       y = "")))


bind_rows(p_demog_downstr_1, p_demog_downstr_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Elixhauser Comorbidity Index (vWs)",
      "Index of Multiple Deprivation\n(IMD) Decile",
      "Previous ED attendances",
      "4-Hour ED Performance",
      "Arrival Month",
      "Arrival Time",
      "Arrival Year"
    )
  )) %>% 
arrange(name) %>%
pull(plot) %>%
patchwork::wrap_plots() & patchwork::plot_annotation(title = "Demographics for Downstream Impact Analysis",
                                                    subtitle = "Patients included in Standardised Mortality Analysis") 


ggsave("materials/demog_downstream_smr.png",
       last_plot(),
       bg = "white",
       height = 10,
       width = 12.5, 
       scale = 0.7)


# handover nhs_numbers:

handover_ids <- glue::glue(
  "SELECT DISTINCT
    [ID]
    ,[Derived_Pseudo_NHSNumber]
    ,[Incident_Number]
    ,[Time_Of_Incident]
    ,[CCG]
    ,[Post_Code_Sector]
    ,[Symptom_Group_Chief_Complaint]
   ,b.[OrganisationName] as 'Site_Name'
    ,[T0_Time_of_Call]
    ,[T1_Time_Call_answered]
    ,[Time_First_Core_Resource_Allocated]
    ,[Time_Last_Core_Resource_Cleared]
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
) %>% 
  RODBC::sqlQuery(con, .) %>%
  mutate(handover_time = lubridate::parse_date_time(Time_of_Patient_Handover, orders = "dmy HMS")) %>%
  mutate(handover_time = floor_date(handover_time, "month")) %>%
  filter(handover_time <= report_end) %>%
  select(nhs_number = Derived_Pseudo_NHSNumber, handover_time) %>%
  filter(!is.na(nhs_number)) %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  distinct()


# get bnssg nhs nums for filtering
string_attr <- glue::glue("select distinct 
 nhs_number,
 attribute_period,
 age,
 sex,
 lsoa,
 lsoa_2011
from MODELLING_SQL_AREA.dbo.primary_care_attributes
where attribute_period between '{report_start}' and '{report_end}'")


dat_attr <- sqlQuery(con,string_attr) %>%
  filter(!is.na(nhs_number)) %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  mutate(attribute_period = ymd(attribute_period))

saveRDS(dat_attr, "data/dat_attr_demog.RDS")
dat_attr <- readRDS("data/dat_attr_demog.RDS")

string_imd <- "select distinct * from Analyst_SQL_Area.dbo.IMD2019"

dat_imd <- sqlQuery(con,string_imd) 


handover_attr <- handover_ids %>%
  left_join(dat_attr, by = join_by(nhs_number == nhs_number, handover_time == attribute_period)) %>%
  distinct() %>%
  na.omit() %>%
  left_join(dat_imd, by = join_by(lsoa_2011 == "LSOA code (2011)")) %>%
  select(nhs_number, Age = age, Sex = sex, "Index of Multiple Deprivation\n(IMD) Decile" = `Index of Multiple Deprivation (IMD) Decile`) %>%
  distinct()

response_ids <- glue::glue(
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
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
LEFT JOIN [ABI].[Lard].[vw_Organisation_Lookup] b on a.[First_Hospital_Attended]=b.[OrganisationCode]
where Outcome in ('See & Convey', 'See & Convey') AND
CCG = '15C' AND
OrganisationName in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') AND
Conveyed_To_Ward_Type = 'A&E' AND
  
  CONVERT(DATE, [T0_Time_of_Call]) > '{report_start}'"
  ) %>% 
  RODBC::sqlQuery(con, .) %>% 
    mutate(across(matches("Time|Clock"),
                  .f = ~lubridate::parse_date_time(.x, orders = c("ymd_HMS", "dmy_HMS")))) %>%
    filter(Outcome %in% c("See & Treat", "See & Convey"))  %>%
    filter(T0_Time_of_Call >= ymd(report_start),
           T0_Time_of_Call <= ymd(report_end) ) %>%
    mutate(dt = floor_date(T0_Time_of_Call, glue::glue("month")),
           call_category = Incident_Colour,
           response = (`Response_Target_1_Clock Stop` - `Response_Target_1_Clock Start`)/dminutes(1)) %>%
    filter(call_category %in% str_c("Category ", c(1, 2, 3))) %>%
    mutate(call_category = recode(call_category,
                                  "Category 3"= "cat3",
                                  "Category 2"= "cat2",
                                  "Category 1"= "cat1")) %>%
    select(dt, nhs_number = Derived_Pseudo_NHSNumber, call_category, response)  %>%
  filter(!is.na(nhs_number)) %>%
  mutate(nhs_number = as.character(nhs_number))


response_attr <- response_ids %>%
  left_join(dat_attr, by = join_by(nhs_number == nhs_number, dt == attribute_period)) %>%
  distinct() %>%
  na.omit() %>%
  left_join(dat_imd, by = join_by(lsoa_2011 == "LSOA code (2011)")) %>%
  select(nhs_number, Age = age, Sex = sex, "Index of Multiple Deprivation\n(IMD) Decile" = `Index of Multiple Deprivation (IMD) Decile`) %>%
  distinct()
  


p_demog_upstr_resp_1 <- response_attr %>%
  filter(Sex %in% c("Male", "Female")) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = factor(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      levels = 1:10,
      labels = 1:10
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_recode(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "1 (least deprived)" = "1",
      "10 (least deprived)" = "10"
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_relevel(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "10 (least deprived)",
      after = Inf
    )
  ) %>%
  select(Sex, `Index of Multiple Deprivation\n(IMD) Decile`) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      name,
      ~ ggplot(.x, aes(y = value, x = n)) +
        geom_col() +
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          size = 3,
          col = "white"
        ) +
        theme_minimal() +
        scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
        guides(alpha = "none") +
        theme(
          legend.position = "bottom",
          axis.text.x = element_blank(),
          legend.justification = "center",
          legend.title = element_text()
        ) +
        labs(
          y = str_wrap(.y, 25),
          x = ""#,
          #y = ""
        )
    )
  )

p_demog_upstr_resp_2 <- response_attr %>%
  select(Age) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(
    data,
    name,
    ~
      ggplot(.x, aes(x = value, y = "")) +
      geom_boxplot() +
      theme_minimal() +
      guides(alpha = "none") +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        legend.title = element_text()
      ) +
      labs(
        y = str_wrap(.y, 20),
         x = ""#,
         #y = ""
      )
  ))


p_resp_ptc <- bind_rows(p_demog_upstr_resp_1, p_demog_upstr_resp_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
      )
  )) %>% 
  arrange(name) %>%
  pull(plot) %>%
  patchwork::wrap_plots(ncol = 1)  



# ggsave("materials/demog_upstream_resp.png",
#        last_plot(),
#        bg = "white",
#        height = 10,
#        width = 10, 
#        scale = 0.6)


p_demog_upstr_handover_1 <- handover_attr %>%
  filter(Sex %in% c("Male", "Female")) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = factor(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      levels = 1:10,
      labels = 1:10
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_recode(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "1 (least deprived)" = "1",
      "10 (least deprived)" = "10"
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_relevel(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "10 (least deprived)",
      after = Inf
    )
  ) %>%
  select(Sex, `Index of Multiple Deprivation\n(IMD) Decile`) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      name,
      ~ ggplot(.x, aes(y = value, x = n)) +
        geom_col() +
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          size = 3,
          col = "white"
        ) +
        theme_minimal() +
        scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
        guides(alpha = "none") +
        theme(
          legend.position = "bottom",
          axis.text.x = element_blank(),
          legend.justification = "center",
          legend.title = element_text()
        ) +
        labs(
          y = str_wrap(.y, 25),
          x = ""#,
          #y = ""
        )
    )
  )

p_demog_upstr_handover_2 <- handover_attr %>%
  select(Age) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(
    data,
    name,
    ~
      ggplot(.x, aes(x = value, y = "")) +
      geom_boxplot() +
      theme_minimal() +
      guides(alpha = "none") +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        #axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        legend.title = element_text()
      ) +
      labs(
        y = str_wrap(.y, 20),
        x = ""#,
        # y = ""
      )
  ))


p_handover_plots <- bind_rows(p_demog_upstr_handover_1, p_demog_upstr_handover_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
    )
  )) %>% 
  arrange(name) %>%
  pull(plot) 

p_resp_plots <- bind_rows(p_demog_upstr_resp_1, p_demog_upstr_resp_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
    )
  )) %>% 
  arrange(name) %>%
  pull(plot) 


  p_upstream_ptch <-  c(p_handover_plots, p_resp_plots) %>%
  patchwork::wrap_plots(ncol = 2, byrow = FALSE) &
    patchwork::plot_annotation(title = "Demographics for Upstream Impact Analysis",
                               subtitle = "Patients included in ambulance handover time (left) & call response time (right)") 

p_upstream_ptch[[4]] <- p_upstream_ptch[[4]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 

p_upstream_ptch[[5]] <- p_upstream_ptch[[5]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 
p_upstream_ptch[[6]] <- p_upstream_ptch[[6]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 

p_upstream_ptch

ggsave("materials/demog_upstream.png",
       last_plot(),
       bg = "white",
       height = 15,
       width = 20, 
       scale = 0.35)  
