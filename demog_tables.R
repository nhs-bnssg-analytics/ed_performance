# refs: van Walraven paper: https://pubmed.ncbi.nlm.nih.gov/19433995/

require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)
library(comorbidity)
library(gtsummary)

source("theme.R")
source("colour_functions.R")


n_fun <- function(x){
  return(data.frame(y = x, label = paste0(scales::number(x, big.mark = ","))))
}


# UPSTREAM TABLES

handover_mod_df <- readRDS("data/handover_outputs.RDS") %>%
  `[[`("mod_dat") %>%
  mutate(imd = fct_drop(imd)) %>%
  mutate(sex = factor(sex)) %>%
  rename(arrival_time = hour)

resp_mod_df <- readRDS("data/resp_outputs.RDS") %>%
  `[[`("mod_dat") %>%
  mutate(imd = fct_drop(imd)) %>%
  mutate(sex = factor(sex)) %>%
  rename(arrival_time = hour)

time_ed_mod_df <- readRDS("data/time_ed_outputs.RDS") %>%
  `[[`("mod_dat") %>%
  mutate(imd = fct_drop(imd)) %>%
  rename(arrival_time = hour)


label_list <- list(
  n_dta_norm = "DTA queue at the patient’s time of arrival as a percentage of cubicle capacity",
  n_ed_norm = "Number in ED (excluding DTA) as a percentage of cubicle capacity",
  age = "Age",
  sex = "Sex",
  imd = "Index of multiple deprivation decile",
  wkend = "Weekend",
  arrival_time = "Time of arrival",
  arrival_month = "Month of arrival"#,
  # arrival_year = "Year of Arrival"
)

median_pct <- function(x, ...) {
  100*median(x)
}

iqr_pct <- function(x, ...) {
  100*IQR(x)
}

p25_pct <- function(x, ...) {
  100*quantile(x, 0.25) 
}

p75_pct <- function(x, ...) {
  100*quantile(x, 0.75) 
}


handover_demo_tbl <- handover_mod_df %>%
  tbl_summary(include = c(
    n_dta_norm,
    n_ed_norm,
    age,
    sex,
    imd,
    hour,
    arrival_month,
    wkend
  ),
  digits = list(n_dta_norm = 1,
                n_ed_norm = 1),
  label = label_list,
  statistic = list(c(n_dta_norm, n_ed_norm) ~ "{median_pct} ({p25_pct}, {p75_pct})%")
  )

time_ed_demo_tbl <- time_ed_mod_df %>%
  tbl_summary(include = c(
    n_dta_norm,
    n_ed_norm,
    age,
    sex,
    imd,
    hour,
    arrival_month,
    wkend
  ),
  digits = list(n_dta_norm = 1,
                n_ed_norm = 1),
  label = label_list,
  statistic = list(c(n_dta_norm, n_ed_norm) ~ "{median_pct} ({p25_pct}, {p75_pct})%"))


resp_demo_tbl <- resp_mod_df %>%
  mutate(call_category = recode(call_category,
                                "cat1" = "999 Category 1",
                                "cat2" = "999 Category 2",
                                "cat3" = "999 Category 3",
  )) %>%
  tbl_summary(include = c(
    n_dta_norm,
    n_ed_norm,
    age,
    sex,
    imd,
    hour,
    arrival_month,
    wkend
  ),
  digits = list(n_dta_norm = 1,
                n_ed_norm = 1),
  label = label_list,
  statistic = list(c(n_dta_norm, n_ed_norm) ~ "{median_pct} ({p25_pct}, {p75_pct})%"),
  by = "call_category")

tbl_merge(list(time_ed_demo_tbl, handover_demo_tbl, resp_demo_tbl),
          tab_spanner = c("Patients in ED, non-admits", "Ambulance handovers", "Ambulance reponses")
) %>%
  as_gt() %>%
  gt::gtsave("materials/upstream_demog_tab.html")


# DOWNSTREAM TABLES

downstream_mod_df <- readRDS("data/mort_effects_output.RDS") %>%
  `[[`("mod_df")

label_list_downstream <- list(
  dta_time = "Time waiting for admission (hours)",
  perf = "Four-hour breach rate in ED at the patient’s time of arrival",
  age = "Age",
  sex = "Sex",
  vw_score = "Elixhauser comorbidity index (vWs)",
  imd = "Index of multiple deprivation decile",
  wkend = "Weekend",
  arrival_time = "Time of arrival",
  arrival_month = "Month of arrival"#,
  # arrival_year = "Year of Arrival"
)


downstream_demo_tbl <- tbl_summary(downstream_mod_df,
            include = c(dta_time,
                        perf,
                        age,
                        sex,
                        vw_score,
                        imd,
                        arrival_time,
                        arrival_month,
                        n_ed,
                        wkend),
            label = label_list_downstream,
            digits = list(perf = 1),
            statistic = list(c(perf) ~ "{median_pct} ({p25_pct}, {p75_pct})%")
            ) 





tbl_merge(
  list(resp_demo_tbl, handover_demo_tbl, time_ed_demo_tbl, downstream_demo_tbl),
          tab_spanner = c(
                          "Ambulance responses",
                          "Ambulance handovers",
                          "Patients in ED, non-admits",
                          "IPLOS, 30-day readmission rate, 30-day mortality rate")
) %>%
  as_gt() %>%
  gt::gtsave("materials/merged_demog_tab.html")
