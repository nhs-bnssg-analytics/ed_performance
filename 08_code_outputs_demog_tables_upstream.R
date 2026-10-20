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

handover_mod_df <- readRDS("data/handover_outputs.RDS") %>%
  `[[`("mod_dat")

resp_mod_df <- readRDS("data/resp_outputs.RDS") %>%
  `[[`("mod_dat")

time_ed_mod_df <- readRDS("data/time_ed_outputs.RDS") %>%
  `[[`("mod_dat")


label_list <- list(
  n_dta_norm = "DTA queue at the patient’s time of arrival as a percentage of cubicle capacity (Majors + Resus)",
  n_ed_norm = "Number in ED (excluding DTA) as a percentage of cubicle capacity (Majors + Resus)",
  age = "Age",
  sex = "Sex",
  imd = "Index of multiple deprivation decile",
  wkend = "Weekend",
  hour = "Time of arrival",
  arrival_month = "Month of arrival"#,
  # arrival_year = "Year of Arrival"
)

mean_pct <- function(x, ...) {
  100*mean(x)
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
    statistic = list(c(n_dta_norm, n_ed_norm) ~ "{mean_pct} ({p25_pct}, {p75_pct})%")
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
  statistic = list(c(n_dta_norm, n_ed_norm) ~ "{mean_pct} ({p25_pct}, {p75_pct})%"))


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
  statistic = list(c(n_dta_norm, n_ed_norm) ~ "{mean_pct} ({p25_pct}, {p75_pct})%"),
  by = "call_category")

tbl_merge(list(time_ed_demo_tbl, handover_demo_tbl, resp_demo_tbl),
          tab_spanner = c("Patients in ED", "Ambulance handovers", "Ambulance recipients")
          ) %>%
  as_gt() %>%
  gt::gtsave("materials/upstream_demog_tab.html")




fct_lvls <- rev(c(
  "(Missing)",
  "1 (least deprived)",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10 (most deprived)",
  # "0-9 years old",
  # "10-19 years old",
  # "20-29 years old",
  "29 years old or under",
  "30-39 years old",
  "40-49 years old",
  "50-59 years old",
  "60-69 years old",
  "70-79 years old",
  "80-89 years old",
  "90-99 years old",
  "100 years old or over",
  "00:00-06:00",
  "06:00-12:00",
  "12:00-18:00",
  "18:00-24:00",
  "Male",
  "Female",
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
))

colour_vals <- c(
  "#97a6c4",
  "lightslategrey",
  "#384860"
  )


p_demog_p1 <- handover_mod_df %>%
  select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
  mutate(impact = "Ambulance handovers") %>%
  bind_rows(resp_mod_df %>%
              select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
              mutate(impact = "999 call respondees")) %>%
  bind_rows(time_ed_mod_df %>%
              select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
              mutate(impact = "ED attendees")) %>%
  # mutate(imd = fct_recode(imd,
  #                         "1\n(least deprived)" = "1 (least deprived)",
  #                         "10\n(least deprived)" = "10 (most deprived)"
  #                         )) %>%
  select(impact,
         Sex = sex,
         Age = age,
         "Index of multiple deprivation decile" = imd,
         `Arrival time` = hour,
         `Arrival month` = arrival_month#, 
         # `Arrival Year` = arrival_year
  ) %>%
  mutate(impact = factor(impact,
                         levels = c(
                                    "Ambulance handovers",
                                    "999 call respondees",
                                    "ED attendees"
                                    ))) %>%
  pivot_longer(-impact) %>%
  group_by(impact, name, value) %>%
  count() %>% 
  mutate(value = factor(value, levels = fct_lvls)) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~ ggplot(.x, aes(y = value, x = n, fill = impact)) +
                       geom_col(position = "dodge") +
                       geom_text(aes(label=number(n, big.mark = ",")), position=position_dodge(width=0.9), hjust=-0.1) +
                       
                       
                       # stat_summary(fun.data = n_fun,
                       #              geom = "text",
                       #              size = 3,
                       #              col = "black"
                       # ) +
                       theme_minimal() +
                       scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
                       scale_fill_manual(values = colour_vals) +
                       guides(alpha = "none") +
                       theme(legend.position = "bottom",
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             axis.text.y = element_text(size = 11),
                             axis.text.x = element_blank(),
                             legend.justification = "center",
                             legend.title = element_text()) +
                       labs(title = str_wrap(.y, 60),
                            x = "",
                            y = "",
                            fill = "Patient cohort:") +
                       expand_limits(x = 1.25*max(.x$n))))



p_demog_p2 <- handover_mod_df %>%
  select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
  mutate(impact = "Ambulance handovers") %>%
  bind_rows(resp_mod_df %>%
              select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
              mutate(impact = "999 call repsondees")) %>%
  bind_rows(time_ed_mod_df %>%
              select(age, sex, imd, wkend, hour, arrival_month, n_dta_norm, n_ed_norm) %>%
              mutate(impact = "ED attendees")) %>%
  select(impact, 
         `DTA queue as a pecentage of cubicle capacity` = n_dta_norm,
         `Number in ED as a pecentage of cubicle capacity` = n_ed_norm) %>%
  pivot_longer(-impact) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~
                       ggplot(.x, aes(x = value, y = "", fill = impact)) +
                       geom_boxplot(show.legend = FALSE) +
                       scale_fill_manual(values = colour_vals) +
                       scale_x_continuous(labels = percent) +
                       theme_minimal() +
                       guides(alpha = "none") +
                       theme(legend.position = "off",
                             legend.justification = "center",
                             axis.text = element_text(size = 11),
                             plot.margin = margin(0, 0, 0, 0, "pt"),
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.line.y = element_blank(),
                             axis.title.y = element_blank(),
                             axis.ticks.length.y = unit(0, "pt"),
                             legend.title = element_text()) +
                       labs(title = str_wrap(.y, 60),
                            x = "",
                            y = "",
                            fill = "Patient cohort:")))


library(patchwork)


ptc_1 <- wrap_plots(p_demog_p1$plot[c(1, 2, 4)], nrow = 1) 
ptc_2 <- wrap_plots(p_demog_p1$plot[c(3, 5)], ncol = 1) 
ptc_3 <- wrap_plots(p_demog_p2$plot, ncol = 1)


wrap_plots(ptc_1, free(ptc_2) | free(ptc_3), ncol = 1, guides = "collect") +
  plot_layout(heights = c(2/4, 2/4)) &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11)) 


ggsave("materials/demog_upstream.png",
       last_plot(),
       bg = "white",
       height = 20,
       width = 20, 
       scale = 0.7)  
