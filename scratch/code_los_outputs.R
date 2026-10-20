library(tidyverse)
library(tidymodels)

dat_adm <- readRDS("data/dat_adm_final.RDS")


t_max <- 24*60
dat_adm <- dat_adm %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  filter(sex %in% 1:2) %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  mutate(imd = factor(imd)) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_time = fct_recode(arrival_time,
                                   "00:00-06:00" = "00:00:00",
                                   "06:00-12:00" = "06:00:00",
                                   "12:00-18:00" = "12:00:00",
                                   "18:00-24:00" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    los,
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    age,
    n_ed,
    Acuity_Desc,
    arrival_mode,
    arrival_cat,
    sex,
    imd,
    vw_score,
    dta_time,
    ed_time
  ) %>%
  distinct() 



los_outputs <- local({
  
  plot_df <- dat_adm %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    distinct() %>%
    filter(between(dta_time, 0, 60 * 25)) 
  
  p_raw <- plot_df %>%
    mutate(dta_time = factor(dta_time %/% 60, levels = 0:24)) %>%
    ggplot(aes(
      x = dta_time,
      y = los
    )) +
    geom_boxplot(outliers = FALSE,
                 fatten = NULL) +
    stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
    theme_minimal() +
    scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE)]}) +
    # scale_y_continuous(limits = c(0, 25), breaks = seq(0, 45, 10)) +
    guides(alpha = "none") +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text()
    ) +
    labs(
      shape = "",
      x = "Time waiting for admission (hour)",
      y = "In-hospital length of stay (day)",
      colour = "Total length of stay"
    )
  
  mod_std <- lm(los ~ . 
                + I(perf^2)
                + I(age^2)
                + I(vw_score^2)
                + I(n_ed^2)
                -arrival_cat
                -dta_time
                -ed_time
                , plot_df)
  
  reg_tab_std <- gtsummary::tbl_regression(mod_std,
                                           label = list(
                                             arrival_time = "Time of arrival",
                                             arrival_month = "Month of arrival",
                                             arrival_year = "Year of arrival",
                                             arrival_cat = "Arrival category",
                                             arrival_mode = "Arrival mode",
                                             perf = "4-hour ED performance",
                                             `I(perf^2)` = "4-hour ED performance (squared)",
                                             age = "Age",
                                             `I(age^2)` = "Age (squared)",
                                             n_ed = "Number of previous ED attendances",
                                             `I(n_ed^2)` = "Number of previous ED attendances (squared)",
                                             sex = "Sex",
                                             imd = "IMD",
                                             vw_score = "Van Walraven score",
                                             `I(vw_score^2)` = "Van Walraven score (squared)",
                                             Acuity_Desc = "Acuity description"#,
                                             # investigation = "Investigation description (encoded)",
                                             # diagnoses = "Diagnoses description (encoded)"
                                           )) %>%
    gtsummary::add_glance_table(include = c(nobs)) %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
      title = gt::md("**In Hospital Length of Stay (days)**"))
  
  mod_full <- lm(los ~ . 
                 + I(perf^2)
                 + I(age^2)
                 + I(vw_score^2)
                 + I(n_ed^2)
                 -arrival_cat
                 -ed_time
                 , {plot_df %>% mutate(dta_time = dta_time/60)})
  
  reg_tab_full <- gtsummary::tbl_regression(
    mod_full,
    label = list(
      dta_time = "Hours spent DTA",                                    
      arrival_time = "Time of arrival",
      arrival_month = "Month of arrival",
      arrival_year = "Year of arrival",
      arrival_cat = "Arrival category",
      arrival_mode = "Arrival mode",
      perf = "4-hour ED performance",
      `I(perf^2)` = "4-hour ED performance (squared)",
      age = "Age",
      `I(age^2)` = "Age (squared)",
      n_ed = "Number of previous ED attendances",
      `I(n_ed^2)` = "Number of previous ED attendances (squared)",
      sex = "Sex",
      imd = "IMD",
      vw_score = "Van Walraven score",
      `I(vw_score^2)` = "Van Walraven score (squared)",
      Acuity_Desc = "Acuity description"
    )) %>%
    gtsummary::add_glance_table(include = c(nobs)) %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
      title = gt::md("**In Hospital Length of Stay (days)**"))
  
  los_pred <- predict(mod_std, plot_df)
  
  p_std <- plot_df %>%
    mutate(los_adj = los - los_pred,
           los_adj_inv = los_adj + mean(los, na.rm = TRUE),
           dta_time = factor(dta_time %/% 60, levels = 0:24)) %>%
    ggplot(aes(
      x = dta_time,
      y = los_adj_inv,
    )) +
    geom_boxplot(outliers = FALSE,
                 fatten = NULL) +
    stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(shape = "Mean")) +
    theme_minimal() +
    scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE)]}) +
    # scale_y_continuous(limits = c(0, 25), breaks = seq(0, 45, 10)) +
    guides(alpha = "none") +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text()
    ) +
    labs(
      shape = "",
      x = "Time waiting for admission (hour)",
      y = "In-hospital length of stay (day)",
      colour = "Total length of stay"
    )
  
  list(p_raw = p_raw, 
       p_std = p_std,
       mod_std = mod_std,
       mod_full = mod_full,
       reg_tab_std = reg_tab_std,
       reg_tab_full = reg_tab_full)
  
})

saveRDS(los_outputs, "data/los_outputs.RDS")

