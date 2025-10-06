require(tidyverse)
require(RODBC)
require(lubridate)
library(feasts)
library(tidymodels)



source("theme.R")
source("colour_functions.R")

con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

# report_start <- '2021-06-01'
# report_end <- '2024-06-01'

report_start <- '2023-07-01'
report_end <- '2025-04-01'
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



dta_df <-  readRDS("data/upstream_dta_df.RDS")
imd_df <- readRDS("data/upstream_imd_df.RDS")
ed_df <-  readRDS("data/upstream_ed_df.RDS")
trt_df <-  readRDS("data/upstream_trt_df.RDS")
handover_df <-  readRDS("data/upstream_handover_df.RDS")
resp_time_df <-  readRDS("data/upstream_resp_time_df.RDS")


dta_df <- dta_df %>%
  filter(!(site == "WESTON GENERAL HOSPITAL" & hour(dt) %in% c(10:12, 1:7))) 

# handover_rows_1 <- nrow(filter(handover_df, !is.na(nhs_number)))

handover_df <- handover_df %>%
  na.omit() %>%
  ungroup() %>%
  mutate(imd = factor(imd, levels = 1:10, labels = c("1 (least deprived)",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "5",
                                                     "6",
                                                     "7",
                                                     "8",
                                                     "9",
                                                     "10 (most deprived)"))
  ) %>%
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)")) %>%
  mutate(imd = fct_relevel(imd, "(Missing)", after = Inf)) %>%
  mutate(age = fct_recode(age, 
                          "29 years old or under" = "0-4 Years Old",
                          "29 years old or under" = "5-9 Years Old",
                          "29 years old or under" = "10-14 Years Old",
                          "29 years old or under" = "15-19 Years Old",
                          "29 years old or under" = "20-24 Years Old",
                          "29 years old or under" = "25-29 Years Old",
                          "30-39 years old" = "30-34 Years Old",
                          "30-39 years old" = "35-39 Years Old",
                          "40-49 years old" = "40-44 Years Old",
                          "40-49 years old" = "45-49 Years Old",
                          "50-59 years old" = "50-54 Years Old",
                          "50-59 years old" = "55-59 Years Old",
                          "60-69 years old" = "60-64 Years Old",
                          "60-69 years old" = "65-69 Years Old",
                          "70-79 years old" = "70-74 Years Old",
                          "70-79 years old" = "75-79 Years Old",
                          "80-89 years old" = "80-84 Years Old",
                          "80-89 years old" = "85-89 Years Old",
                          "90-99 years old" = "90-94 Years Old",
                          "90-99 years old" = "95-99 Years Old",
                          "100 years old or over" = "100 Years Old Or Over"
  )) %>%
  mutate(age = fct_relevel(age, "100 years old or over", after = 10)) 

# handover_rows_2 <- nrow(handover_df)

# resp_rows_1 <- 
#   resp_time_df %>%
#     filter(!is.na(nhs_number), nhs_number %in% dat_attr$nhs_number) %>%
#     nest(.by = call_category) %>%
#     arrange(call_category) %>%
#     mutate(n = map_dbl(data, nrow)) %>%
#     pull(n)
#   

resp_time_df <- resp_time_df %>%
  na.omit() %>%
  filter(sex %in% c("Male", "Female")) %>%
  mutate(imd = factor(imd, levels = 1:10, labels = c("1 (least deprived)",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "5",
                                                     "6",
                                                     "7",
                                                     "8",
                                                     "9",
                                                     "10 (most deprived)"))
  ) %>%
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)")) %>%
  mutate(age = fct_recode(age, 
                          "29 years old or under" = "0-4 Years Old",
                          "29 years old or under" = "5-9 Years Old",
                          "29 years old or under" = "10-14 Years Old",
                          "29 years old or under" = "15-19 Years Old",
                          "29 years old or under" = "20-24 Years Old",
                          "29 years old or under" = "25-29 Years Old",
                          "30-39 years old" = "30-34 Years Old",
                          "30-39 years old" = "35-39 Years Old",
                          "40-49 years old" = "40-44 Years Old",
                          "40-49 years old" = "45-49 Years Old",
                          "50-59 years old" = "50-54 Years Old",
                          "50-59 years old" = "55-59 Years Old",
                          "60-69 years old" = "60-64 Years Old",
                          "60-69 years old" = "65-69 Years Old",
                          "70-79 years old" = "70-74 Years Old",
                          "70-79 years old" = "75-79 Years Old",
                          "80-89 years old" = "80-84 Years Old",
                          "80-89 years old" = "85-89 Years Old",
                          "90-99 years old" = "90-94 Years Old",
                          "90-99 years old" = "95-99 Years Old",
                          "100 years old or over" = "100 Years Old Or Over"
  )) %>%
  mutate(age = fct_relevel(age, "100 years old or over", after = 10)) 

# resp_rows_2 <- resp_time_df %>%
#   nest(.by = call_category) %>%
#   arrange(call_category) %>%
#   mutate(n = map_dbl(data, nrow)) %>%
#   pull(n)

# 1 - (resp_rows_2/resp_rows_1)

# trt_rows_1 <- nrow(trt_df)

trt_df <- trt_df %>%
  na.omit() %>%
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
  mutate(imd = fct_na_value_to_level(imd, level = "(Missing)"))  %>%
  mutate(sex = factor(ifelse(sex == "1", "Male", "Female"))) %>%
  filter(age < 120) %>%
  mutate(age = cut(age, breaks = c(0, seq(30, 100, 10), Inf), right = FALSE, labels = c(
    "29 years old or under" ,
    "30-39 years old" ,
    "40-49 years old" ,
    "50-59 years old" ,
    "60-69 years old" ,
    "70-79 years old" ,
    "80-89 years old" ,
    "90-99 years old" ,
    "100 years old or over" 
  )))


reg_tab_labels <- list(
  n_dta_norm = "Number of medical boarders, as a percentage of ED bedded capacity",
  n_ed_norm = "ED occupancy excluding boarders medical, as a percentage of ED bedded capacity",
  `I(n_ed_norm^2)` = "ED occupancy excluding boarders medical, as a percentage of ED bedded capacity (squared)",
  sex = "Sex",
  age = "Age",
  imd = "Deprivation decile",
  wkend = "Event on weekend",
  hour = "Time of arrival",
  arrival_month = "Month of arrival"#,
  # arrival_year = "Year of Arrival"
)

 
# trt_rows_2 <- nrow(trt_df)

# Normalisation and standardisation

n_dta_cutoff <- 1.1
bin_acc <- 0.05

handover_outputs <- local({
  
  
  handover_df <- handover_df %>% select(-nhs_number)
  
  # bin_acc <- 0.1
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Standardised ambulance handover\ntime (min)"
  
  # Non standardised plot
  p_raw <-   dta_df %>%
    left_join(handover_df) %>%
    left_join(site_resus_maj) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    mutate(n_dta_norm = n_dta/total,
           standard = 30) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    # distinct() %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    ggplot(aes(x = n_dta_norm, y = handover_time/60)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_x_continuous(labels = scales::percent) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    labs(title = "",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))
  
  
  
  # Standardised plot  
  mod_dat <- dta_df %>% 
    left_join(handover_df) %>%
    left_join(site_resus_maj) %>%
    left_join(ed_df) %>%
    mutate(n_ed_norm = (n_ed - n_dta)/total,
           n_dta_norm = n_dta / total) %>%
    mutate(n_dta_norm = n_dta/total) %>%
    mutate(age = factor(age)) %>%
    ungroup() %>%
    na.omit() %>%
    # filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    filter(handover_time > 0) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
    mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                             "06:00-11:59" = "06:00:00",
                             "12:00-17:59" = "12:00:00",
                             "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
    # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt, -maj, -resus, -total) %>%
    ungroup() 
  
  # handover_mod <- lm(handover_time ~ . -n_dta -n_dta_norm -n_ed -site, data = mod_dat)
  handover_mod <- lm(handover_time ~ age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -site, data = mod_dat)
  handover_pred <- predict(handover_mod, mod_dat)
  
  std_mod <- handover_mod
  
  std_mod_tab <- gtsummary::tbl_regression(
    intercept = TRUE,
    std_mod,
    label = list(
      age = "Age",
      imd = "Index of multiple deprivation decile",
      wkend = "Weekend",
      hour = "Time of arrival",
      arrival_month = "Month of arrival"#,
      # arrival_year = "Year of Arrival"
    )
  ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Ambulance Handover Time (min)**')))#,
  
  full_mod <- lm(handover_time/60 ~ n_dta_norm + age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -site, data = mod_dat)
  
  # This code assesses which transform is best?
  
  
  full_mod_tab <-  full_mod %>%
    gtsummary::tbl_regression(
      intercept = TRUE,
      label = reg_tab_labels
    ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) # |>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Ambulance handover time (min)**')))
  
  
  
  mod_dat <- mod_dat %>%
    mutate(handover_standard = handover_pred,
           handover_adj = handover_time - handover_standard,
           handover_adj_inv = handover_adj + mean(handover_time)) 
  
  plot_df <- mod_dat %>%
    # filter(n_dta <= 35) 
    filter(n_dta_norm <= n_dta_cutoff)
  
  
  p_std <- plot_df %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    mutate(standard = 30) %>%
    filter(n_dta_norm >= 0.1) %>%
    ggplot(aes(
      x = n_dta_norm,
      y = handover_adj_inv / 60,
      group = cut_width(n_dta_norm, width = bin_acc)
    )) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(outliers = FALSE, fatten = NULL) +
    # scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_point(aes(y = standard), col = "red") +
    geom_hline(aes(yintercept = standard),
               col = "red",
               linetype = 2) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(col = "Mean (standardised data)", shape = "Mean (standardised data)")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.95),
      geom = "point",
      col = "black",
      aes(col = "Comparator quantile (standardised data)", shape = "Comparator quantile (standardised data)")
    ) +
    # bnssgtheme() +
    theme_minimal() +
    scale_colour_manual(
      # breaks = c("Mean", "Comparator quantile (standardised data)", "National target"),
      values = c("Mean" = "black", "Comparator quantile (standardised data)"= "black", "National target" = "red"),
      # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
    ) +
    scale_x_continuous(labels = scales::percent,
                       breaks = scales::breaks_width(0.5)) +
    coord_cartesian(xlim = c(0.1, NA)) +
    scale_shape_manual(values = c(17, 19),
                       breaks = c("Mean (standardised data)", "Comparator quantile (standardised data)"),
                       # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
    ) +
    labs(title = "", 
         y = y_lab,
         x = x_lab,
         shape = "",
         colour = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.text = element_text(size = 11),
          legend.title = element_blank(),
          legend.spacing.x = unit(0, "cm")) +
    guides(shape = guide_legend(order = 1),
           colour = guide_legend(order = 2))
  
  
  
  list(
    p_raw = p_raw,
    p_std = p_std,
    mod_dat = mod_dat,
    plot_df = plot_df,
    std_mod = std_mod,
    full_mod = full_mod,
    std_mod_tab = std_mod_tab,
    full_mod_tab = full_mod_tab
  )
  
})

saveRDS(handover_outputs, "data/handover_outputs.RDS")

resp_outputs <- local({
  
  resp_time_df <- resp_time_df %>% select(-nhs_number)
  
  # bin_acc <- 0.05
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Standardised ambulance response time\n(min)"
  
  labels <- c(cat1 = "999 Call Category 1",
              cat2 = "999 Call Category 2",
              cat3 = "999 Call Category 3")
  
  time_std <- tibble(call_category = c("cat1", "cat2", "cat3"),
                     std = c(15, 40, 120))
  
  # non normalised x-axis
  p_raw_raw <- dta_df %>%
    left_join(resp_time_df) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    filter(n_dta <= 35) %>%
    distinct() %>%
    # mutate(n_dta = factor(n_dta)) %>%
    ggplot(aes(x = n_dta, y = response)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta, width = 1)), outliers = FALSE, fatten = NULL) +
    geom_point(
      data = {time_std %>% expand_grid(n_dta = 0:35)},
      aes(y = std, col = "National target"),
    ) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(col = "Mean", shape = "Mean")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.9),
      geom = "point",
      col = "black",
      aes(col = "Mean", shape = "90th percentile")
    ) +
    facet_wrap(
      vars(call_category),
      ncol = 1,
      scales = "free",
      labeller = as_labeller(labels)
    ) +
    # scale_x_continuous(limits = c(0, 35)) +
    theme_minimal() +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    guides(alpha = "none") +
    labs(
      title = "Raw outcome, un-normalised dependant",
      y = y_lab,
      x = "DTA queue",
      shape = "",
      col = ""
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.spacing.x = unit(0, "cm")
    )
  
  p_norm_raw <- dta_df %>%
    ungroup() %>%
    # filter(n_dta <= 35) %>%
    # na.omit() %>%
    left_join(site_resus_maj) %>%
    mutate(n_dta_norm = n_dta / total) %>%
    group_by(dt) %>%
    summarise(n_dta_norm = mean(n_dta_norm, na.rm = TRUE)) %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    filter(n_dta_norm > 0, n_dta_norm <= n_dta_cutoff) %>%
    left_join(resp_time_df)  %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    distinct() %>%
    # mutate(n_dta = factor(n_dta)) %>%
    ggplot(aes(x = n_dta_norm, y = response)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(
      mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)),
      outliers = FALSE,
      fatten = NULL
    ) +
    geom_point(
      data = {time_std %>% expand_grid(n_dta_norm = seq(0, 1.75, bin_acc))},
      aes(y = std, col = "National target"),
    ) +
    stat_summary(
      fun = ~ mean(.x),
      geom = "point",
      col = "black",
      aes(shape = "Mean")
    ) +
    stat_summary(
      fun = ~ quantile(.x, 0.9),
      geom = "point",
      col = "black",
      aes(shape = "90th percentile")
    ) +
    facet_wrap(
      vars(call_category),
      ncol = 1,
      scales = "free",
      labeller = as_labeller(labels)
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal()  +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    guides(alpha = "none") +
    labs(
      title = "Raw outcome, normalised dependant",
      y = y_lab,
      x = x_lab,
      shape = "",
      col = ""
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text()
    )
  
  
  # y standardisation
  
  mod_dat <- dta_df %>%
    ungroup() %>%
    filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    left_join(site_resus_maj) %>%
    left_join(ed_df) %>%
    # filter(n_dta <= 35) %>%
    mutate(n_ed_norm = (n_ed - n_dta)/total,
           n_dta_norm = n_dta / total
    ) %>%
    group_by(dt) %>%
    summarise(
      n_dta_norm = mean(n_dta_norm, na.rm = TRUE),
      n_ed_norm = mean(n_ed_norm, na.rm = TRUE),
    ) %>%
    left_join(resp_time_df) %>%
    na.omit() %>%
    filter(n_dta_norm > 0, n_dta_norm <= n_dta_cutoff) %>%
    distinct() %>%
    ungroup() %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
    mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                             "06:00-11:59" = "06:00:00",
                             "12:00-17:59" = "12:00:00",
                             "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
    # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt) %>%
    ungroup()
  
  resp_mods <- 
    mod_dat %>%
    group_by(call_category) %>%
    nest() %>%
    mutate(mod = map(data, ~ lm(response ~ age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -n_dta_norm -outcome,  data = .x))) %>%
    mutate(mod_full = map(data, ~ lm(response ~ + n_dta_norm + age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -outcome, data = .x))) %>%
    mutate(pred = map2(mod, data, predict)) %>%
    mutate(data = pmap(list(data, call_category, pred), ~bind_cols(..1, call_category = ..2, response_pred = ..3)))
  
  # resp_mod <- lm(response ~ . - n_dta_norm - outcome, data = mod_dat)
  # resp_pred <- predict(resp_mod, mod_dat)
  
  
  std_mod_tabs <- resp_mods %>%
    ungroup() %>%
    mutate(call_category = recode(call_category, cat1 = "Category 1", cat2 = "Category 2", cat3 = "Category 3")) %>%
    select(call_category, mod) %>%
    mutate(tab = map2(mod, call_category, ~{
      gtsummary::tbl_regression(.x,
                                intercept = TRUE,
                                label = list(
                                  age = "Age",
                                  sex = "Sex",
                                  imd = "Index of multiple deprivation decile",
                                  wkend = "Weekend",
                                  hour = "Time of arrival",
                                  arrival_month = "Month of arrival"
                                )) %>%
        gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
      # gtsummary::as_gt() %>%
      # gt::tab_header(title = gt::md(glue::glue('**{.y} Ambulance response time (min)**')))
    })) %>%
    ungroup() %>%
    select(call_category, tab) %>%
    as.list() 
  
  
  full_mod_tabs <- resp_mods %>%
    ungroup() %>%
    mutate(call_category = recode(call_category, cat1 = "Category 1", cat2 = "Category 2", cat3 = "Category 3")) %>%
    select(call_category, mod_full) %>%
    mutate(tab = map2(mod_full, call_category, ~{
      gtsummary::tbl_regression(.x,
                                intercept = TRUE,
                                label = reg_tab_labels
                                ) %>%
        gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
      # gtsummary::as_gt() %>%
      # gt::tab_header(title = gt::md(glue::glue('**{.y} Ambulance response time (min)**')))
    })) %>%
    ungroup() %>%
    select(call_category, tab) %>%
    as.list() 
  
  
  plot_dat <- resp_mods %>% 
    pull(data) %>%
    bind_rows() %>%
    mutate(response_adj = response - response_pred) %>%
    mutate(response_adj_inv = response_adj + mean(response), .by = call_category)
  
  p_norm_std <- plot_dat %>%
    # mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
    filter(n_dta_norm >= 0.1) %>%
    group_by(call_category) %>%
    nest() %>%
    mutate(plot = map2(data, call_category, \(x, y) {x %>%
        ggplot(aes(x = n_dta_norm, y = response_adj_inv)) +
        # geom_boxplot(outliers = FALSE) +
        geom_boxplot(
          mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)),
          outliers = FALSE,
          fatten = NULL
        ) +
        geom_hline(data = filter(time_std, call_category == y), aes(yintercept = std), linetype = 2, col = "red") +
        # stat_summary(
        #   fun = ~ mean(.x),
        #   geom = "errorbar",
        #   col = "black",
        #   width = 0.035,
        #   aes(ymax = ..y.., ymin = ..y..)
        # ) +
        geom_point(
          data = {filter(time_std, call_category == y) %>% expand_grid(n_dta_norm = seq(bin_acc, n_dta_cutoff, bin_acc))},
          aes(y = std, col = "National target"),
        ) +
        stat_summary(
          fun = ~ mean(.x),
          geom = "point",
          col = "black",
          aes(shape = "Mean (standardised data)")
        ) +
        stat_summary(
          fun = ~ quantile(.x, 0.9),
          geom = "point",
          col = "black",
          aes(shape = "Comparator quantile (standardised data)")
        ) +
        # facet_wrap(
        #   vars(call_category),
        #   ncol = 1,
        #   scales = "free",
        #   labeller = as_labeller(labels)
        # ) +
        scale_x_continuous(labels = scales::percent, breaks = scales::breaks_width(0.5)) +
        coord_cartesian(xlim = c(0.1, NA)) +
        theme_minimal()  +
        scale_colour_manual(values = c("red", "black"),
                            # guide = guide_legend(override.aes = list(size = 3,alpha = 1))
        ) +
        scale_shape_manual(breaks = c("Mean (standardised data)", "Comparator quantile (standardised data)"),
                           values = c(17, 19),
                           # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
        ) +
        facet_grid(str_c("999 Category ", str_replace(y, "cat", ""), " call")~.) +
        guides(alpha = "none") +
        labs(
          title = "",
          y = y_lab,
          x = x_lab,
          shape = "",
          col = ""
        ) +
        theme(
          legend.position = "bottom",
          legend.justification = "center",
          legend.text = element_text(size = 11),
          legend.title = element_blank()
        )
    })) %>%
    pull(plot)
  
  
  list(
    p_raw_raw = p_raw_raw,
    p_norm_raw = p_norm_raw,
    p_norm_std = p_norm_std,
    plot_dat = plot_dat,
    std_mod = set_names(resp_mods$mod, resp_mods$call_category),
    full_mod = set_names(resp_mods$mod_full, resp_mods$call_category),
    mod_dat = mod_dat,
    std_mod_tabs = std_mod_tabs,
    full_mod_tabs = full_mod_tabs
  )
  
})

saveRDS(resp_outputs, "data/resp_outputs.RDS")

time_ed_outputs <- local({
  
  
  # bin_acc <- 0.1
  
  x_lab <- "DTA queue as a percentage of cubicle capacity (Majors + Resus)"
  y_lab <- "Time in ED \n(min)"
  
  # Non standardised plot
  p_raw <-   dta_df %>%
    left_join(trt_df) %>%
    left_join(site_resus_maj) %>%
    # filter(site == "SOUTHMEAD HOSPITAL") %>%
    # filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    na.omit() %>%
    mutate(n_dta_norm = n_dta/total,
           standard = 60) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    # distinct() %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    ggplot(aes(x = n_dta_norm, y = departure_time)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    # geom_smooth(method = lm) +
    scale_y_continuous(breaks = breaks_width(60)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_x_continuous(labels = scales::percent) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    labs(title = "Raw outcome, normalised dependant",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))
  
  # ED performance
  dta_df %>%
    left_join(trt_df) %>%
    left_join(site_resus_maj) %>%
    mutate(n_dta_norm = n_dta /
             total) %>%
    group_by(`queue over 50%` = n_dta_norm > 0.5) %>%
    summarise(`4-hour performance` = mean(departure_time<4*60, na.rm = TRUE))
  
  # Standardised plot  
  mod_dat <- dta_df %>% 
    left_join(trt_df) %>%
    left_join(site_resus_maj) %>%
    left_join(ed_df) %>%
    mutate(n_ed_norm = (n_ed - n_dta)/total,
           n_dta_norm = n_dta / total) %>%
    mutate(n_dta_norm = n_dta/total) %>%
    ungroup() %>%
    na.omit() %>%
    # filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
    filter(departure_time >= 0) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    mutate(wkend = factor(ifelse(wday(dt, week_start = 1) < 6, "No", "Yes")),
           hour = factor(hms::as_hms(floor_date(dt, "6 hour")))) %>%
    mutate(hour = fct_recode(hour, "00:00-05:59" = "00:00:00",
                             "06:00-11:59" = "06:00:00",
                             "12:00-17:59" = "12:00:00",
                             "18:00-23:59" = "18:00:00")) %>%
    mutate(arrival_month = factor(month(dt), labels = month.name)) %>%
    # mutate(arrival_year = factor(year(dt))) %>%
    select(-dt, -maj, -resus, -total) %>%
    ungroup() 
  
  trt_mod <- lm(departure_time ~ age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -site, data = mod_dat)
  trt_pred <- predict(trt_mod, mod_dat)
  
  tibble(n_dta_norm = mod_dat$n_dta_norm, departure_time = trt_pred) %>%
    filter(n_dta_norm <= n_dta_cutoff) %>%
    mutate(standard = 60) %>%
    # distinct() %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    ggplot(aes(x = n_dta_norm, y = departure_time)) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
    # geom_hline(yintercept = 30, linetype = 2) +
    # geom_smooth(method = lm) +
    scale_y_continuous(breaks = breaks_width(60)) +
    # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = q), col = NA, alpha = 0.25) +
    # geom_line(aes(y = fit, group = q)) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_x_continuous(labels = scales::percent) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    labs(title = "Raw outcome, normalised dependant",
         y = y_lab,
         x = x_lab,
         shape = "",
         col = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))
  
  std_mod <- trt_mod
  
  std_mod_tab <- gtsummary::tbl_regression(
    std_mod,
    intercept = TRUE,
    label = list(
      age = "Age",
      sex = "Sex",
      imd = "Index of multiple deprivation decile",
      wkend = "Weekend",
      hour = "Time of arrival",
      arrival_month = "Month of arrival"#,
      # arrival_year = "Year of Arrival"
    )
  ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Time in ED (min)**')))#,
  
  full_mod <- lm(departure_time ~ n_dta_norm + age + sex + imd + n_ed_norm + I(n_ed_norm^2) + wkend + hour + arrival_month -site, data = mod_dat)
  
  # This code assesses which transform is best?
  
  
  
  full_mod_tab <-  full_mod %>%
    gtsummary::tbl_regression(
      intercept = TRUE,
      label = reg_tab_labels
    ) %>%
    gtsummary::add_glance_table(include = c(nobs, r.squared)) #|>
  # gtsummary::as_gt() %>%
  # gt::tab_header(title = gt::md(glue::glue('**Time in ED (min)**')))
  
  
  
  mod_dat <- mod_dat %>%
    mutate(trt_standard = trt_pred,
           trt_adj = departure_time - trt_standard,
           trt_adj_inv = trt_adj + mean(departure_time)) 
  
  plot_df <- mod_dat %>%
    # filter(n_dta <= 35) 
    filter(n_dta_norm <= n_dta_cutoff)
  
  
  
  p_std <- plot_df %>%
    mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = ceiling)) %>%
    mutate(standard = 4*60) %>%
    filter(n_dta_norm >= 0.1) %>%
    ggplot(aes(x = n_dta_norm, y = trt_adj_inv, group = cut_width(n_dta_norm, width = bin_acc))) +
    # geom_boxplot(outliers = FALSE) +
    geom_boxplot(outliers = FALSE, fatten = NULL) +
    # scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30)) +
    # geom_point(aes(y = standard), col = "red") +
    geom_hline(aes(yintercept = standard), col = "red", linetype = 2) +
    geom_point(aes(y = standard, col = "National target")) +
    stat_summary(fun = ~mean(.x), geom = "point", col = "black", aes(col = "Mean", shape = "Mean")) +
    stat_summary(fun = ~quantile(.x, 0.95), geom = "point", col = "black", aes(col = "95th percentile", shape = "95th percentile")) +
    # bnssgtheme() +
    theme_minimal() +
    scale_shape_manual(values = c(21, 24, 19), guide = guide_legend(override.aes = list(size = 3,
                                                                                        alpha = 1))) +
    scale_colour_manual(values = c("red", "black"), guide = guide_legend(override.aes = list(size = 3,
                                                                                             alpha = 1))) +
    scale_y_continuous(breaks = breaks_width(120)) +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_width(0.5)) +
    coord_cartesian(xlim = c(0.1, NA)) +
    scale_shape_manual(values = c(19, 17),
                       guide = guide_legend(override.aes = list(size = 3,
                                                                alpha = 1))) +
    labs(title = "",
         y = y_lab,
         x = x_lab,
         shape = "",
         colour = "") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.spacing.x = unit(0, "cm"))
  
  
  
  list(
    p_raw = p_raw,
    p_std = p_std,
    plot_df = plot_df,
    full_mod = full_mod,
    mod_dat = mod_dat,
    std_mod = std_mod,
    std_mod_tab = std_mod_tab,
    full_mod_tab = full_mod_tab
  )
  
  
})

saveRDS(time_ed_outputs, "data/time_ed_outputs.RDS")
