library(tidyverse)

mort_effect_output <- readRDS("data/mort_effects_output.RDS")
readm_effect_output <- readRDS("data/readm_effects_output.RDS")
mort_effect_plot <- mort_effect_output$mort_effects_plot
readm_effect_plot <- readm_effect_output$readm_effects_plot
mort_margins_plot <- mort_effect_output$mort_margins_plot
readm_margins_plot <- readm_effect_output$readm_marginal_plot
mort_roc_lab <- mort_effect_output$mort_roc_lab
readm_roc_lab <- readm_effect_output$readm_roc_lab
los_effect_output <- readRDS("data/los_outputs_reformulated.RDS")
plot_df <- los_effect_output$mod_df
los_condn_plot <- los_effect_output$los_margins_plot


wrap <- 20

ylab_mort <- str_wrap("30-day mortality rate", wrap)
ylab_readm <- str_wrap("30-day readmission rate", wrap)
ylab_los <- str_wrap("Inpatient length of stay", wrap)
xlab <- "Medical boarding time, hours"

patchwork::wrap_plots(
                      los_effect_output$los_effects_plot + facet_grid(.~str_wrap("Inpatient length of stay", 20)) + scale_y_continuous(limits = c(0, NA)) + labs(y = "", x = xlab),
                      readm_effect_plot + facet_grid(.~str_wrap("30-day readmission rate", 20)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
                      mort_effect_plot + facet_grid(.~str_wrap("30-day mortality rate", 20)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
                      nrow = 1, axes = "collect_x")
# patchwork::wrap_plots(
#                       los_effect_output$los_effect_plot + facet_grid(.~"In-hospital length of stay (day)") + scale_y_continuous(limits = c(2, 16)) + labs(y = "", x = xlab),
#                       readm_effect_plot + facet_grid(.~"30-day readmission rate") + scale_y_continuous(limits = c(0.02, 0.16), labels = scales::percent) + labs(y = "", x = xlab),
#                       mort_effect_plot + facet_grid(.~"30-day mortality rate") + scale_y_continuous(limits = c(0.02, 0.16), labels = scales::percent) + labs(y = "", x = xlab),
#                       nrow = 1, axes = "collect_x")

ggsave("materials/effects_plot_patch_1row_pres_202505.png",
       last_plot(),
       bg = "white",
       height = 4.75,
       width = 10, 
       scale = 0.5)

patchwork::wrap_plots(
  los_effect_output$los_margins_plot + facet_grid(.~str_wrap("Inpatient length of stay", 25)) + scale_y_continuous(limits = c(0, NA)) + labs(y = "", x = xlab),
  readm_margins_plot + facet_grid(.~str_wrap("30-day readmission rate", 25)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
  mort_margins_plot + facet_grid(.~str_wrap("30-day mortality rate", 25)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
  nrow = 1, axes = "collect_x")


ggsave("materials/condn_pred_plot_patch_1row_pres_202505.png",
       last_plot(),
       bg = "white",
       height = 4.75,
       width = 10, 
       scale = 0.6)

# mort_effect_output$mort_mod_raw %>%
#   tidy() %>%
#   View()


label_list_downstream <- list(
  dta_time = "Time waiting for admission (hours)",
  time_to_dta = "Time to decision to admit (hours)",
  `I(time_to_dta^2)` = "Time to decision to admit (hours, squared)",
  # perf = "Four-hour breach rate in ED at the patient’s time of arrival",
  # `I(perf^2)` = "Four-hour breach rate in ED at the patient’s time of arrival (squared)",
  nctr = "Number of patients with no criteria to reside as a proportion of ED bedded capacity (at time of event)",
  `I(nctr^2)` = "Number of patients with no criteria to reside as a proportion of ED bedded capacity (at time of event, squared)",
  age = "Age",
  sex = "Sex",
  vw_score = "Elixhauser comorbidity index (vWs)",
  `I(vw_score^2)` = "Elixhauser comorbidity index (vWs) (squared)",
   n_ed = "Number of previous ED attendances",
  `I(n_ed^2)` = "Number of previous ED attendances (squared)",
  imd = "Index of multiple deprivation decile",
  wkend = "Weekend",
  arrival_time = "Time of arrival",
  arrival_month = "Month of arrival"
)


my_tidy <- function(x, exponentiate =  TRUE, conf.level = 0.95, ...) {

  dplyr::bind_cols(
    broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
    # calculate the confidence intervals, and save them in a tibble
    exp(stats::confint.default(x)) %>%
      tibble::as_tibble() %>%
      rlang::set_names(c("conf.low", "conf.high"))  )
}

reg_table_mort <-
  mort_effect_output$mort_mod_raw %>%
  gtsummary::tbl_regression(
    intercept = TRUE,
    tidy_fun = my_tidy,
    exponentiate = TRUE,
    estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
    label = label_list_downstream
  ) %>%
  gtsummary::add_glance_table(include = c(nobs))


# %>%
#     gtsummary::modify_fmt_fun(
#       # p.value = label_style_pvalue(digits = 3),
#       c(conf.high) ~ function(x) scales::scientific(x),
#       rows = variable == "n_ed"
#     )

# gtsummary::as_gt() %>%
# gt::tab_header(
# title = gt::md("**30-day Mortality**")) %>%
#   gt::gtsave("materials/mort_reg_table.html")

reg_table_readm <- readm_effect_output$readm_mod_raw %>% gtsummary::tbl_regression(exponentiate = TRUE,     intercept = TRUE,
                                                              estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
                                                              label = label_list_downstream
  ) %>% gtsummary::add_glance_table(include = c(nobs))


reg_table_los <- los_effect_output$mod_full %>%
  gtsummary::tbl_regression(intercept = TRUE,estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 2),
  label = label_list_downstream
) %>% gtsummary::add_glance_table(include = c(nobs)) 

gtsummary::tbl_merge(list(
                          reg_table_los,
                          reg_table_readm,
                          reg_table_mort
  )) %>%
  gtsummary::as_gt() %>%
  gt::gtsave("materials/reg_table_downstream_202505.html")


# Relative risks

rr_fn <- function(or, p0) {
  or / ((1-p0)+(p0*or))
}

mod_df_raw <- mort_effect_output$mod_df

mort_or <- mort_effect_output$mort_mod_raw %>%
  update(. ~ . -dta_time + I(dta_time/(4))) %>%
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(str_detect(term, "dta_time"))%>%
  select(estimate, conf.low, conf.high, p.value)

mort_or[1, 1:3] %>% flatten() %>% as.numeric() - 1 
  

mod_df_raw <- readm_effect_output$mod_df
readm_or <- readm_effect_output$readm_mod_raw %>%
  update(. ~ . -dta_time + I(dta_time/(4))) %>%
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)


readm_or[1, 1:3] %>% flatten() %>% as.numeric() - 1 
    
# mort_or %>%
#   mutate(across(-p.value, exp)) %>%
#   mutate(across(-p.value, \(x) 100*(rr_fn(x, 0.04)-1), .names = "rr_{.col}"))
# 
# readm_or %>%
#   mutate(across(-p.value, exp)) %>%
#   mutate(across(-p.value, \(x) 100*(rr_fn(x, 0.1)-1), .names = "rr_{.col}"))


mod_df <- los_effect_output$mod_df

los_eff <- los_effect_output$mod_full %>%
  update(. - los + I(24*los) ~ . -dta_time + I(dta_time/(4))) %>%
  broom::tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)

bind_rows(
los_eff %>%
  mutate(
    Exposure = "+4 hours boarding time for medical patients",
    Outcome = "Inpatient lenght of stay",
    `Effect size (95% CI)` = str_c(round(estimate, 1), " (", round(conf.low, 1), ", ",  round(conf.high, 1), ") hours additional LOS"),
    `P-value` = gtsummary::style_pvalue(p.value)) %>%
  select(Exposure, Outcome, `Effect size (95% CI)`, `P-value`),

readm_or %>%
  mutate(
    Exposure = "+4 hours boarding time for medical patients",
    Outcome = "30-day readmission rate",
    `Effect size (95% CI)` = str_c(percent(estimate-1, accuracy = 0.1), " (", percent(conf.low-1, accuracy =  0.1), ", ",  percent(conf.high-1, accuracy = 0.1), ") increase in odds of readmission within 30 days"),
    `P-value` = gtsummary::style_pvalue(p.value)) %>%
  select(Exposure, Outcome, `Effect size (95% CI)`, `P-value`),

mort_or %>%
  mutate(
    Exposure = "+4 hours boarding time for medical patients",
    Outcome = "30-day mortality rate",
    `Effect size (95% CI)` = str_c(percent(estimate-1, accuracy = 0.1), " (", percent(conf.low-1, accuracy =  0.1), ", ",  percent(conf.high-1, accuracy = 0.1), ") increase in odds of mortality within 30 days"),
    `P-value` = gtsummary::style_pvalue(p.value)) %>%
  select(Exposure, Outcome, `Effect size (95% CI)`, `P-value`)
) %>%
  show_in_excel()
  