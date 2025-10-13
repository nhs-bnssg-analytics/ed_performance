library(tidyverse)
library(marginaleffects)
library(lme4)
library(lmerTest)
library(scales)


# mort_effect_output <- readRDS("data/mort_effects_output_multilvl.RDS")
# readm_effect_output <- readRDS("data/readm_effects_output_multilvl.RDS")
mort_effect_output <- readRDS("data/mort_effects_output.RDS")
readm_effect_output <- readRDS("data/readm_effects_output.RDS")
mort_effect_plot <- mort_effect_output$mort_effects_plot
readm_effect_plot <- readm_effect_output$readm_effects_plot
mort_margins_plot <- mort_effect_output$mort_margins_plot
readm_margins_plot <- readm_effect_output$readm_marginal_plot
mort_roc_lab <- mort_effect_output$mort_roc_lab
readm_roc_lab <- readm_effect_output$readm_roc_lab
los_effect_output <- readRDS("data/los_outputs_reformulated_multilvl.RDS")
plot_df <- los_effect_output$mod_df
los_condn_plot <- los_effect_output$los_margins_plot


wrap <- 20

ylab_mort <- str_wrap("30-day mortality rate", wrap)
ylab_readm <- str_wrap("30-day readmission rate", wrap)
ylab_los <- str_wrap("Inpatient length of stay", wrap)
xlab <- "Medical boarding time, hours"


# READMISSION plot
# Create a single reference row with covariates fixed at marginal means/modes
ref_data <- datagrid(
  model = readm_effect_output$readm_mod_raw
) %>% 
  slice(1) # Ensure it's just one row of marginal means

# Create the full prediction grid by combining the reference row 
# with all desired dta_time values
time_points <- seq(0, 24, 0.25)
pred_grid <- ref_data %>%
  uncount(length(time_points)) %>% # Repeat the reference row
  mutate(dta_time = rep(time_points, each = 1)) # Add all time points

pred_out <- predict(readm_effect_output$readm_mod_raw, 
                    newdata = pred_grid, 
                    type = "link",
                    se.fit = TRUE,
                    re.form = NA # Crucial for population-average
                    )

# Combine results and calculate CIs/transform (all vectorized)
results_df <- pred_grid %>% 
  select(dta_time) %>%
  mutate(
    log_odds_estimate = pred_out$fit,
    log_odds_se = pred_out$se.fit,
    log_odds_upper = log_odds_estimate + 1.96 * log_odds_se,
    log_odds_lower = log_odds_estimate - 1.96 * log_odds_se,
    # Final transformation to probability (inv.logit is vectorized)
    mean = boot::inv.logit(log_odds_estimate),
    upper = boot::inv.logit(log_odds_upper),
    lower = boot::inv.logit(log_odds_lower)
  )


readm_margins_plot <- ggplot(results_df, aes(x = dta_time, y = mean, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.25) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model-Estimated 30-Day Readmission Rate.", 50)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))


# MORTALITY plot

ref_data <- datagrid(
  model = mort_effect_output$mort_mod_raw
) %>% 
  slice(1) # Ensure it's just one row of marginal means

# Create the full prediction grid by combining the reference row 
# with all desired dta_time values
time_points <- seq(0, 24, 0.25)
pred_grid <- ref_data %>%
  uncount(length(time_points)) %>% # Repeat the reference row
  mutate(dta_time = rep(time_points, each = 1)) # Add all time points

pred_out <- predict(mort_effect_output$mort_mod_raw, 
                    newdata = pred_grid, 
                    type = "link",
                    se.fit = TRUE,
                    re.form = NA # Crucial for population-average
                    )

# Combine results and calculate CIs/transform (all vectorized)
results_df <- pred_grid %>% 
  select(dta_time) %>%
  mutate(
    log_odds_estimate = pred_out$fit,
    log_odds_se = pred_out$se.fit,
    log_odds_upper = log_odds_estimate + 1.96 * log_odds_se,
    log_odds_lower = log_odds_estimate - 1.96 * log_odds_se,
    # Final transformation to probability (inv.logit is vectorized)
    mean = boot::inv.logit(log_odds_estimate),
    upper = boot::inv.logit(log_odds_upper),
    lower = boot::inv.logit(log_odds_lower)
  )


mort_margins_plot <- ggplot(results_df, aes(x = dta_time, y = mean, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.25) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model-Estimated 30-Day Mortality Rate.", 50)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))


patchwork::wrap_plots(
  los_effect_output$los_margins_plot + facet_grid(.~str_wrap("Inpatient length of stay", 25)) + scale_y_continuous(limits = c(0, NA)) + labs(y = "", x = xlab),
  # readm_effect_plot + facet_grid(.~str_wrap("30-day readmission rate", 25)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
  readm_margins_plot + facet_grid(.~str_wrap("30-day readmission rate", 25)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
  mort_margins_plot + facet_grid(.~str_wrap("30-day mortality rate", 25)) + scale_y_continuous(limits = c(0, NA), labels = scales::percent) + labs(y = "", x = xlab),
  nrow = 1, axes = "collect_x")


ggsave("materials/condn_pred_plot_patch_1row_pres_multilvl.png",
       last_plot(),
       bg = "white",
       height = 4.75,
       width = 10, 
       scale = 0.6)



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
browser()
  dplyr::bind_cols(
    broom::tidy(x, exponentiate = exponentiate, conf.int = FALSE),
    # calculate the confidence intervals, and save them in a tibble
    exp(stats::confint(x)) %>%
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



# EFFECTS SIZE TABLE

mort_or <- mort_effect_output$mort_mod_raw %>%
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)

# scale for 4-hour exposure from 1-hour
mort_or <- mort_or %>%
  mutate(
    estimate = estimate ^ 4,
    conf.low = conf.low ^ 4,
    conf.high = conf.high ^ 4
  )

mort_or[1, 1:3] %>% flatten() %>% as.numeric() - 1 
  
mod_df_raw <- readm_effect_output$mod_df
readm_or <- readm_effect_output$readm_mod_raw %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)

# scale for 4-hour exposure from 1-hour
readm_or <- readm_or %>%
  mutate(
    estimate = estimate ^ 4,
    conf.low = conf.low ^ 4,
    conf.high = conf.high ^ 4
  )


readm_or[1, 1:3] %>% flatten() %>% as.numeric() - 1 
    


mod_df <- los_effect_output$mod_df


mod_los_rescaled <- los_effect_output$mod_full %>%
  update(. - los + I(24*los) ~ . -dta_time + I(dta_time/(4)))

los_eff <- mod_los_rescaled %>%
  broom.mixed::tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)

print(los_eff)


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
    `Effect size (95% CI)` = str_c(percent((estimate-1), accuracy = 0.1), " (", percent((conf.low-1), accuracy =  0.1), ", ",  percent((conf.high-1), accuracy = 0.1), ") increase in odds of readmission within 30 days"),
    `P-value` = gtsummary::style_pvalue(p.value)) %>%
  select(Exposure, Outcome, `Effect size (95% CI)`, `P-value`),

mort_or %>%
  mutate(
    Exposure = "+4 hours boarding time for medical patients",
    Outcome = "30-day mortality rate",
    `Effect size (95% CI)` = str_c(percent((estimate-1), accuracy = 0.1), " (", percent((conf.low-1), accuracy =  0.1), ", ",  percent((conf.high-1), accuracy = 0.1), ") increase in odds of mortality within 30 days"),
    `P-value` = gtsummary::style_pvalue(p.value)) %>%
  select(Exposure, Outcome, `Effect size (95% CI)`, `P-value`)
) %>%
  show_in_excel()
