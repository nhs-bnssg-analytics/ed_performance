library(tidyverse)

mort_effect_output <- readRDS("data/mort_effects_output.RDS")
readm_effect_output <- readRDS("data/readm_effects_output.RDS")
mort_effect_plot <- mort_effect_output$mort_effects_plot
readm_effect_plot <- readm_effect_output$readm_effects_plot
mort_roc_lab <- mort_effect_output$mort_roc_lab
readm_roc_lab <- readm_effect_output$readm_roc_lab
los_effect_output <- readRDS("data/los_effect_output.RDS")
plot_df <- los_effect_output$los_mod_df


wrap <- 30

ylab_mort <- str_wrap("Model-Estimated 30-day Mortality Rate.", wrap)
ylab_readm <- str_wrap("Model-Estimated 30-day Readmission Rate.", wrap)
ylab_los <- str_wrap("Model-Estimated In-Hospital Length of Stay (day).", wrap)



patchwork::wrap_plots(mort_effect_plot + labs(y = ylab_mort),
                      readm_effect_plot + labs(y = ylab_readm),
                      los_effect_output$los_effect_plot + labs(y = ylab_los),
                      ncol = 1, axes = "collect")

ggsave("materials/effects_plot_patch.png",
       last_plot(),
       bg = "white",
       height = 12.5,
       width = 10, 
       scale = 0.5)

# mort_effect_output$mort_mod_raw %>%
#   tidy() %>%
#   View()


mort_effect_output$mort_mod_raw %>% gtsummary::tbl_regression(exponentiate = TRUE,
                                                              estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
  label = list(
    dta_time = "Hours waiting for admission",
    arrival_time = "Time of arrival",
    arrival_month = "Month of arrival",
    # arrival_year = "Year of arrival",
    perf = "4-hour ED performance",
    `I(perf^2)` = "4-hour ED performance (squared)",
    age = "Age",
    `I(age^2)` = "Age (squared)",
    n_ed = "Number of previous ED attendances",
    `I(n_ed^2)` = "Number of previous ED attendances (squared)",
    sex = "Sex",
    imd = "IMD",
    vw_score = "Van Walraven score",
    `I(vw_score^2)` = "Van Walraven score (squared)"
    # investigation = "Investigation description (encoded)",
    # diagnoses = "Diagnoses description (encoded)"
  )
) %>%
gtsummary::add_glance_table(include = c(nobs)) %>%
gtsummary::as_gt() %>%
gt::tab_header(
title = gt::md("**30-day Mortality**")) %>%
  gt::gtsave("materials/mort_reg_table.html")

readm_effect_output$readm_mod_raw %>% gtsummary::tbl_regression(exponentiate = TRUE,
                                                              estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
  label = list(
    dta_time = "Hours waiting for admission",
    arrival_time = "Time of arrival",
    arrival_month = "Month of arrival",
    # arrival_year = "Year of arrival",
    perf = "4-hour ED performance",
    `I(perf^2)` = "4-hour ED performance (squared)",
    age = "Age",
    `I(age^2)` = "Age (squared)",
    n_ed = "Number of previous ED attendances",
    `I(n_ed^2)` = "Number of previous ED attendances (squared)",
    sex = "Sex",
    imd = "IMD",
    vw_score = "Van Walraven score",
    `I(vw_score^2)` = "Van Walraven score (squared)"
    # investigation = "Investigation description (encoded)",
    # diagnoses = "Diagnoses description (encoded)"
  )
) %>%
gtsummary::add_glance_table(include = c(nobs)) %>%
gtsummary::as_gt() %>%
gt::tab_header(
title = gt::md("**30-day Readmission**"))  %>%
  gt::gtsave("materials/readm_reg_table.html")

los_effect_output$los_mod %>% gtsummary::tbl_regression(estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 2),
  label = list(
    dta_time = "Hours waiting for admission",
    arrival_time = "Time of arrival",
    arrival_month = "Month of arrival",
    # arrival_year = "Year of arrival",
    perf = "4-hour ED performance",
    `I(perf^2)` = "4-hour ED performance (squared)",
    age = "Age",
    `I(age^2)` = "Age (squared)",
    n_ed = "Number of previous ED attendances",
    `I(n_ed^2)` = "Number of previous ED attendances (squared)",
    sex = "Sex",
    imd = "IMD",
    vw_score = "Van Walraven score",
    `I(vw_score^2)` = "Van Walraven score (squared)"
    # investigation = "Investigation description (encoded)",
    # diagnoses = "Diagnoses description (encoded)"
  )
) %>%
gtsummary::add_glance_table(include = c(nobs)) %>%
gtsummary::as_gt() %>%
gt::tab_header(
title = gt::md("**In-Hospital Length of Stay (days)**"))   %>%
  gt::gtsave("materials/los_reg_table.html")



rr_fn <- function(or, p0) {
  or / ((1-p0)+(p0*or))
}

mort_or <- mort_effect_output$mort_mod_raw %>%
  update(. ~ . -dta_time + I(dta_time/(4))) %>%
  tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time"))%>%
  select(estimate, conf.low, conf.high, p.value)
  
readm_or <- readm_effect_output$readm_mod_raw %>%
  update(. ~ . -dta_time + I(dta_time/(4))) %>%
  tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time"))%>%
  select(estimate, conf.low, conf.high, p.value)

    
mort_or %>%
  mutate(across(-p.value, exp)) %>%
  mutate(across(-p.value, \(x) 100*(rr_fn(x, 0.04)-1), .names = "rr_{.col}"))

readm_or %>%
  mutate(across(-p.value, exp)) %>%
  mutate(across(-p.value, \(x) 100*(rr_fn(x, 0.1)-1), .names = "rr_{.col}"))


los_effect_output$los_mod %>%
  update(. - los + I(24*los) ~ . -dta_time + I(dta_time/(4))) %>%
  tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time"))

  