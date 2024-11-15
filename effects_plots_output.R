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

ylab_mort <- str_wrap("30-day mortality rate", wrap)
ylab_readm <- str_wrap("30-day readmission rate", wrap)
ylab_los <- str_wrap("In-hospital length of stay (day)", wrap)
xlab <- "Duration between DTA and admission (hours)"

patchwork::wrap_plots(
                      los_effect_output$los_effect_plot + facet_grid(.~"In-hospital length of stay (day)") + scale_y_continuous(limits = c(2, 16)) + labs(y = "", x = xlab),
                      readm_effect_plot + facet_grid(.~"30-day readmission rate") + scale_y_continuous(limits = c(0.02, 0.16), labels = scales::percent) + labs(y = "", x = xlab),
                      mort_effect_plot + facet_grid(.~"30-day mortality rate") + scale_y_continuous(limits = c(0.02, 0.16), labels = scales::percent) + labs(y = "", x = xlab),
                      nrow = 1, axes = "collect_x")

ggsave("materials/effects_plot_patch_1row.png",
       last_plot(),
       bg = "white",
       height = 7.5,
       width = 10, 
       scale = 0.7)

# mort_effect_output$mort_mod_raw %>%
#   tidy() %>%
#   View()


label_list_downstream <- list(
  dta_time = "Time waiting for admission (hours)",
  perf = "Four-hour breach rate in ED at the patient’s time of arrival",
  `I(perf^2)` = "Four-hour breach rate in ED at the patient’s time of arrival (squared)",
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


reg_table_mort <- mort_effect_output$mort_mod_raw %>% gtsummary::tbl_regression(exponentiate = TRUE,
                                                              estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
  label = label_list_downstream
) %>% gtsummary::add_glance_table(include = c(nobs)) 

# %>%
# gtsummary::as_gt() %>%
# gt::tab_header(
# title = gt::md("**30-day Mortality**")) %>%
#   gt::gtsave("materials/mort_reg_table.html")

reg_table_readm <- readm_effect_output$readm_mod_raw %>% gtsummary::tbl_regression(exponentiate = TRUE,
                                                              estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3),
                                                              label = label_list_downstream
  ) %>% gtsummary::add_glance_table(include = c(nobs))


reg_table_los <- los_effect_output$los_mod %>% gtsummary::tbl_regression(estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 2),
  label = label_list_downstream
) %>% gtsummary::add_glance_table(include = c(nobs)) 

gtsummary::tbl_merge(list(reg_table_mort,
                          reg_table_readm,
                          reg_table_los)) %>%
  gtsummary::as_gt() %>%
  gt::gtsave("materials/reg_table_downstream.html")

# %>%
# gtsummary::as_gt() %>%
# gt::tab_header(
# title = gt::md("**In-Hospital Length of Stay (days)**"))   %>%
#   gt::gtsave("materials/los_reg_table.html")



rr_fn <- function(or, p0) {
  or / ((1-p0)+(p0*or))
}

mod_df_raw <- mort_effect_output$mod_df

mort_or <- mort_effect_output$mort_mod_raw %>%
  update(. ~ . -dta_time + I(dta_time/(4))) %>%
  tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time"))%>%
  select(estimate, conf.low, conf.high, p.value)
  

mod_df <- readm_effect_output$mod_df
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

  