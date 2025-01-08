library(tidyverse)

# DOWNSTREAM IMPACTS

mort_effect_output <- readRDS("data/mort_effects_output.RDS")
readm_effect_output <- readRDS("data/readm_effects_output.RDS")
los_effect_output <- readRDS("data/los_effect_output.RDS")


mod <- mort_effect_output$mort_mod_raw

mod_df_raw <- mort_effect_output$mod_df


lvl_props <- mod_df_raw %>%
  select(arrival_time, wkend, age, arrival_month, sex, imd) %>%
  map(table) %>%
  map(proportions) %>%
  map(\(x) x[-1]) %>%
  imap(\(x, idx) set_names(x, str_c(idx, names(x)))) %>%
  reduce(c)

mod_eff <- effects::predictorEffect("dta_time", mod, fixed.predictors = list(given.values = lvl_props))




set.seed(123)
test_samp <- mod_df_raw %>%
  sample_n(1000)

pred_loc <- which(names(mod$coefficients) == "dta_time") 

mod_mat <- mod_eff$model.matrix %>%
  as_tibble() %>%
  dplyr::select(-dta_time) %>%
  unique() %>%
  expand_grid(dta_time = test_samp$dta_time) %>%
  relocate(dta_time, .after = which(names(mod$coefficients) == "dta_time")) %>%
  as.matrix()

off <- 0L

scoef <- ifelse(is.na(mod$coefficients), 0L, mod$coefficients)
effect <- off + mod_mat %*% scoef

exp(effect) %>% sum()


# standardised mortality model
mod_std <- mod %>%
  update(. ~ . -dta_time)


expect_mort <- predict(mod_std, test_samp, type = "response") %>%
  sum()

actual_mort <- sum(test_samp$mort_30 == "yes")

smr <- actual_mort/expect_mort

expected_mort_rate = expect_mort/nrow(test_samp)
actual_mort_rate = actual_mort/nrow(test_samp)
nnh = 1/(actual_mort_rate - expected_mort_rate)

