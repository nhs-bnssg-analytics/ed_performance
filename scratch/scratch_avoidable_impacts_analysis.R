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


# empirical distribution of vw_score

rvw <- partial(EnvStats::remp, obs = as.numeric(mod_df_raw$vw_score))

# impute with bagged trees

library(tidymodels)


impute_rec <- recipe(mort_30 ~ ., data = mod_df_raw) %>%
  step_impute_knn(vw_score) %>%
  prep()

n_s <- 100

res_cf <- map_dbl(seq_len(1000), ~{
test_samp <- mod_df_raw %>%
  sample_n(n_s)

test_samp_synth <- 
  test_samp %>%
  mutate(vw_score = NA_real_) %>%
  bake(impute_rec, new_data = .)


raw_cf <- sum(predict(mod, test_samp, type = "response")) - sum(predict(mod, mutate(test_samp, dta_time = 0), type = "response"))
synth_cf <- sum(predict(mod, test_samp_synth, type = "response")) - sum(predict(mod, mutate(test_samp_synth, dta_time = 0), type = "response"))

(raw_cf - synth_cf)/raw_cf
}
)

hist(res_cf)
mean(res_cf)

# synth_cf <- map_dbl(seq_len(1000), ~ {
#   vws <- rvw(n_s)
#   sum(predict(mod, mutate(test_samp, vw_score = vws), type = "response")) - sum(predict(mod, mutate(test_samp, dta_time = 0, vw_score = vws), type = "response"))
# })

raw_cf - synth_cf

eff_full_gap <- map_dbl(1:1000, ~{
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
  
full_gap <- (predict(mod, test_samp, type = "response") %>%  sum()) - (predict(mod, mutate(test_samp, dta_time = 0), type = "response") %>%  sum())
est_gap <- exp(effect) %>% sum()  

full_gap-est_gap
})

library(marginaleffects)

test_samp <- mod_df_raw %>%
  sample_n(1000)

test_samp %>%
  bind_rows(test_samp %>% mutate(dta_time = 0)) %>%
  comparisons(mod, newdata = ., variables = "dta_time") 


predictions(mod, mutate(test_samp, dta_time = 0)) %>%
 as_data_frame() %>%
 pull(estimate) %>%
  sum()

mod_mat <- mod_eff$model.matrix %>%
  as_tibble() %>%
  dplyr::select(-dta_time) %>%
  unique() %>%
  expand_grid(dta_time = mod_df_raw$dta_time) %>%
  relocate(dta_time, .after = which(names(mod$coefficients) == "dta_time")) %>%
  as.matrix()

off <- 0L

scoef <- ifelse(is.na(mod$coefficients), 0L, mod$coefficients)
effect <- off + mod_mat %*% scoef

exp(effect) %>% sum()


# full data
(predict(mod, mod_df_raw, type = "response") %>%  sum()) - (predict(mod, mutate(mod_df_raw, dta_time = 0), type = "response") %>%  sum())
# synthetic data
(predict(mod, mutate(mod_df_raw, vw_score = 6.305284), type = "response") %>%  sum()) - (predict(mod, mutate(mod_df_raw, vw_score = 6.305284, dta_time = 0), type = "response") %>%  sum())



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

