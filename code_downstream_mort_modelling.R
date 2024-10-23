library(tidyverse)
library(tidymodels)

mod_df <- readRDS("data/mort_mod_df_final.RDS")
mod_df <- mod_df %>%
  filter(ed_time < 60*30) %>%
  na.omit()

split_mort <- initial_split(mod_df, strata = mort_30) 
glm_spec <- logistic_reg(penalty = 0) %>% 
  set_engine("glm", model = TRUE) %>% 
  translate()

rec_mort <- recipe(mort_30 ~ ., data = mod_df) %>%
  # step_interact(terms = ~ dta_time:ed_time) %>%
  update_role(ed_time, new_role = "not for prediction") %>%
  update_role(Acuity_Desc, arrival_mode, arrival_cat, new_role = "not for prediction") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_other(all_nominal_predictors(), -matches("arrival_month"), threshold = 0.01) %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_mort <- workflow() %>%
  add_recipe(rec_mort) %>%
  add_model(glm_spec)  


fit_mort <- last_fit(wf_mort, split_mort)

augment(fit_mort) %>%
  roc_curve(mort_30,
            .pred_yes,
            event_level = "second") %>%
  autoplot()


full_fit <- fit(wf_mort, mod_df)

full_fit %>%
  extract_fit_engine() %>%
  broom::tidy() %>% 
  filter(term == "dta_time")


full_fit %>%
  extract_fit_engine() %>%
  broom::tidy() %>% 
  arrange(desc(abs(estimate)))

full_pred = predict(full_fit, mod_df, type = "prob")

yardstick::roc_auc_vec(mod_df$mort_30, estimate = full_pred$.pred_no)
# 
foo <- map_dbl(60 * seq(0, 24, 0.25),
        \(x) mod_df %>%
          mutate(dta_time = x) %>%
          predict(full_fit, new_data = ., type = "prob") %>%
          pull(.pred_yes) %>%
          mean(na.rm = TRUE))



plot(foo)

full_fit %>%
  extract_fit_engine() %>%
  jtools::effect_plot(pred = dta_time, interval = TRUE) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal()


ggplot2::ggsave("materials/mort_model_effects_plot.png",
                last_plot(),
                bg = "white",
                height = 7.5,
                width = 10,
                scale = 0.5)

mod_df_raw <- select(mod_df, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat)

mod_folds <- vfold_cv(mod_df_raw, v = 25)

roc_stability <- map(mod_folds$splits, \(x) {
  mod_raw <- glm(
    formula = mort_30 ~ .
    # + I(dta_time ^ 2)+I(age ^ 2)+I(perf ^ 2)
    + I(age ^ 2)
    + I(perf ^ 2)
    + I(n_ed ^ 2)
    + I(vw_score ^2),
    data = training(x),
    family = binomial(link = "logit")
  )
  full_pred = predict(mod_raw, testing(x), type = "response")
  pROC::roc(testing(x)$mort_30, full_pred)
}) %>%
map(\(x) {
  
  auc = as.numeric(pluck(x, "auc"))
  tibble(
    y = x$sensitivities,
    x = 1- x$specificities,
    auc = auc
  )
  }) %>%
  bind_rows(.id = "id") 


roc_stab <- roc_stability %>%
  select(id, auc) %>%
  distinct() %>%
  summarise(mean_auc = mean(auc), u_auc = quantile(auc, 0.975), l_auc = quantile(auc, 0.025))



mod_raw <- glm(
  formula = mort_30 ~ . 
  # + I(dta_time ^ 2)
  + I(age ^ 2)
  + I(perf ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2),
  data = select(mod_df_raw, -arrival_year),
  family = binomial(link = "logit")
) 



lvl_props <- mod_df %>%
  select(arrival_time, arrival_month, sex, imd) %>%
  map(table) %>%
  map(proportions) %>%
  map(\(x) x[-1]) %>%
  imap(\(x, idx) set_names(x, str_c(idx, names(x)))) %>%
  reduce(c)
  

mod_eff <- effects::predictorEffect("dta_time", mod_raw, fixed.predictors = list(given.values = lvl_props))

mort_roc_lab <- str_c("ROC: ", round(roc_stab[1], 2), " [", round(as.numeric(roc_stab[2]), 2), ", ", round(as.numeric(roc_stab[3]), 2), "]")

mort_effects_plot <- mod_eff[c("x", "fit", "upper", "lower")] %>%
  as.data.frame() %>%
  mutate(across(-dta_time, \(x) mod_eff$link$linkinv(x))) %>%
  ggplot(aes(x = dta_time, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  geom_line() +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal() +
  geom_text(data = tibble(), aes(label = mort_roc_lab, x = 0, y = Inf), vjust = +3, hjust = -0.1)
  

saveRDS(list(mort_mod_raw = mod_raw, mort_effects_plot = mort_effects_plot, mort_roc_lab = mort_roc_lab), "data/mort_effects_output.RDS")


mod_raw %>%
  broom::tidy() %>% 
  filter(str_detect(term, "dta_time"))

raw_pred = predict(mod_raw, mod_df, type = "response")

yardstick::roc_auc_vec(mod_df$mort_30,
                       estimate = raw_pred, 
                       event_level = "second"
                       )


sensitivity_vec(mod_df$mort_30, raw_pred)

mort_effect_plot <- mod_raw %>%
  jtools::effect_plot(pred = "dta_time", interval = TRUE) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model Estimated 30-day Mortality Rate.", 50)) +
  theme_minimal()


ggplot2::ggsave("materials/mort_model_effects_plot.png",
                mort_effect_plot,
                bg = "white",
                height = 7.5,
                width = 10,
                scale = 0.5)

saveRDS(mort_effect_plot, "data/mort_effect_plot.RDS")

mod_raw <- glm(
  formula = mort_30 ~ . 
  # -dta_time
  # + poly(dta_time, 2)
  # + I(dta_time ^ 2)
  + I(age ^ 2)
  + I(perf ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2),
  data = select(mod_df, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat),
  family = binomial(link = "logit")
) 

mod_raw %>%
  broom::tidy() %>% 
  filter(str_detect(term, "dta_time"))

raw_pred = predict(mod_raw, mod_df, type = "response")

yardstick::roc_auc_vec(mod_df$mort_30, estimate = raw_pred, event_level = "second")


mod_raw %>%
  jtools::effect_plot(pred = "dta_time", interval = TRUE) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 120), labels = \(x) x/60, , expand = c(0, 1)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal()




sum_fn <- function(data_points, standard_errors) {
  # Calculate weights
  weights <- 1 / standard_errors^2
  
  # Calculate weighted mean
  weighted_mean <- weighted.mean(data_points, standard_errors, na.rm = TRUE)
  
  # Calculate SEM
  sem <- sqrt(1 / sum(weights, na.rm = TRUE))
  
  # Calculate 95% confidence interval
  lower_bound <- weighted_mean - 1.96 * sem
  upper_bound <- weighted_mean + 1.96 * sem
  c(mean = weighted_mean, l95 = lower_bound, u95 = upper_bound)
}

pred <- predict(mod_raw, newdata = mutate(mod_df, dta_time = 0*60), type = "link", se.fit = TRUE)

critval <- 1.96 ## approx 95% CI
upr <- pred$fit + (critval * pred$se.fit)
lwr <- pred$fit - (critval * pred$se.fit)
fit <- pred$fit

fit2 <- mod_raw$family$linkinv(fit)
upr2 <- mod_raw$family$linkinv(upr)
lwr2 <- mod_raw$family$linkinv(lwr)


map_dfr(
  60 * seq(0, 24, 1),
  \(x){
  pred <- mod_df %>%
    mutate(dta_time = x) %>%
    predict(
      mod_raw,
      newdata = .,
      type = "link",
      se.fit = TRUE
    ) 
  critval <- 1.96 ## approx 95% CI
  upr <- pred$fit + (critval * pred$se.fit)
  lwr <- pred$fit - (critval * pred$se.fit)
  fit <- pred$fit
  # 
  fit2 <- mod_raw$family$linkinv(fit)
  upr2 <- mod_raw$family$linkinv(upr)
  lwr2 <- mod_raw$family$linkinv(lwr)
  
  list(dta_time = x, fit = fit2, u95 = upr2, l95 = lwr2)
  
  # c(dta_time = x, sum_fn(data_points = pred[["fit"]], standard_errors = pred[["se.fit"]]))
  }
  
  # %>%
  #   summarise(
  #     # u = quantile(fit, 0.975, na.rm = TRUE),
  #     # l = quantile(fit, 0.025, na.rm = TRUE),
  #     fit = mean(fit, na.rm = TRUE)
  #   ) %>%
  #   mutate(
  #     dta_time = x,
  #     # u = fit + se.fit,
  #     # l = fit - se.fit
  #   )
) %>%
  ggplot(aes(x = dta_time, y = fit)) +
  geom_boxplot(aes(group = cut_width(dta_time, 60))) +
  # geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.25) +
  # geom_line() +
  scale_x_continuous(breaks = scales::breaks_width(width = 120), labels = \(x) x/60) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal()



effect_plot_fn <- function(fit, var) {
  
  var <- rlang::sym(var)
  
  full_fit %>%
    extract_fit_engine() %>%
    jtools::effect_plot(pred = !!var, interval = TRUE) 
}

rec_mort %>%
  prep() %>%
  juice() %>%
  names() %>% 
  `[`(!. %in% c("mort_30", "arrival_mode", "arrival_cat", "ed_time")) %>%
  map(~effect_plot_fn(full_fit, .x))

# cross validation ROC


## compute mod on kept part
cv_fit <- function(splits, mod, ...) {
  res_mod <-
    fit(mod, mort_30 ~ ., data = analysis(splits))
  return(res_mod)
}

## get predictions on holdout sets
cv_pred <- function(splits, mod){
  # Save the 10%
  holdout <- assessment(splits)
  pred_assess <- bind_cols(truth = holdout$mort_30, predict(mod, new_data = holdout))
  return(pred_assess)
}

## get probs on holdout sets
cv_prob <- function(splits, mod){
  holdout <- assessment(splits)
  prob_assess <- bind_cols(truth = as.factor(holdout$mort_30), 
                           predict(mod, new_data = holdout, type = "prob"))
  return(prob_assess)
}

mort_folds <- vfold_cv(mod_df, v = 25)

fit_cv <- fit_resamples(wf_mort, mort_folds)

res_cv_train <- 
  mort_folds %>% 
  mutate(res_mod = map(splits, .f = cv_fit, mod = glm_spec), ## fit model
         res_pred = map2(splits, res_mod, .f = cv_pred), ## predictions
         res_prob = map2(splits, res_mod, .f = cv_prob)) ## probabilities


res_cv_train %>% 
  mutate(roc = map(res_prob, roc_curve, truth = truth, .pred_no)) %>% 
  mutate(id = seq_along(roc)) %>% 
  unnest(roc) %>%
  ggplot() +
  aes(x = 1 - specificity, y = sensitivity, col = id) +
  geom_path() +
  geom_abline(lty = 3)



mod_df %>%
  mutate(dta_time = dta_time %/% 60) %>%
  summarise(mort = mean(mort_30 == "yes"), .by = dta_time) %>%
  ggplot(aes(x = dta_time, y = mort)) + 
  geom_point() +
  geom_smooth(method = "lm")
  