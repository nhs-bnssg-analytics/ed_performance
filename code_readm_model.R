library(tidyverse)
library(tidymodels)

mod_df <- readRDS("data/mort_mod_df_final.RDS")
mod_df <- mod_df %>%
  filter(ed_time < 60*30) %>%
  na.omit()

mod_df <- mod_df %>%
  filter(ed_time < 60*30) %>%
  mutate(readm = factor(ifelse(readm, "yes", "no"))) %>%
  select(-mort_30, -los, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat, -arrival_year)

mod_folds <- vfold_cv(mod_df, v = 25)

roc_stability <- map(mod_folds$splits, \(x) {
  mod_raw <- glm(
    formula = readm ~ . 
    # -n_ed
    + I(age ^ 2)
    + I(perf ^ 2)
    + I(n_ed ^ 2)
    + I(vw_score ^2),
    data = training(x),
    family = binomial(link = "logit")
  )
  full_pred = predict(mod_raw, testing(x), type = "response")
  pROC::roc(testing(x)$readm, full_pred)
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
  formula = readm ~ . 
  # -n_ed
  + I(age ^ 2)
  + I(perf ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2),
  data = mod_df,
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

readm_roc_lab <- str_c("ROC: ", round(roc_stab[1], 2), " [", round(as.numeric(roc_stab[2]), 2), ", ", round(as.numeric(roc_stab[3]), 2), "]")

readm_effects_plot <- mod_eff[c("x", "fit", "upper", "lower")] %>%
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
       y = str_wrap("Model-Estimated 30-Day Readmission Rate.", 50)) +
  theme_minimal() +
  geom_text(data = tibble(), aes(label = readm_roc_lab, x = 0, y = Inf), vjust = +3, hjust = -0.1)

saveRDS(list(readm_mod_raw = mod_raw, readm_effects_plot = readm_effects_plot, readm_roc_lab = readm_roc_lab), "data/readm_effects_output.RDS")
