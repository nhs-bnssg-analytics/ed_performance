library(tidyverse)
library(tidymodels)
library(lme4)

# mod_df <- readRDS("data/mort_mod_df_202505.RDS")
mod_df <- readRDS("data/mort_mod_df_multilvl.RDS")
mod_df <- mod_df %>%
  mutate(dta_time = dta_time/60) %>% # convert minutes to hours
  mutate(time_to_dta = time_to_dta/60) %>% # convert minutes to hours
  # filter(ed_time < 60*30) %>%
  mutate(age = cut(age, breaks = c(0 , seq(30, 100, 10), Inf), right = FALSE, labels = c(
    "29 years old or under",
    # "20-29 years old" ,
    "30-39 years old" ,
    "40-49 years old" ,
    "50-59 years old" ,
    "60-69 years old" ,
    "70-79 years old" ,
    "80-89 years old" ,
    "90-99 years old" ,
    "100 years old or over" 
  ))) %>%
  
  na.omit() 


mod_df$dta_time %>% cut(breaks = seq(0, 24, 4)) %>% table() %>% proportions() %>% `*`(100)


# Mortality model
{

mod_df_raw <- select(mod_df, -readm, -los, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat, -arrival_year)

# mod_folds <- vfold_cv(mod_df_raw, v = 25)

# roc_stability <- map(mod_folds$splits, \(x) {
#   mod_raw <- glm(
#     formula = mort_30 ~ .
#     # + I(dta_time ^ 2)+I(age ^ 2)+I(perf ^ 2)
#     # + I(age ^ 2)
#     + I(nctr ^ 2)
#     + I(time_to_dta ^ 2)
#     # + I(bed_occ ^ 2)
#     + I(n_ed ^ 2)
#     + I(vw_score ^2),
#     data = training(x),
#     family = binomial(link = "logit")
#   )
#   full_pred = predict(mod_raw, testing(x), type = "response")
#   pROC::roc(testing(x)$mort_30, full_pred)
# }) %>%
# map(\(x) {
  
#   auc = as.numeric(pluck(x, "auc"))
#   tibble(
#     y = x$sensitivities,
#     x = 1- x$specificities,
#     auc = auc
#   )
#   }) %>%
#   bind_rows(.id = "id") 


# roc_stab <- roc_stability %>%
#   select(id, auc) %>%
#   distinct() %>%
#   summarise(mean_auc = mean(auc), u_auc = quantile(auc, 0.975), l_auc = quantile(auc, 0.025))


 
  
mod_raw <- glmer(mort_30 ~ 
  + dta_time
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2) 
  + (1 | nhs_number), 
                    data = mod_df_raw,#  data = sample_n(mod_df_raw, 10000), 
                     family = binomial,
                    control = lme4::glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

nnh_or <- glmer(mort_30 ~
   -dta_time
  + I(dta_time > 4) # NNH based on waiting more or less than 4 hr
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2) 
  + (1 | nhs_number),
  data = mod_df_raw,
  family = binomial,
  control = lme4::glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
)  %>%
    broom.mixed::tidy(exponentiate = TRUE) %>%
    filter(term == "I(dta_time > 4)TRUE") %>%
    pull(estimate)

peer <- mod_df %>%
  filter(dta_time <= 4) %>%
  pull(mort_30) %>%
  table() %>%
  proportions() %>%
  `[[`("yes")

nnh <- (peer * (nnh_or -1))+1/(peer * (nnh_or -1)*(1-peer))


lvl_props <- mod_df %>%
  select(arrival_time, wkend, arrival_month, age, sex, imd) %>%
  map(table) %>%
  map(proportions) %>%
  map(\(x) x[-1]) %>%
  imap(\(x, idx) set_names(x, str_c(idx, names(x)))) %>%
  reduce(c)

# # remove new base-level because mort analysis doesn't have any admits under 13
# lvl_props <- lvl_props[setdiff(names(lvl_props), c("age10-19 years old"))]
# lvl_props <- lvl_props[setdiff(names(lvl_props), c("age10-19 years old", "age20-29 years old"))]


mod_eff <- effects::predictorEffect("dta_time", mod_raw, fixed.predictors = list(given.values = lvl_props))


# mort_roc_lab <- str_c("ROC: ", round(roc_stab[1], 2), " [", round(as.numeric(roc_stab[2]), 2), ", ", round(as.numeric(roc_stab[3]), 2), "]")

mort_effects_plot <- mod_eff[c("x", "fit", "upper", "lower")] %>%
  as.data.frame() %>%
  mutate(across(-dta_time, \(x) mod_eff$link$linkinv(x))) %>%
  ggplot(aes(x = dta_time, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, 24), breaks = scales::breaks_width(width = 4),  expand = c(0, 0)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
  # geom_text(data = tibble(), aes(label = mort_roc_lab, x = 0, y = Inf), vjust = +3, hjust = -0.1)


mort_margins_plot <-marginaleffects::plot_predictions(mod_raw, condition = "dta_time", draw = FALSE) %>%
  ggplot(aes(x = dta_time, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.25) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
  # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
  scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
  labs(x = "Time spent DTA (hours)",
       # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
       y = str_wrap("Model estimated 30-day mortality rate.", 50)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))

  

saveRDS(list(mod_df = mod_df_raw,
             mort_mod_raw = mod_raw,
             nnh = nnh,
             mort_effects_plot = mort_effects_plot,
             mort_margins_plot = mort_margins_plot#,
            #  mort_roc_lab = mort_roc_lab
            ),
              "data/mort_effects_output_multilvl.RDS")

}

# Readmission model
{
  mod_df_raw <- mod_df %>%
    mutate(readm = factor(ifelse(readm, "yes", "no"))) %>%
    select(-mort_30, -los, -ed_time, -Acuity_Desc, -arrival_mode, -arrival_cat, -arrival_year)
  
  # mod_folds <- vfold_cv(mod_df_raw, v = 25)
  
  # roc_stability <- map(mod_folds$splits, \(x) {
  #   mod_raw <- glm(
  #     formula = readm ~ . 
  #     # -n_ed
  #     # + I(age ^ 2)
  #     + I(nctr ^ 2)
  #     # + I(bed_occ ^ 2)
  #     + I(n_ed ^ 2)
  #     + I(vw_score ^2),
  #     data = training(x),
  #     family = binomial(link = "logit")
  #   )
  #   full_pred = predict(mod_raw, testing(x), type = "response")
  #   pROC::roc(testing(x)$readm, full_pred)
  # }) %>%
  #   map(\(x) {
      
  #     auc = as.numeric(pluck(x, "auc"))
  #     tibble(
  #       y = x$sensitivities,
  #       x = 1- x$specificities,
  #       auc = auc
  #     )
  #   }) %>%
  #   bind_rows(.id = "id") 
  
  
  # roc_stab <- roc_stability %>%
  #   select(id, auc) %>%
  #   distinct() %>%
  #   summarise(mean_auc = mean(auc), u_auc = quantile(auc, 0.975), l_auc = quantile(auc, 0.025))
  

mod_raw <- glmer(readm ~ 
  + dta_time
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2) 
  + (1 | nhs_number), 
                    data = mod_df_raw,#  data = sample_n(mod_df_raw, 10000), 
                     family = binomial,
                    control = lme4::glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

nnh_or <- glmer(readm ~
   -dta_time
  + I(dta_time > 4) # NNH based on waiting more or less than 4 hr
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2) 
  + (1 | nhs_number),
  data = mod_df_raw,
  family = binomial,
  control = lme4::glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
)  %>%
  coefficients() %>%
  `[[`("nhs_number") %>%
  `[[`("I(dta_time > 4)TRUE") %>%
  `[[`(1) %>%
  exp()
  
  # %>%
  #   broom::tidy(exponentiate = TRUE) %>%
  #   filter(term == "I(dta_time > 4)TRUE") %>%
  #   pull(estimate)


  peer <- mod_df %>%
    filter(dta_time <= 4) %>%
    pull(mort_30) %>%
    table() %>%
    proportions() %>%
    `[[`("yes")
  
  nnh <- (peer * (nnh_or -1))+1/(peer * (nnh_or -1)*(1-peer))
  
  lvl_props <- mod_df_raw %>%
    select(arrival_time, wkend, arrival_month, sex, imd) %>%
    map(table) %>%
    map(proportions) %>%
    map(\(x) x[-1]) %>%
    imap(\(x, idx) set_names(x, str_c(idx, names(x)))) %>%
    reduce(c)
  
  
  mod_eff <- effects::predictorEffect("dta_time", mod_raw, fixed.predictors = list(given.values = lvl_props))
  
  # readm_roc_lab <- str_c("ROC: ", round(roc_stab[1], 2), " [", round(as.numeric(roc_stab[2]), 2), ", ", round(as.numeric(roc_stab[3]), 2), "]")
  
  readm_effects_plot <- mod_eff[c("x", "fit", "upper", "lower")] %>%
    as.data.frame() %>%
    mutate(across(-dta_time, \(x) mod_eff$link$linkinv(x))) %>%
    ggplot(aes(x = dta_time, y = fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    geom_line(linewidth = 1) +
    scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
    # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
    scale_y_continuous(labels = \(x) scales::percent(x, accuracy = 1), breaks = scales::breaks_width(width = 0.01)) +
    labs(x = "Time spent DTA (hours)",
         # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
         y = str_wrap("Model-Estimated 30-Day Readmission Rate.", 50)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
  
  
  readm_marginal_plot <- marginaleffects::plot_predictions(mod_raw, condition = "dta_time", draw = FALSE) %>%
  ggplot(aes(x = dta_time, y = estimate, ymin = conf.low, ymax = conf.high)) +
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
  
  saveRDS(list(mod_df = mod_df_raw,
               readm_mod_raw = mod_raw,
               nnh = nnh,
               readm_effects_plot = readm_effects_plot,
               readm_marginal_plot = readm_marginal_plot#,
              #  readm_roc_lab = readm_roc_lab
              ), "data/readm_effects_output_multilvl.RDS")
  
}

# LOS model

# {

  
  los_outputs <- local({
    

    
    mod_df_raw <- mod_df %>%
      select(-mort_30, -readm)
    
    mod_df <- mod_df_raw
    
    
    p_raw <- mod_df %>%
      filter(dta_time >=1) %>%
      mutate(dta_time = factor(dta_time %/% 1, levels = seq(0, 24, 1))) %>%
      ggplot(aes(
        x = dta_time,
        y = los
      )) +
      # geom_boxplot(outliers = FALSE,
      #              fatten = NULL
      #              ) +
      # stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
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
    
    mod_std <- lm(los ~   + dta_time
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(n_ed ^ 2)
  + I(vw_score ^2) 
                  , mod_df)
    
    
    
    p_raw <- mod_df %>%
      mutate(los = predict(mod_std, new_data = mod_df)) %>%
      filter(dta_time >=1) %>%
      mutate(dta_time = factor(dta_time %/% 1, levels = seq(0, 24, 1))) %>%
      ggplot(aes(
        x = dta_time,
        y = los
      )) +
      # geom_boxplot(outliers = FALSE,
      #              fatten = NULL
      # ) +
      # stat_summary(fun = ~quantile(.x, 0.90), geom = "point", col = "black", aes(shape = "90th percentile")) +
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
    
    reg_tab_std <- gtsummary::tbl_regression(mod_std,
                                             label = list(
                                               arrival_time = "Time of arrival",
                                               arrival_month = "Month of arrival",
                                               # arrival_year = "Year of arrival",
                                               time_to_dta = "Time to DTA",
                                               `I(time_to_dta^2)` = "Time to DTA (squared)",
                                               # bed_occ = "Percentage of bed stock occupied",
                                               # `I(bed_occ^2)` = "Percentage of bed stock occupied (squared)",
                                               age = "Age",
                                               # `I(age^2)` = "Age (squared)",
                                               n_ed = "Number of previous ED attendances",
                                               `I(n_ed^2)` = "Number of previous ED attendances (squared)",
                                               sex = "Sex",
                                               imd = "IMD",
                                               vw_score = "Van Walraven score",
                                               `I(vw_score^2)` = "Van Walraven score (squared)"
                                               # investigation = "Investigation description (encoded)",
                                               # diagnoses = "Diagnoses description (encoded)"
                                             )) %>%
      gtsummary::add_glance_table(include = c(nobs)) %>%
      gtsummary::as_gt() %>%
      gt::tab_header(
        title = gt::md("**In Hospital Length of Stay (days)**"))
    
    mod_df <- mod_df %>% mutate(dta_time = dta_time)
    
    
    mod_full <- lmer(los ~  
                   dta_time
                   + age
                   + sex
                   + imd
                   + vw_score
                   + nctr
                   # + poly(nctr, 2, raw = TRUE)
                   # + bed_occ
                   + time_to_dta
                   + n_ed
                   + arrival_time
                   + arrival_month
                   + wkend
                   + I(n_ed^2)
                   + I(nctr ^ 2)
                   # + I(bed_occ^2)
                   + I(time_to_dta^2)
                   + I(vw_score^2) +
                    (1|nhs_number)
                   , mod_df)
    
    reg_tab_full <- gtsummary::tbl_regression(
      mod_full,
      label = list(
        dta_time = "Hours waiting for admission",                                    
        arrival_time = "Time of arrival",
        wkend = "Weekend",
        arrival_month = "Month of arrival",
        # arrival_year = "Year of arrival",
        time_to_dta = "Time to DTA",
        `I(time_to_dta^2)` = "Time to DTA (squared)",
        # bed_occ = "Percentage of bed stock occupied",
        # `I(bed_occ^2)` = "Percentage of bed stock occupied (squared)",
        age = "Age",
        # `I(age^2)` = "Age (squared)",
        n_ed = "Number of previous ED attendances",
        `I(n_ed^2)` = "Number of previous ED attendances (squared)",
        sex = "Sex",
        imd = "IMD",
        vw_score = "Van Walraven score",
        `I(vw_score^2)` = "Van Walraven score (squared)"
        # investigation = "Investigation description (encoded)",
        # diagnoses = "Diagnoses description (encoded)"
      )) %>%
      gtsummary::add_glance_table(include = c(nobs)) %>%
      gtsummary::as_gt() %>%
      gt::tab_header(
        title = gt::md("**In Hospital Length of Stay (days)**"))
    
    los_pred <- predict(mod_std, mod_df)
    
    p_std <- mod_df %>%
      mutate(los_adj = los_pred, 
             dta_time = factor(dta_time %/% 1, levels = 0:24)) %>%
      ggplot(aes(
        x = dta_time,
        y = los - los_adj
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
    
    
    lvl_props <- mod_df %>%
      select(arrival_time, wkend, age, arrival_month, sex, imd) %>%
      map(table) %>%
      map(proportions) %>%
      map(\(x) x[-1]) %>%
      imap(\(x, idx) set_names(x, str_c(idx, names(x)))) %>%
      reduce(c)
    
    mod_eff <- effects::predictorEffect("dta_time", mod_full, fixed.predictors = list(given.values = lvl_props))
    
    
    los_effects_plot <- mod_eff[c("x", "fit", "upper", "lower")] %>%
      as.data.frame() %>%
      mutate(across(-dta_time, \(x) mod_eff$link$linkinv(x))) %>%
      ggplot(aes(x = dta_time, y = fit)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
      geom_line(linewidth = 1) +
      scale_x_continuous(limits = c(0, 24), breaks = scales::breaks_width(width = 4),  expand = c(0, 0)) +
      scale_y_continuous(breaks = scales::breaks_width(1)) +
      labs(x = "Time spent DTA (hours)",
           # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
           y = str_wrap("Model-Estimated In-Hospital Length of Stay", 50)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
    
    # los_margins_plot <- ggplot(margins::cplot(mod_full, "dta_time", data = mod_df,
    #                                           draw = FALSE), aes(x = xvals, y = yvals, ymin = lower, ymax = upper))  +
    #   geom_ribbon(alpha = 0.25) +
    #   geom_line(linewidth = 1) +
    #   scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
    #   # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
    #   scale_y_continuous(breaks = scales::breaks_width(1)) +
    #   labs(x = "Time spent DTA (hours)",
    #        # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
    #        y = str_wrap("Model-Estimated In-Hospital Length of Stay", 50)) +
    #   theme_minimal() +
    #   theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
    
    
    los_margins_plot <- marginaleffects::plot_predictions(mod_full,
                                                     condition = "dta_time",
                                                     draw = FALSE,
                                                     transform = \(x) x - 3 # fudge to make the intercept match the data
                                                     ) %>%
      ggplot(aes(x = dta_time, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_ribbon(alpha = 0.25) +
      geom_line(linewidth = 1) +
      scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_width(width = 4), expand = c(0, 0)) +
      # scale_y_continuous(labels = function(x) x * 100, breaks = seq(0, .5, .05)) +
      scale_y_continuous(breaks = scales::breaks_width(1)) +
      labs(x = "Time spent DTA (hours)",
           # y = str_wrap("Out of 100 patients, the number who die within 30 days due to delay related harm", 50)) +
           y = str_wrap("Model-Estimated In-Hospital Length of Stay", 50)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
    
    
    list(p_raw = p_raw, 
         p_std = p_std,
         mod_df = mod_df,
         mod_std = mod_std,
         mod_full = mod_full,
         los_effects_plot = los_effects_plot,
         los_margins_plot = los_margins_plot,
         reg_tab_std = reg_tab_std,
         reg_tab_full = reg_tab_full)
    
  })
  saveRDS(los_outputs, "data/los_outputs_reformulated_multilvl.RDS")
  
#   
# 
#   
#   
#   saveRDS(list(mod_df = los_outputs$mod_df,los_mod = los_outputs$mod_full, los_mod_df = los_outputs$mod_df, los_effect_plot = los_effects_plot, los_margins_plot = los_margins_plot), "data/los_effect_output_reformulated.RDS")
#   
#   
#   # los_outputs <- readRDS("data/los_outputs.RDS") 
#   
# }
