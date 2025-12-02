smr_plots <- function(mort, bin, time_max){

map(mort, function(days_mort){
  map(bin, function(bins){

    mod_df <- dat_adm %>%
      # filter(site == "SOUTHMEAD HOSPITAL") %>%
      filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
      filter(dta_time <= t_max) %>%
      filter(dta_time > 0) %>%
      filter(sex %in% 1:2) %>%
      left_join(mortality_df) %>%
      # mutate(age = cut(age, breaks = seq(0, 120, 10), include.lowest = TRUE)) %>%
      # mutate(dta_time = dta_time > 4*60) %>%
      # mutate(dta_time = cut(dta_time, breaks = 60*seq(0, 24, 4), include.lowest = TRUE)) %>%
      mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
      # mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
      mutate(age_sq = age^2) %>%
      mutate(perf_sq = perf^2) %>%
      mutate(n_ed_sq = n_ed^2) %>%
      mutate(n_adm_sq = n_adm^2) %>%
      mutate(vw_score_sq = vw_score^2) %>%
      mutate(imd = factor(imd)) %>%
      mutate(imd = fct_recode(imd, "1 (least deprived)" = "1", "10 (most deprived)" = "10")) %>%
      mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
      mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
      mutate(mort_30 = factor(ifelse(mort_30, "yes", "no"), levels = c("no", "yes"))) %>%
      mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
      mutate(arrival_time = fct_recode(arrival_time,
                                       "00:00-06:00" = "00:00:00",
                                       "06:00-12:00" = "06:00:00",
                                       "12:00-18:00" = "12:00:00",
                                       "18:00-24:00" = "18:00:00")) %>%
      mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
      mutate(arrival_year = factor(year(arrival_dt))) %>%
      select(
        arrival_time,
        arrival_month,
        arrival_year,
        perf,
        perf_sq,
        age,
        age_sq,
        n_ed,
        n_ed_sq,
        # Acuity_Desc,
        # arrival_mode,
        # arrival_cat,
        # diagnoses,
        # investigation,
        # n_adm,    # bring these back in when they aren't all
        # n_adm_sq,
        sex,
        imd,
        vw_score,
        vw_score_sq,
        mort_30,
        dta_time,
        ed_time
      )

split <- initial_split(mod_df, strata = mort_30) 
glm_spec <- logistic_reg()

rec <- recipe(mort_30 ~ ., data = training(split)) %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)

# fit <- last_fit(wf, split)
# 
# augment(fit) %>%
#   roc_curve(mort_30,
#             .pred_yes,
#             event_level = "second") %>%
#   autoplot()

# fit on all data

full_fit <- fit(wf, mod_df)

# predictions 
full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")

# smr

smr_plot_quant <- function(t_var,
                           time_max = 30*60,
                           n_bin = 10,
                           x_lab = "Time waiting for admission"){ 
  
  p = seq(0, 1, length = n_bin + 1)
  
  t_var_raw <- mod_df %>%
    filter({{t_var}} <= time_max) %>%
    pull({{t_var}})
  
  full_pred <- mod_df %>%
    filter({{t_var}} <= time_max) %>%
    bake(prep(rec), new_data = .) %>%
    predict(extract_fit_engine(full_fit),
            newdata = .,
            type = "response")
  
  plot_df <- 
    mod_df %>%
    filter({{t_var}} <= time_max) %>%
    mutate({{t_var}} := cut({{t_var}}, breaks = unique(quantile({{t_var}}, p)), include.lowest = TRUE)) %>%
    mutate(mort_30 = mort_30 == "yes") %>%
    select({{t_var}}, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by({{t_var}}) %>%
    summarise(
      n = n(),
      expected_mort = sum(pred_mort),
      actual_mort = sum(mort_30),
      smr = actual_mort / expected_mort
    ) %>%
    mutate(
      left = head(unique(quantile(t_var_raw, p)), -1),
      right = tail(unique(quantile(t_var_raw, p)), -1),
      mid = (left + right) / 2,
      width = right - left,
      weight = 1/width
    )
  
  plot_df <- bind_rows(plot_df, 
                       {tail(plot_df, 1) %>% mutate(left = right)})
  
  

  pred_df <- lm(smr ~ mid, weights = weight, data = plot_df) %>%
    predict(newdata = plot_df, se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit,
              u_se = fit + se.fit,
              l_se = fit - se.fit) %>%
    bind_cols(select(plot_df, mid)) 
    
    
    pred_df <- bind_rows(pred_df, {
      #predict at limits
      lm(smr ~ mid, weights = weight, data = plot_df) %>%
        predict(newdata = data.frame(mid = range(plot_df$left, plot_df$right)), se.fit = TRUE) %>%
        as_tibble() %>%
        transmute(pred = fit,
                  u_se = fit + se.fit,
                  l_se = fit - se.fit) %>%
        mutate(mid = range(plot_df$left, plot_df$right))
    })
    
  
    shade_df <- unique(plot_df$left) %>%
      sort() %>%
      head(n = 2*length(.) %/% 2) %>% # slice the nearest multiple of 2 values 
      matrix(ncol = 2, byrow = TRUE) %>%
      as_tibble() %>%
      mutate(min = min(pred_df$l_se, plot_df$smr))
    
    x_breaks <- unique(plot_df$left, plot_df$right) %>%
    tail(-1) %>%
    head(-1) %>%
    c(0, time_max) %>%
    sort()
    
    lab_fn <- function(x) {

      x <- round(x)
      glue::glue("{x%/%60}h {x - (60*x%/%60)}m") |>
      str_replace_all(" 0m", "") |>
      str_replace_all("0h ", "")
    }
    
  x_lab = glue::glue("{x_lab}\n(hours, {n_bin} bins)")
  
  ggplot(plot_df, aes(x = mid/60, y = smr)) +
    # geom_step(aes(x = left/60)) +
    geom_rect(shade_df, mapping = aes(x = NULL, y = NULL, xmin = V1/60, xmax = V2/60, ymin = min, ymax = Inf), alpha = 0.15) + 
    geom_ribbon(data = pred_df, aes(x = mid/60, y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    geom_line(data = pred_df, aes(x = mid/60, y = pred), col = bnssgcols[4]) +
    geom_linerange(aes(xmin = left/60, xmax = right/60)) +
    # geom_vline(aes(xintercept = right/60), linetype = 2) +
    # geom_vline(aes(xintercept = left/60), linetype = 2) +
    # geom_col(aes(width = (right-left)/60),col = bnssgcols[4]) +
    geom_point(col = bnssgcols[4]) +
    # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = x_breaks/60, labels = lab_fn(x_breaks)) +
    bnssgtheme() +
    # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, time_max/60)) +
    labs(y = "Standardised Mortality Ratio",
         x = x_lab)
  
}

map(bins, ~smr_plot_quant(dta_time,
                                   time_max = time_max,
                                   n_bin = .x))})})
}


mort <- seq(10, 30, 5)
bin <- seq(5, 10, 1)
out <- smr_plots(mort, bin, time_max = 24*60)

out %>%
  flatten() %>%
  flatten() %>%
  patchwork::wrap_plots(ncol = length(bin), nrow = length(mort), axes = "collect")


smr_final <-  smr_plots(mort = 30, bin = 5:10, time_max = 24*60)


smr_final %>%
  flatten() %>%
  flatten() %>%
  patchwork::wrap_plots(axes = "collect")


out <- smr_plots(mort = 30, bin = seq(5, 10, 1), time_max = 24*60)


out %>%
  flatten() %>%
  flatten() %>%
  patchwork::wrap_plots()


ggsave("materials/smr_quant_bin_scratch.png",
       last_plot(),
       height = 5,
       width = 7.5, 
       scale = 1)


# making final plot

days_mort  <- 30
bin <- 7
time_max <- 24*60



mod_df <- dat_adm %>%
  # filter(site == "SOUTHMEAD HOSPITAL") %>%
  filter(!between(dta_dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>% # filter bad DQ period for NBT
  filter(dta_time <= t_max) %>%
  filter(dta_time > 0) %>%
  filter(sex %in% 1:2) %>%
  left_join(mortality_df) %>%
  # mutate(age = cut(age, breaks = seq(0, 120, 10), include.lowest = TRUE)) %>%
  # mutate(dta_time = dta_time > 4*60) %>%
  # mutate(dta_time = cut(dta_time, breaks = 60*seq(0, 24, 4), include.lowest = TRUE)) %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  # mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(age_sq = age^2) %>%
  mutate(perf_sq = perf^2) %>%
  mutate(n_ed_sq = n_ed^2) %>%
  mutate(n_adm_sq = n_adm^2) %>%
  mutate(vw_score_sq = vw_score^2) %>%
  mutate(imd = factor(imd)) %>%
  mutate(imd = fct_recode(imd, "1 (least deprived)" = "1", "10 (most deprived)" = "10")) %>%
  mutate(mort_30 = ifelse((interval(arrival_dt, date_death)/ddays(1)) < days_mort, TRUE, FALSE)) %>%
  mutate(mort_30 = coalesce(mort_30, FALSE)) %>%
  mutate(mort_30 = factor(ifelse(mort_30, "yes", "no"), levels = c("no", "yes"))) %>%
  mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
  mutate(arrival_time = fct_recode(arrival_time,
                                   "00:00-06:00" = "00:00:00",
                                   "06:00-12:00" = "06:00:00",
                                   "12:00-18:00" = "12:00:00",
                                   "18:00-24:00" = "18:00:00")) %>%
  mutate(arrival_month = factor(month(arrival_dt), labels = month.name)) %>%
  mutate(arrival_year = factor(year(arrival_dt))) %>%
  select(
    arrival_time,
    arrival_month,
    arrival_year,
    perf,
    perf_sq,
    age,
    age_sq,
    n_ed,
    n_ed_sq,
    # Acuity_Desc,
    # arrival_mode,
    # arrival_cat,
    # diagnoses,
    # investigation,
    # n_adm,    # bring these back in when they aren't all
    # n_adm_sq,
    sex,
    imd,
    vw_score,
    vw_score_sq,
    mort_30,
    dta_time,
    ed_time
  )


saveRDS(mod_df, "data/mod_df_smr_figure_final.RDS")
mod_df <- readRDS("data/mod_df_smr_figure_final.RDS")


split <- initial_split(mod_df, strata = mort_30)
glm_spec <- logistic_reg()
# 
rec <- recipe(mort_30 ~ ., data = training(split)) %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(glm_spec)
# 
# fit <- last_fit(wf, split)


# fit on all data

full_fit <- fit(wf, mod_df)

# predictions 
full_pred <- predict(extract_fit_engine(full_fit), bake(prep(rec), new_data = mod_df), type = "response")

# smr

  p = seq(0, 1, length = bin + 1)
  
  t_var_raw <- mod_df %>%
    filter(dta_time <= time_max) %>%
    pull(dta_time)
  
  full_pred <- mod_df %>%
    filter(dta_time <= time_max) %>%
    bake(prep(rec), new_data = .) %>%
    predict(extract_fit_engine(full_fit),
            newdata = .,
            type = "response")
  
  plot_df <- 
    mod_df %>%
    filter(dta_time <= time_max) %>%
    mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
    mutate(mort_30 = mort_30 == "yes") %>%
    select(dta_time, mort_30) %>%
    mutate(pred_mort = full_pred) %>%
    group_by(dta_time) %>%
    summarise(
      n = n(),
      expected_mort = sum(pred_mort),
      actual_mort = sum(mort_30),
      smr = actual_mort / expected_mort
    ) %>%
    mutate(
      left = head(unique(quantile(t_var_raw, p)), -1),
      right = tail(unique(quantile(t_var_raw, p)), -1),
      mid = (left + right) / 2,
      width = right - left,
      weight = 1/width
    ) %>%
    mutate(
      z = qnorm(0.95 + (1 - 0.95) / 2),
       u_ci = smr + z*sqrt(smr/n),
       l_ci = smr - z*sqrt(smr/n)
      )
  
  plot_df <- bind_rows(plot_df, 
                       {tail(plot_df, 1) %>% mutate(left = right)})
  
  
  
  pred_df <- lm(smr ~ mid, weights = weight, data = plot_df) %>%
    predict(newdata = plot_df, se.fit = TRUE) %>%
    as_tibble() %>%
    transmute(pred = fit,
              u_se = fit + se.fit,
              l_se = fit - se.fit) %>%
    bind_cols(select(plot_df, mid)) 
  
  
  pred_df <- bind_rows(pred_df, {
    #predict at limits
    lm(smr ~ mid, weights = weight, data = plot_df) %>%
      predict(newdata = data.frame(mid = range(plot_df$left, plot_df$right)), se.fit = TRUE) %>%
      as_tibble() %>%
      transmute(pred = fit,
                u_se = fit + se.fit,
                l_se = fit - se.fit) %>%
      mutate(mid = range(plot_df$left, plot_df$right))
  })
  
  
  shade_df <- unique(plot_df$left) %>%
    sort() %>%
    head(n = 2*length(.) %/% 2) %>% # slice the nearest multiple of 2 values 
    matrix(ncol = 2, byrow = TRUE) %>%
    as_tibble() %>%
    mutate(min = min(pred_df$l_se, plot_df$smr, plot_df$l_ci))
  
  x_breaks <- unique(plot_df$left, plot_df$right) %>%
    tail(-1) %>%
    head(-1) %>%
    c(0, time_max) %>%
    sort()
  
  lab_fn <- function(x) {
    
    x <- round(x)
    glue::glue("{x%/%60}h {x - (60*x%/%60)}m") |>
      str_replace_all(" 0m", "") |>
      str_replace_all("^0h ", "")
  }
  
  
  ggplot(plot_df, aes(x = mid/60, y = smr)) +
    # geom_step(aes(x = left/60)) +
    geom_rect(shade_df, mapping = aes(x = NULL, y = NULL, xmin = V1/60, xmax = V2/60, ymin = -Inf, ymax = Inf), alpha = 0.15) + 
    # geom_ribbon(data = pred_df, aes(x = mid/60, y = pred, ymin = l_se, ymax = u_se), alpha = 0.25) +
    # geom_line(data = pred_df, aes(x = mid/60, y = pred), col = bnssgcols[4]) +
    # geom_linerange(aes(xmin = left/60, xmax = right/60), linetype = 2) +
    geom_errorbar(aes(ymin = l_ci, ymax = u_ci, width = width/(2*60))) +
    # geom_vline(aes(xintercept = right/60), linetype = 2) +
    # geom_vline(aes(xintercept = left/60), linetype = 2) +
    # geom_col(aes(width = (right-left)/60),col = bnssgcols[4]) +
    # geom_line(linetype = 2) +
    geom_point() +
    
    # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(#limits = c(NA, (time_max/60)-1),
                       breaks = x_breaks/60,
                       labels = lab_fn(x_breaks),
                       guide = guide_axis(n.dodge = 2)
                       ) +
    # bnssgtheme() +
    theme_minimal() +
    # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, time_max/60)) +
     labs(y = "Standardised Mortality Ratio",
          x = "Time waiting for admission")
  
  ggsave("materials/smr_quant_7_bin_3yrs_white_bg.png",
         last_plot(),
         bg = "white",
         height = 5,
         width = 7.5, 
         scale = 0.7)
  
  mod_df_dta <- mod_df %>%
    mutate(dta_time = dta_time > 4*60)
  
  split_dta <- initial_split(mod_df_dta, strata = mort_30)
  
  rec_dta <- recipe(mort_30 ~ ., data = training(split_dta)) %>%
    update_role(ed_time, new_role = "not for prediction") %>%
    # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
    step_novel(all_nominal_predictors(), new_level = "novel") %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv()
  
  wf_dta <- workflow() %>%
    add_recipe(rec_dta) %>%
    add_model(glm_spec)
  
  full_fit_dta <- fit(wf_dta, mod_df_dta)

  
  full_fit_dta %>% broom::tidy()
  