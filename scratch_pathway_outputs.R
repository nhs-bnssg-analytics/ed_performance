downstream_outputs <- function(dat, workflow, outcome_var) {
  
  
  outcome_raw <- dat %>% pull({{outcome_var}})
  
  rec <- extract_recipe(fit(workflow, dat))
  
  full_fit <- fit(workflow, dat)
  full_pred <- predict(extract_fit_engine(full_fit), bake(rec, new_data = dat), type = "response")
  auc <- yardstick::roc_auc_vec(outcome_raw, estimate = full_pred, event_level = "second")
  
  reg_tab <- full_fit %>%
    extract_fit_engine() %>%
    gtsummary::tbl_regression(label = list(
      arrival_time = "Time of arrival",
      arrival_month = "Month of arrival",
      arrival_year = "Year of arrival",
      arrival_cat = "Arrival category",
      arrival_mode = "Arrival mode",
      perf = "4-hour ED performance",
      perf_sq = "4-hour ED performance (squared)",
      age = "Age",
      age_sq = "Age (squared)",
      n_ed = "Number in ED",
      n_ed_sq = "Number in ED (squared)",
      sex = "Sex",
      imd = "IMD",
      vw_score = "Van Walraven score",
      vw_score_sq = "Van Walraven score (squared)",
      Acuity_Desc = "Acuity description"#,
      # investigation = "Investigation description (encoded)",
      # diagnoses = "Diagnoses description (encoded)"
    )) %>%
    gtsummary::add_glance_table(include = c(nobs)) %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
      title = gt::md("**30-day Mortality**"),
      subtitle = glue::glue("ROC curve statistic = {round(auc, 4)}")
    )
  
  
  # predictions 
  
  t_var_raw <- dat %>%
    filter(dta_time <= t_max) %>%
    pull(dta_time)
  
  full_pred <- dat %>%
    filter(dta_time <= t_max) %>%
    bake(rec, new_data = .) %>%
    predict(extract_fit_engine(full_fit),
            newdata = .,
            type = "response")
  
  plot_df <- 
    dat %>%
    filter(dta_time <= t_max) %>%
    mutate(dta_time := cut(dta_time, breaks = unique(quantile(dta_time, p)), include.lowest = TRUE)) %>%
    mutate({{outcome_var}} := outcome_raw == "yes") %>%
    select(dta_time, {{outcome_var}}) %>%
    mutate(pred = full_pred) %>%
    group_by(dta_time) %>%
    summarise(
      n = n(),
      expected = sum(pred),
      actual = sum({{outcome_var}}),
      proportion = mean({{outcome_var}}),
      smr = actual / expected
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
    c(0, t_max) %>%
    sort()
  
  lab_fn <- function(x) {
    
    x <- round(x)
    glue::glue("{x%/%60}h {x - (60*x%/%60)}m") |>
      str_replace_all(" 0m", "") |>
      str_replace_all("^0h ", "")
  }
  
  
  
  (p_standard <- ggplot(plot_df, aes(x = mid/60, y = smr)) +
      geom_rect(shade_df, mapping = aes(x = NULL,
                                        y = NULL,
                                        xmin = V1/60,
                                        xmax = V2/60,
                                        ymin = -Inf,
                                        ymax = Inf),
                alpha = 0.15) + 
      geom_errorbar(aes(ymin = l_ci, ymax = u_ci, width = width/(2*60))) +
      
      geom_point() +
      
      # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
      scale_x_continuous(#limits = c(NA, (t_max/60)-1),
        breaks = x_breaks/60,
        labels = lab_fn(x_breaks),
        guide = guide_axis(n.dodge = 2)
      ) +
      # bnssgtheme() +
      theme_minimal() +
      # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, t_max/60)) +
      labs(y = "Standardised output",
           x = "Time Waiting for Admission"))
  
  
  
  (p_raw <- ggplot(plot_df, aes(x = mid/60, y = proportion)) +
      geom_rect(shade_df, mapping = aes(x = NULL,
                                        y = NULL,
                                        xmin = V1/60,
                                        xmax = V2/60,
                                        ymin = -Inf,
                                        ymax = Inf),
                alpha = 0.15) + 
      
      geom_point() +
      
      # scale_x_continuous(breaks = x_breaks/60, labels = scales::number_format(accuracy = 0.1)) +
      scale_x_continuous(#limits = c(NA, (t_max/60)-1),
        breaks = x_breaks/60,
        labels = lab_fn(x_breaks),
        guide = guide_axis(n.dodge = 2)
      ) +
      # bnssgtheme() +
      theme_minimal() +
      # coord_cartesian(ylim = c(0.75, 1.2), xlim = c(0, t_max/60)) +
      labs(y = "Output proportion",
           x = "Time Waiting for Admission"))
  
  
  list(auc = auc,
       reg_tab = reg_tab,
       p_standard = p_standard,
       p_raw = p_raw)
  
}


# pathway outcome

mod_df_pathway_1 <- mod_df %>%
  filter(pathway != "missing") %>%
  mutate(pathway = factor(ifelse(pathway == "P1", "yes", "no")))

saveRDS(mod_df_pathway_1, "data/mod_df_pathway_1.RDS")
mod_df_pathway_1 <- readRDS("data/mod_df_pathway_1.RDS")

split_pathway <- initial_split(mod_df_pathway_1, strata = pathway) 
glm_spec <- logistic_reg()

rec_pathway_1 <- recipe(pathway ~ ., data = training(split_pathway)) %>%
  # update_role(mort_30, sc, new_role = "outcomes") %>%
  # update_role(ed_time, new_role = "not for prediction") %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_pathway_1 <- workflow() %>%
  add_recipe(rec_pathway_1) %>%
  add_model(glm_spec)

p1_outputs <- downstream_outputs(mod_df_pathway_1, wf_pathway_1, pathway)

mod_df_pathway_2 <- mod_df %>%
  filter(pathway != "missing") %>%
  mutate(pathway = factor(ifelse(pathway %in% c("P2", "P3"), "yes", "no")))

saveRDS(mod_df_pathway_2, "data/mod_df_pathway_2.RDS")

mod_df_pathway_2 <- readRDS("data/mod_df_pathway_2.RDS")

split_pathway <- initial_split(mod_df_pathway_2, strata = pathway) 
glm_spec <- logistic_reg()

rec_pathway_2 <- recipe(pathway ~ ., data = training(split_pathway)) %>%
  # update_role(mort_30, sc, new_role = "outcomes") %>%
  # update_role(ed_time, new_role = "not for prediction") %>%
  update_role(dta_time, ed_time, new_role = "not for prediction") %>%
  # step_other(all_nominal_predictors(), -dta_time, other = "other") %>%
  # step_novel(all_nominal_predictors(), new_level = "novel") %>%
  # step_dummy(all_nominal_predictors()) %>%
  step_nzv()

wf_pathway_2 <- workflow() %>%
  add_recipe(rec_pathway_2) %>%
  add_model(glm_spec)

p2_outputs <- downstream_outputs(mod_df_pathway_2, wf_pathway_2, pathway)

saveRDS(p1_outputs, "data/downstream_pathway_1_outputs.RDS")
saveRDS(p2_outputs, "data/downstream_pathway_2_outputs.RDS")
