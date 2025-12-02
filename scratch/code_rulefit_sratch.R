library(rules)
library(xrf)

tune_spec <- rule_fit(
  mode = "unknown",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  penalty = tune(),
) %>%
set_mode("regression") %>%
set_engine("xrf", counts = FALSE)

rec <- recipe(perf_all ~ ., data = training(split))  %>%
  update_role(dt, new_role = "id")

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tune_spec)

folds <- vfold_cv(training(split), v = 10)


fit_grid <-  tune_wf %>% 
  extract_parameter_set_dials() %>% 
  grid_latin_hypercube(size = 200)

cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)

reg_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = fit_grid,
  control = control_grid(parallel_over = "everything")
)


best_rmse <- select_best(reg_res, "rmse")

final_spec <- finalize_model(tune_spec, best_rmse) %>%
  set_engine("xrf", importance = "permutation", counts = FALSE)

# final_spec <- rand_forest(
#   mtry = 1,
#   trees = 1000,
#   min_n = 250) %>%
#   set_mode("regression") %>%
#   set_engine("ranger", importance = "permutation")

final_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_spec)

fit <- last_fit(final_wf, split)

fit$.metrics

fit$.predictions[[1]] %>% ggplot(aes(x = .pred, y = perf_all)) + geom_point()

fit %>% extract_fit_engine() %>% vip::vip(geom = "point")


