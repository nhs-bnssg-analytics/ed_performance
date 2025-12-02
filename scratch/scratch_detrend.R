library(tidyverse)
library(feasts)

foo <- tibble(index = 1:100,
       var = runif(100)) %>%
mutate(var2 = lag(var),
       var3 = lag(var, 4)) %>%
na.omit() %>%
as_tsibble(index = index) 


foo %>%
CCF(var2, var) %>%
autoplot()

foo %>%
CCF(var, var3)
autoplot()


dat_perf %>%
  left_join(amb_conv) %>%
  filter(ed_category == "all",
         # filter strange DQ issues
         date != lubridate::ymd("2024-04-21"),
         date != lubridate::ymd("2024-04-19"),
         date != lubridate::ymd("2024-02-01")
  ) %>%
  filter(site == "BRISTOL ROYAL INFIRMARY") %>%
  ggplot(aes(x = lag(n_conv, 7), y = perf_all)) +
  geom_point() +
  geom_smooth(method = lm) 

library(feasts)

foo_3 <- dat_perf %>%
  filter(ed_category == "all",
         # filter strange DQ issues
         date != lubridate::ymd("2024-04-21"),
         date != lubridate::ymd("2024-04-19"),
         date != lubridate::ymd("2024-02-01")
  ) %>%
  select(site, date, perf_all) %>%
  full_join(bed_occ) %>%
  full_join(walk_ins %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(tot_arr) %>%
  full_join(n_major) %>%
  full_join(amb_conv %>% filter(ed_category == "all") %>% select(-ed_category)) %>%
  full_join(n_nctr) %>%
  full_join(calls_aband) %>%
  na.omit() %>% 
  pivot_longer(cols = -c(site, date), names_to = "metric", values_to = "value") %>%
  as_tsibble(index = date, key = c(site, metric)) %>%
  tsibble::fill_gaps() %>%
  tidyr::fill(value, .direction = "down") %>%
  model(STL(value)) %>%
  components() %>% 
  select(date, site, metric, remainder) %>%
  as_tibble() %>%
  pivot_wider(id_cols = c(date, site), names_from = "metric", values_from = "remainder") %>%
  relocate(perf_all, .after = site) %>%
  group_by(site) %>%
  nest() %>%
  mutate(data = map(data, ~rename(.x, !!!var_rn_2))) %>%
  mutate(pairs = map(data, ~.x %>% select(where(is.numeric)) %>% GGally::ggpairs())) %>%
  mutate(pairs = map(pairs, ~.x +
                       theme_bw() +
                       labs(title = glue::glue("{str_to_title(site)}")) +
                       scale_x_continuous(guide = guide_axis(check.overlap = TRUE)))) %>%
  # mutate(cor = map(data, ~ select(.x, where(is.numeric)) %>% rstatix::cor_mat())) %>%
  # mutate(cor = map(cor, rstatix::cor_gather)) %>%
  mutate(lags = map(data, function(dat){
    map(setdiff(names(dat), c("date", "4hr\nperformance")),
        function(col_name) {
          ccf_util_2(dat, col_name)
        }
    )
  })) %>%
  mutate(lags = map(lags, ~patchwork::wrap_plots(.x, ncol = 2) + patchwork::plot_annotation(title = str_to_title(site))))


foo_3$lags[[3]]
