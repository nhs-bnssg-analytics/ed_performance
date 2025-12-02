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


mod_df %>% 
  select(-nhs_number) %>%
select(mort_30, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-mort_30) %>%
  nest(.by = name) %>%
  mutate(name = recode(name, 
  "age" = "Age",
  "sex" = "Sex",
  "arrival_time" = "Time of event",
  "arrival_month" = "Month of event",
  "wkend" = "Event on weekend",
  "imd" = "Deprivation decile"
  )) %>%
  mutate(plot = map2(data, name, \(x, y) {
    x %>% summarise(mort = mean(mort_30 == "yes", na.rm = TRUE), .by = value) %>%
  ggplot(aes(y = value, x = mort)) +
  geom_col() +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  labs(y = y, x = "30-day mortality proportion")
  })) %>%
  pull(plot) %>%
  patchwork::wrap_plots(ncol = 1, axes = "collect")


p1 <- mod_df %>% 
  select(-nhs_number) %>%
select(mort_30, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-mort_30) %>%
  mutate(name = recode(name, 
  "age" = "Age",
  "sex" = "Sex",
  "arrival_time" = "Time of event",
  "arrival_month" = "Month of event",
  "wkend" = "Event on weekend",
  "imd" = "Deprivation decile"
  ))  %>% summarise(mort = mean(mort_30 == "yes", na.rm = TRUE), .by = c(name, value)) %>%
  ggplot(aes(y = value, x = mort)) +
  geom_col() +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "30-day mortality proportion") +
  facet_wrap(vars(name), ncol = 1, scales = "free_y", space = "free_y")
  
  
p2 <- mod_df %>% 
  select(-nhs_number) %>%
select(age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = recode(name, 
  "age" = "Age",
  "sex" = "Sex",
  "arrival_time" = "Time of event",
  "arrival_month" = "Month of event",
  "wkend" = "Event on weekend",
  "imd" = "Deprivation decile"
  )) %>%
  ggplot(aes(y = value)) +
  geom_bar() +
  theme_minimal() +
  # scale_x_continuous(labels = scales::percent) +
  labs(x = "Number of participants") +
  facet_wrap(vars(name), ncol = 1, scales = "free_y", space = "free_y")


patchwork::wrap_plots(list(p1, p2))

mod_df %>%
  select(-nhs_number) %>%
  select(mort_30, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-mort_30) %>%
  mutate(
    name = recode(
      name,
      "age" = "Age",
      "sex" = "Sex",
      "arrival_time" = "Time of event",
      "arrival_month" = "Month of event",
      "wkend" = "Event on weekend",
      "imd" = "Deprivation decile"
    )
  ) %>%
  summarise(
    mort = mean(mort_30 == "yes", na.rm = TRUE),
    n = n(),
    .by = c(name, value)
  ) %>%
  pivot_longer(cols = -c(name, value), names_to = "metric", values_to = "val") %>%
  ggplot(aes(y = value, x = val)) +
  geom_col() +
  theme_minimal() +
  # scale_x_continuous(labels = scales::percent) +
  # labs(x = "Number of participants") +
  facet_grid(name~metric, scales = "free", space = "free_y")
