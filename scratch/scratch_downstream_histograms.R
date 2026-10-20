library(tidyverse)
library(tidymodels)
library(lme4)
library(patchwork)

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
  "arrival_time" = str_wrap("Time of event", 10),
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
  "arrival_time" = str_wrap("Time of event", 10),
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
  "arrival_time" = str_wrap("Time of event", 10),
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


custom_formatter <- function(x) {
  # Check if the maximum value is 1 and the minimum value is non-negative,
  # which strongly suggests a 0-1 percentage/rate scale.
  if (max(x, na.rm = TRUE) <= 1 && min(x, na.rm = TRUE) >= 0) {
    # Use percent formatting (multiplies by 100 and adds %)
    return(label_percent()(x))
  } else {
    # Otherwise, assume it's a large number and use comma formatting
    # The 'big.mark' argument sets the thousands separator
    return(label_comma(big.mark = ",")(x))
  }
}

p1 <- mod_df %>%
  select(-nhs_number) %>%
  select(readm, mort_30, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(readm, mort_30)) %>%
  mutate(
    name = recode(
      name,
      "age" = "Age",
      "sex" = "Sex",
      "arrival_time" = str_wrap("Time of event", 10),
      "arrival_month" = "Month of event",
      "wkend" = str_wrap("Event on weekend", 10),
      "imd" = "Deprivation decile"
    )
  ) %>%
  summarise(
    mort = mean(mort_30 == "yes", na.rm = TRUE),
    readm = mean(readm, na.rm = TRUE),
    n = n(),
    .by = c(name, value)
  ) %>%
  pivot_longer(cols = -c(name, value), names_to = "metric", values_to = "val") %>%
    mutate(
    metric = recode(
      metric,
      "mort" = "30-day mortality rate",
      "readm" = "30-day readmission rate",
      n = "N"
    )
  ) %>%
  mutate(metric = fct_rev(factor(metric))) %>%
  ggplot(aes(y = value, x = val)) +
  geom_col() +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(strip.placement = "outside", plot.margin = margin(0))

p2 <- mod_df %>%
  select(-nhs_number) %>%
  select(los, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(los)) %>%
  mutate(
    name = recode(
      name,
      "age" = "Age",
      "sex" = "Sex",
      "arrival_time" = str_wrap("Time of event", 10),
      "arrival_month" = "Month of event",
      "wkend" = str_wrap("Event on weekend", 10),
      "imd" = "Deprivation decile"
    )
  ) %>%
  mutate(metric = "Inpatient length of stay (days)") %>%
  ggplot(aes(y = value, x = los)) +
  geom_boxplot(outliers = FALSE) +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") #+
  # theme(strip.text.y = element_blank(), axis.text.y = element_blank(), plot.margin = margin(0))



wrap_plots(list(p1, p2), nrow = 1, widths = c(3, 1))



p_n <- mod_df %>%
  select(-nhs_number) %>%
  select(age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(everything()) %>%
  mutate(
    name = factor(name,
       levels = c("age", "sex", "imd", "arrival_time", "arrival_month", "wkend"),
      labels = c("Age", "Sex", "Deprivation decile", str_wrap("Time of event", 10), "Month of event", str_wrap("Event on weekend", 10)))
  ) %>%
  summarise(
    n = n(),
    .by = c(name, value)
  ) %>%
  pivot_longer(cols = -c(name, value), names_to = "metric", values_to = "val") %>%
    mutate(
    metric = recode(
      metric,
      n = "N"
    )
  ) %>%
  mutate(metric = fct_rev(factor(metric))) %>%
  ggplot(aes(y = value, x = val)) +
  geom_col() +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(strip.placement = "outside"#,
#  plot.margin = margin(0)
)

p_dta_time <- mod_df %>%
  select(-nhs_number) %>%
  select(dta_time, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(dta_time)) %>%
  mutate(
    name = factor(name,
       levels = c("age", "sex", "imd", "arrival_time", "arrival_month", "wkend"),
      labels = c("Age", "Sex", "Deprivation decile", str_wrap("Time of event", 10), "Month of event", str_wrap("Event on weekend", 10)))
  ) %>%
  mutate(metric = "Medical boarding time (hours)") %>%
  ggplot(aes(y = value, x = dta_time)) +
  geom_boxplot(outliers = FALSE) +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(strip.placement = "outside"#,
#  plot.margin = margin(0)
)

p_los <- mod_df %>%
  select(-nhs_number) %>%
  select(los, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(los)) %>%
  mutate(
    name = factor(name,
       levels = c("age", "sex", "imd", "arrival_time", "arrival_month", "wkend"),
      labels = c("Age", "Sex", "Deprivation decile", str_wrap("Time of event", 10), "Month of event", str_wrap("Event on weekend", 10)))
  ) %>%
  mutate(metric = "Inpatient length of stay (days)") %>%
  ggplot(aes(y = value, x = los)) +
  geom_boxplot(outliers = FALSE) +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(strip.placement = "outside"#,
#  plot.margin = margin(0)
)


p_readm <- mod_df %>%
  select(-nhs_number) %>%
  select(readm, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(readm)) %>%
  mutate(
    name = factor(name,
       levels = c("age", "sex", "imd", "arrival_time", "arrival_month", "wkend"),
      labels = c("Age", "Sex", "Deprivation decile", str_wrap("Time of event", 10), "Month of event", str_wrap("Event on weekend", 10)))
  ) %>%
  summarise(
    readm = mean(readm, na.rm = TRUE),
    .by = c(name, value)
  ) %>%
  pivot_longer(cols = -c(name, value), names_to = "metric", values_to = "val") %>%
    mutate(
    metric = recode(
      metric,
      "readm" = "30-day readmission rate",
    )
  ) %>%
  mutate(metric = fct_rev(factor(metric))) %>%
  ggplot(aes(y = value, x = val)) +
  geom_col() +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(strip.placement = "outside"#,
#  plot.margin = margin(0)
)


p_mort <- mod_df %>%
  select(-nhs_number) %>%
  select(mort_30, age, sex, imd, arrival_time, arrival_month, wkend) %>%
  pivot_longer(-c(mort_30)) %>%
  mutate(
    name = factor(name,
       levels = c("age", "sex", "imd", "arrival_time", "arrival_month", "wkend"),
      labels = c("Age", "Sex", "Deprivation decile", str_wrap("Time of event", 10), "Month of event", str_wrap("Event on weekend", 10)))
  ) %>%
  summarise(
    mort = mean(mort_30 == "yes", na.rm = TRUE),
    .by = c(name, value)
  ) %>%
  pivot_longer(cols = -c(name, value), names_to = "metric", values_to = "val") %>%
    mutate(
    metric = recode(
      metric,
      "mort" = "30-day mortality rate",
      "readm" = "30-day readmission rate",
      n = "N"
    )
  ) %>%
  mutate(metric = fct_rev(factor(metric))) %>%
  ggplot(aes(y = value, x = val)) +
  geom_col() +
  scale_x_continuous(labels = custom_formatter) +
  theme_minimal() +
  labs(x = "", y = "") +
  facet_grid(name ~ metric, scales = "free", space = "free_y", switch = "y") +
  theme(
    strip.placement = "outside"#,
    #  plot.margin = margin(0)
    )

wrap_plots(list(p_n, p_dta_time + theme(strip.text.y = element_blank()), p_los + theme(strip.text.y = element_blank()), p_readm + theme(strip.text.y = element_blank()), p_mort + theme(strip.text.y = element_blank())), axes = "collect", nrow = 1) &
  # plot_annotation(title = "Cohort size and demography compared with delay-mediated outcomes") &
  theme(strip.placement = "outside",
  #  panel.spacing = unit(1, "lines"),
    strip.clip = "off") 


ggsave(last_plot(),
filename = "materials/downstream_histograms.png",
bg = "white",
width = 20,
height = 10,
scale = 0.55)
