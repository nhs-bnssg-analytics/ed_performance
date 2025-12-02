library(tidyverse)
library(lubridate)


maj_resus_staff_df <- read_csv("data/nbt_maj_resus_staff.csv") %>%
  pivot_longer(cols = -c(`Majors & Resus`), 
               names_to = "date",
               values_to = "count") %>%
  mutate(dt = parse_date_time(glue::glue("{date} {`Majors & Resus`}"), orders = "dmy HMS"),
         dt = `second<-`(dt, 0),
         category = "Majors & Resus") %>%
  select(dt, category, count) %>%
  replace_na(list(count = 0)) 

edou_staff_df <- read_csv("data/nbt_edou_staff.csv") %>%
  pivot_longer(cols = -c(`EDOU`), 
               names_to = "date",
               values_to = "count") %>%
  mutate(dt = parse_date_time(glue::glue("{date} {`EDOU`}"), orders = "dmy HMS"),
         dt = `second<-`(dt, 0),
         category = "EDOU") %>%
  select(dt, category, count) %>%
  replace_na(list(count = 0)) 

min_staff_df <- read_csv("data/nbt_min_staff.csv") %>%
  pivot_longer(cols = -c(`Minors`), 
               names_to = "date",
               values_to = "count") %>%
  mutate(dt = parse_date_time(glue::glue("{date} {`Minors`}"), orders = "dmy HMS"),
         dt = `second<-`(dt, 0),
         category = "Minors") %>%
  select(dt, category, count)%>%
  replace_na(list(count = 0))  


nbt_staffing_df <- bind_rows(maj_resus_staff_df, edou_staff_df, min_staff_df)

nbt_staffing_df %>%
  filter(dt %within% interval(ymd("2024-01-01"), ymd("2024-01-14"))) %>%
  ggplot(aes(x = dt, y = count, col = category)) +
  geom_step() 
