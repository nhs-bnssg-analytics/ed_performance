library(tidyverse)

mod_df <- readRDS("data/mort_effects_output.RDS") %>%
  `[[`("mod_df")



p_demog_downstr_1 <- mod_df %>%
  # mutate(imd = fct_recode(imd,
  #                         "1\n(least deprived)" = "1 (least deprived)",
  #                         "10\n(least deprived)" = "10 (most deprived)"
  #                         )) %>%
  select(Sex = sex,
         Age = age,
         "Index of multiple deprivation decile" = imd,
         `Arrival time` = arrival_time,
         `Arrival month` = arrival_month#, 
         # `Arrival Year` = arrival_year
  ) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~ ggplot(.x, aes(y = value, x = n)) +
                       geom_col(fill = "lightslategrey")  +
                       geom_text(aes(label=number(n, big.mark = ",")), position=position_dodge(width=0.9), hjust=-0.1) +
                       theme_minimal() +
                       scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
                       guides(alpha = "none") +
                       theme(legend.position = "bottom",
                             panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank(),
                             axis.text.y = element_text(size = 11),
                             axis.text.x = element_blank(),
                             legend.justification = "center",
                             legend.title = element_text()) +
                       labs(title = str_wrap(.y, 60),
                            x = "",
                            y = "",
                            fill = "Patient cohort:") +
                       expand_limits(x = 1.25*max(.x$n))))



p_demog_downstr_2 <- mod_df %>%
  select(#Age = age,
         `4-Hour ED performance` =perf,
         `Elixhauser comorbidity index (vWs)` = vw_score,
         `Previous ED attendances` = n_ed) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~
                       ggplot(.x, aes(x = value, y = "")) +
                       geom_boxplot(fill = "lightslategrey") +
                       theme_minimal() +
                       guides(alpha = "none") +
                       theme(legend.position = "off",
                             legend.justification = "center",
                             axis.text = element_text(size = 11),
                             plot.margin = margin(0, 0, 0, 0, "pt"),
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.line.y = element_blank(),
                             axis.title.y = element_blank(),
                             axis.ticks.length.y = unit(0, "pt"),
                             legend.title = element_text()) +
                       labs(title = str_wrap(.y, 60),
                            x = "",
                            y = "")))


library(patchwork)

# percent scale for ED perf

p_demog_downstr_2$plot[[1]] <- p_demog_downstr_2$plot[[1]] + scale_x_continuous(labels = percent)

ptc_1 <- wrap_plots(p_demog_downstr_1$plot[c(1, 2, 4)], nrow = 1) 
ptc_2 <- wrap_plots(p_demog_downstr_1$plot[c(3, 5)], ncol = 1) 
ptc_3 <- wrap_plots(p_demog_downstr_2$plot, ncol = 1)


wrap_plots(ptc_1, free(ptc_2) | ptc_3, ncol = 1, guides = "collect") +
  plot_layout(heights = c(2/4, 2/4)) &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11)) 

ggsave("materials/demog_downstream_smr.png",
       last_plot(),
       bg = "white",
       height = 15,
       width = 17.5, 
       scale = 0.8)


# get bnssg nhs nums for filtering
# string_attr <- glue::glue("select distinct 
#  nhs_number,
#  attribute_period,
#  age,
#  sex,
#  lsoa,
#  lsoa_2011
# from MODELLING_SQL_AREA.dbo.primary_care_attributes
# where attribute_period between '{report_start}' and '{report_end}'")
# 
# 
# dat_attr <- sqlQuery(con,string_attr) %>%
#   filter(!is.na(nhs_number)) %>%
#   mutate(nhs_number = as.character(nhs_number)) %>%
#   mutate(attribute_period = ymd(attribute_period))
# 
# saveRDS(dat_attr, "data/dat_attr_demog.RDS")
dat_attr <- readRDS("data/dat_attr_demog.RDS")

string_imd <- "select distinct * from Analyst_SQL_Area.dbo.IMD2019"

dat_imd <- sqlQuery(con,string_imd) 


# handover patient attr:
handover_attr <- readRDS("data/upstream_handover_df.RDS")  %>% 
  ungroup() %>%
  filter(!between(dt, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
  mutate(nhs_number = as.character(nhs_number)) %>%
  mutate(dt = as_date(floor_date(dt, "month"))) %>%
  select(-age) %>%
  left_join(dat_attr, by = join_by(nhs_number == nhs_number, dt == attribute_period)) %>%
  distinct() %>%
  na.omit() %>%
  left_join(dat_imd, by = join_by(lsoa_2011 == "LSOA code (2011)")) %>%
  select(nhs_number, Age = age, Sex = sex, "Index of Multiple Deprivation\n(IMD) Decile" = `Index of Multiple Deprivation (IMD) Decile`) %>%
  distinct() %>%
  na.omit() 

response_attr <- readRDS("data/upstream_resp_time_df.RDS")  %>% 
  mutate(nhs_number = as.character(nhs_number)) %>%
  mutate(dt = as_date(floor_date(dt, "month"))) %>%
  select(-age) %>%
  left_join(dat_attr, by = join_by(nhs_number == nhs_number, dt == attribute_period)) %>%
  distinct() %>%
  na.omit() %>%
  left_join(dat_imd, by = join_by(lsoa_2011 == "LSOA code (2011)")) %>%
  select(nhs_number, Age = age, Sex = sex, "Index of Multiple Deprivation\n(IMD) Decile" = `Index of Multiple Deprivation (IMD) Decile`) %>%
  distinct() %>%
  na.omit()


p_demog_upstr_resp_1 <- response_attr %>%
  filter(Sex %in% c("Male", "Female")) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = factor(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      levels = 1:10,
      labels = 1:10
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_recode(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "1 (least deprived)" = "1",
      "10 (least deprived)" = "10"
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_relevel(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "10 (least deprived)",
      after = Inf
    )
  ) %>%
  select(Sex, `Index of Multiple Deprivation\n(IMD) Decile`) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      name,
      ~ ggplot(.x, aes(y = value, x = n)) +
        geom_col() +
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          size = 3,
          col = "white"
        ) +
        theme_minimal() +
        scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
        guides(alpha = "none") +
        theme(
          legend.position = "bottom",
          axis.text.x = element_blank(),
          legend.justification = "center",
          legend.title = element_text()
        ) +
        labs(
          y = str_wrap(.y, 25),
          x = ""#,
          #y = ""
        )
    )
  )

p_demog_upstr_resp_2 <- response_attr %>%
  select(Age) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(
    data,
    name,
    ~
      ggplot(.x, aes(x = value, y = "")) +
      geom_boxplot() +
      theme_minimal() +
      guides(alpha = "none") +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        legend.title = element_text()
      ) +
      labs(
        y = str_wrap(.y, 20),
        x = ""#,
        #y = ""
      )
  ))


p_resp_ptc <- bind_rows(p_demog_upstr_resp_1, p_demog_upstr_resp_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
    )
  )) %>% 
  arrange(name) %>%
  pull(plot) %>%
  patchwork::wrap_plots(ncol = 1)  



# ggsave("materials/demog_upstream_resp.png",
#        last_plot(),
#        bg = "white",
#        height = 10,
#        width = 10, 
#        scale = 0.6)


p_demog_upstr_handover_1 <- handover_attr %>%
  filter(Sex %in% c("Male", "Female")) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = factor(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      levels = 1:10,
      labels = 1:10
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_recode(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "1 (least deprived)" = "1",
      "10 (least deprived)" = "10"
    )
  ) %>%
  mutate(
    `Index of Multiple Deprivation\n(IMD) Decile` = fct_relevel(
      `Index of Multiple Deprivation\n(IMD) Decile`,
      "10 (least deprived)",
      after = Inf
    )
  ) %>%
  select(Sex, `Index of Multiple Deprivation\n(IMD) Decile`) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      name,
      ~ ggplot(.x, aes(y = value, x = n)) +
        geom_col() +
        stat_summary(
          fun.data = n_fun,
          geom = "text",
          size = 3,
          col = "white"
        ) +
        theme_minimal() +
        scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
        guides(alpha = "none") +
        theme(
          legend.position = "bottom",
          axis.text.x = element_blank(),
          legend.justification = "center",
          legend.title = element_text()
        ) +
        labs(
          y = str_wrap(.y, 25),
          x = ""#,
          #y = ""
        )
    )
  )

p_demog_upstr_handover_2 <- handover_attr %>%
  select(Age) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(
    data,
    name,
    ~
      ggplot(.x, aes(x = value, y = "")) +
      geom_boxplot() +
      theme_minimal() +
      guides(alpha = "none") +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        #axis.title.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        legend.title = element_text()
      ) +
      labs(
        y = str_wrap(.y, 20),
        x = ""#,
        # y = ""
      )
  ))


p_handover_plots <- bind_rows(p_demog_upstr_handover_1, p_demog_upstr_handover_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
    )
  )) %>% 
  arrange(name) %>%
  pull(plot) 

p_resp_plots <- bind_rows(p_demog_upstr_resp_1, p_demog_upstr_resp_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Sex",
      "Age",
      "Index of Multiple Deprivation\n(IMD) Decile"
    )
  )) %>% 
  arrange(name) %>%
  pull(plot) 


p_upstream_ptch <-  c(p_handover_plots, p_resp_plots) %>%
  patchwork::wrap_plots(ncol = 2, byrow = FALSE) &
  patchwork::plot_annotation(title = "Demographics for Upstream Patients",
                             subtitle = "Patients included in ambulance handover time (left) & call response time (right) analysis") 

p_upstream_ptch[[4]] <- p_upstream_ptch[[4]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 

p_upstream_ptch[[5]] <- p_upstream_ptch[[5]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 
p_upstream_ptch[[6]] <- p_upstream_ptch[[6]] +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    legend.title = element_text()
  ) 

p_upstream_ptch

ggsave("materials/demog_upstream.png",
       last_plot(),
       bg = "white",
       height = 15,
       width = 20, 
       scale = 0.35)  
