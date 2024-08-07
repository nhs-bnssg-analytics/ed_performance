n_fun <- function(x){
  return(data.frame(y = 0, label = paste0("n = ",scales::number(x, big.mark = ","))))
}



p_demog_downstr_1 <- mod_df %>%
# mutate(imd = fct_recode(imd,
#                         "1\n(least deprived)" = "1 (least deprived)",
#                         "10\n(least deprived)" = "10 (most deprived)"
#                         )) %>%
select(Sex = sex,
       IMD = imd,
       `Arrival Time` = arrival_time,
       `Arrival Month` = arrival_month, 
       `Arrival Year` = arrival_year) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~ ggplot(.x, aes(y = value, x = n)) +
                      geom_col() +
                       stat_summary(fun.data = n_fun,
                                    geom = "text",
                                    col = "white"
                       ) +
                      theme_minimal() +
                      scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
                      guides(alpha = "none") +
                      theme(legend.position = "bottom",
                            axis.text.x = element_blank(),
                            legend.justification = "center",
                            legend.title = element_text()) +
                      labs(title = str_wrap(.y, 25),
                           x = "",
                           y = "")))



p_demog_downstr_2 <- mod_df %>%
  select(Age = age,
         `4-Hour ED Performance` =perf,
         `Elixhauser Comorbidity Index (vWs)` = vw_score,
         `Previous ED attendances` = n_ed) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map2(data, name, ~
  ggplot(.x, aes(x = value, y = "")) +
  geom_boxplot() +
  theme_minimal() +
  guides(alpha = "none") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text()) +
  labs(title = str_wrap(.y, 20),
       x = "",
       y = "")))


bind_rows(p_demog_downstr_1, p_demog_downstr_2) %>%
  mutate(name = factor(
    name,
    levels = c(
      "Age",
      "Sex",
      "IMD",
      "Elixhauser Comorbidity Index (vWs)",
      "Previous ED attendances",
      "4-Hour ED Performance",
      "Arrival Time",
      "Arrival Month",
      "Arrival Year"
    )
  )) %>% 
arrange(name) %>%
pull(plot) %>%
patchwork::wrap_plots()  

ggsave("materials/demog_downstream.png",
       last_plot(),
       bg = "white",
       height = 12.5,
       width = 12.5, 
       scale = 0.6)

