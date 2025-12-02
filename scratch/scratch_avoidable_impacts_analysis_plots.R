require(tidyverse)
library(patchwork)
library(marquee)

impacts_output <- readRDS("data/avoidable_impacts_output.RDS")

# "#32A895"
# "#9532a8"
# "#A89532"



handover_plots <- 
  impacts_output %>%
  filter(impact == "om2") %>%
  mutate(value = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#9532a8 **-{round(value)}-**}')) %>%
  nest(.by = grp) %>%
  mutate(handover_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
  })) %>%
  select(-data) 


ed_plots <- impacts_output %>%
  filter(impact == "om3") %>%
  mutate(value = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#A89532 **-{round(value)}-**}')) %>%
  nest(.by = grp) %>%
  mutate(ed_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
  })) %>%
  select(-data)


om_margin_plot <-   impacts_output %>%
    filter(impact %in% c("om2", "om3")) %>%
    select(impact, label, colour) %>%
    mutate(label = str_replace(label,
                               pattern = "(ambulance handover|seen for treatment)",
                               replacement = glue::glue(.open = "-{",
                                                        .close = "}-",
                                                        "{.-{colour}- **\\1**}"))) %>%
    select(-colour) %>%
  nest(.by = impact) %>%
  mutate(om_margin_plot = map(data,
                              \(d) d %>%
                                mutate(x = 1, y = 1)  %>%
                                ggplot(mapping = aes(x, y)) +
                                # geom_text(aes(label = label))
                                geom_marquee(
                                  aes(label = label),
                                  size = 8,
                                  width = 0.85,
                                  style = classic_style(align = "center"),
                                  #family = 'Comic Sans MS'
                                ) +
                                theme_void())) %>%
  select(-data)


site_margin_plot <- tibble(
  grp = c(
    "SOUTHMEAD HOSPITAL",
    "WESTON GENERAL HOSPITAL",
    "BRISTOL ROYAL INFIRMARY"
  ),
  label = c(
    "at Southmead hospital",
    "at Weston general hospital",
    "at Bristol royal infirmary"
  )
) %>%
  nest(.by = grp) %>%
  mutate(site_margin_plot = map(data,
                                \(d) d %>%
                                  mutate(x = 1, y = 1)  %>%
                                  ggplot(mapping = aes(x, y)) +
                                  # geom_text(aes(label = label))
                                  geom_marquee(
                                    aes(label = label),
                                    size = 8,
                                    width = 0.85,
                                    style = classic_style(align = "center"),
                                    #family = 'Comic Sans MS'
                                  ) +
                                  theme_void())) %>%
  select(-data)


wi <- c(3/8, 3/8, 2/8)
hi <- c(4/5, 1/5)

p1 <- left_join(handover_plots, ed_plots) %>%
  left_join(site_margin_plot) %>%
  pivot_longer(-grp, names_to = "om") %>%
  pull(value) %>%
  wrap_plots(ncol = 3, byrow = TRUE, widths = wi)

p2 <- wrap_plots(list(om_margin_plot$om_margin_plot[[1]],
                      om_margin_plot$om_margin_plot[[2]],
                      plot_spacer()), widths = wi)

wrap_plots(list(p1, p2), ncol = 1, heights = hi) +
  plot_annotation("Patients arriving at the emergency department waited an additional..") &
  theme(plot.title = element_marquee(size = 34, width = 1))
