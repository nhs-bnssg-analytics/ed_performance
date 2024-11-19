library(tidyverse)
library(tidymodels)
library(patchwork)

# UPSTREAM OUTPUTS

handover_outputs <- readRDS("data/handover_outputs.RDS")
resp_outputs <- readRDS("data/resp_outputs.RDS")
ed_time_outputs <- readRDS("data/time_ed_outputs.RDS")

bin_acc <- 0.1

library(patchwork)

bin_acc <- 0.1
x_lab <- "DTA queue as a percentage of cubicle capacity"

targets <- tribble(~outcome, ~target, ~quantile,
                   "Time in ED (min), standardised", 240, .95,
                   "Ambulance handover time (min), standardised", 30, .95,
                   "Ambulance response time (min) [999 Category 1 call], standardised", 15, .9,
                   "Ambulance response time (min) [999 Category 2 call], standardised", 40, .9,
                   "Ambulance response time (min) [999 Category 3 call], standardised", 120, .9
)

upstream_plots <- resp_outputs$plot_dat %>%
  group_by(call_category) %>%
  nest() %>%
  mutate(data = map2(data, call_category, \(x, y) {
    x %>%
      select(n_dta_norm, response = response_adj_inv) %>%
      mutate(
        outcome = as.character(glue::glue(
          "Ambulance response time (min) [999 Category {str_remove_all(y, 'cat')} call], standardised"
        )
        ))
  })) %>%
  pull(data) %>%
  bind_rows() %>%
  bind_rows(
    handover_outputs$plot_df %>%
      select(n_dta_norm, response = handover_adj_inv) %>%
      mutate(response = response/60, outcome = "Ambulance handover time (min), standardised")
  ) %>%
  bind_rows(
    ed_time_outputs$plot_df %>%
      select(n_dta_norm, response = trt_adj_inv) %>%
      mutate(outcome = "Time in ED (min), standardised")
  )  %>% 
  mutate(n_dta_norm = plyr::round_any(n_dta_norm, accuracy = bin_acc, f = \(x) floor(0.5 + x))) %>%
  filter(n_dta_norm >= 0.1) %>%
  group_by(outcome) %>%
  nest() %>%
  left_join(targets) %>%
  mutate(data = map2(data, quantile, \(x, y) mutate(x, response_q = quantile(response, y), .by = n_dta_norm))) %>%
  mutate(plot = map2(
    data,
    target,
    \(data, target)
    ggplot(data = data, ,mapping = aes(x = n_dta_norm, y = response)) +
      geom_boxplot(mapping = aes(group = cut_width(n_dta_norm, width = bin_acc)), outliers = FALSE, fatten = NULL) +
      stat_summary(
        fun = ~ mean(.x),
        geom = "point",
        col = "black",
        aes(col = "Mean", shape = "Mean")
      ) +
      geom_hline(aes(yintercept = target), col = "red", linetype = 2) +
      geom_point(aes(y = target, col = "National target")) +
      geom_point(aes(y = response_q, shape = "Comparator quantile"), col = "black") +
      theme_minimal() +
      scale_colour_manual(
        values = c("Mean" = "black", "Comparator quantile"= "black", "National target" = "red"),
      ) +
      scale_x_continuous(labels = scales::percent,
                         breaks = scales::breaks_width(0.5)) +
      coord_cartesian(xlim = c(0.05, 1.55)
                      # , ylim = c(0, NA), expand = FALSE
      ) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_shape_manual(values = c(17, 19),
                         breaks = c("Mean", "Comparator quantile"),
                         # guide = guide_legend(override.aes = list(size = 3, alpha = 1))
      ) +
      theme(panel.grid.major.x = element_line(color = "grey65", size = 0.3))
  )) %>%
  mutate(plot = map(plot, \(x) x + labs(x = x_lab, colour = "", shape = "", y = str_remove_all(outcome, " \\[.*\\]"))))



upstream_plots$plot <- modify_at(upstream_plots$plot, .at = c(2), .f = \(x) x +  scale_y_continuous(limits = c(0, NA),
                                                                 expand = expansion(mult = c(0, 0.05)),
                                                                 breaks = breaks_width(width = 240)))

upstream_plots$plot <- modify_at(upstream_plots$plot, .at = c(5), .f = \(x) x +  scale_y_continuous(limits = c(0, NA),
                                                                 expand = expansion(mult = c(0, 0.05)),
                                                                 breaks = breaks_width(width = 120)))

upstream_plots$plot <- modify_at(upstream_plots$plot, .at = c(1, 4), .f = \(x) x +  scale_y_continuous(limits = c(0, NA),
                                                                 expand = expansion(mult = c(0, 0.05)),
                                                                 breaks = breaks_width(width = 60)))

ptc_1 <- upstream_plots %>%
  filter(!str_detect(outcome, "999")) %>%
  pull(plot) %>%
  wrap_plots(axes = "collect", ncol = 1)

ptc_2 <- upstream_plots %>%
  filter(str_detect(outcome, "999")) %>%
  mutate(plot = map2(plot, outcome, \(x, y) x + facet_grid(.~str_remove_all(str_extract(y, "\\[.*\\]"), "\\[|\\]")))) %>%
  arrange(outcome) %>%
  pull(plot) %>%
  wrap_plots(axes = "collect", ncol = 1)

upstream_out <- wrap_plots(ptc_2 | ptc_1,
           axes = "collect",
           guides = "collect") &
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.margin = margin(l = 5, unit = "pt"),
        legend.spacing.x = unit(0, "cm")) 


ggplot2::ggsave("materials/upstream_standard.png",
                upstream_out,
                bg = "white",
                height = 12.5,
                width = 15.0, 
                scale = 0.65)



# ptc_1 <- patchwork::wrap_plots(ed_time_outputs$p_std + guides(colour = "none", shape = "none"),
#                       handover_outputs$p_std + guides(colour = "none", shape = "none"),
#                       ncol = 1,
#                       axes = "collect",
#                       guides = "collect"
#                       ) 
# 
# ptc_2 <- patchwork::wrap_plots(resp_outputs$p_norm_std[[3]] + guides(colour = "none", shape = "none"),
#                       resp_outputs$p_norm_std[[1]] + guides(colour = "none", shape = "none"),
#                       resp_outputs$p_norm_std[[2]],
#                       ncol = 1,
#                       axes = "collect",
#                       guides = "collect"
#                       ) 
# 
# patchwork::wrap_plots(ptc_1 | ptc_2, guides = "collect") &
#   theme(legend.position = "bottom", legend.justification = "center")
# 
# ggplot2::ggsave("materials/upstream_standard.png",
#                 ggplot2::last_plot(),
#                 bg = "white",
#                 height = 12.5,
#                 width = 15.0, 
#                 scale = 0.65)



# x_lab <- "DTA Queue as a Percentage of Cubicle Capacity (Majors + Resus)."
# y_lab <- "Model-Standardised Ambulance Handover Time\n(min)."
# 
# handover_outputs$p_std +
#   labs(x = x_lab,
#        y = y_lab,
#        title = "",
#        col = "") +
#   scale_y_continuous(breaks = breaks_width(60)) +
#   theme(legend.title = element_blank(),
#         legend.spacing.x = unit(0, "cm"))
# 
# 
# ggplot2::ggsave("materials/handover_standard.png",
#                 ggplot2::last_plot(),
#                 bg = "white",
#                 height = 7.5,
#                 width = 10, 
#                 scale = 0.6)
# 
# x_lab <- "DTA Queue as a Percentage of Cubicle Capacity (Majors + Resus)."
# y_lab <- "Model-Standardised Ambulance Response Time\n(min)."
# 
# resp_outputs$p_norm_std +
#   labs(x = x_lab,
#        y = y_lab,
#        title = "",
#        col = "") +
#   # scale_y_continuous(breaks = breaks_width(60)) +
#   theme(legend.title = element_blank(),
#         legend.spacing.x = unit(0, "cm"))
# 
# 
# ggplot2::ggsave("materials/response_standard.png",
#                 ggplot2::last_plot(),
#                 bg = "white",
#                 height = 12.5,
#                 width = 10, 
#                 scale = 0.6)

ed_time_outputs$full_mod %>%
  tidy(conf.int = TRUE) %>%
  filter(term == "n_dta_norm") %>%
  dplyr::select(estimate, conf.low, conf.high, p.value) %>%
  mutate(across(-p.value, \(x) x/10))

handover_outputs$full_mod %>%
  tidy(conf.int = TRUE) %>%
  filter(term == "n_dta_norm") %>%
  dplyr::select(estimate, conf.low, conf.high, p.value) %>%
  mutate(across(-p.value, \(x) x/10))

resp_outputs$full_mod %>% map(\(x) {
  tidy(x, conf.int = TRUE) %>%
  filter(term == "n_dta_norm") %>%
  dplyr::select(estimate, conf.low, conf.high, p.value) %>%
  mutate(across(-p.value, \(x) x/10))
  }
)

handover_tab <- handover_outputs$full_mod_tab 
ed_time_tab <- ed_time_outputs$full_mod_tab 
resp_tab_1 <- resp_outputs$full_mod_tabs$tab[[1]] 
resp_tab_2 <- resp_outputs$full_mod_tabs$tab[[2]] 
resp_tab_3 <- resp_outputs$full_mod_tabs$tab[[3]]

gtsummary::tbl_merge(list(
  ed_time_tab,
  handover_tab,
  resp_tab_3,
  resp_tab_2,
  resp_tab_1
  ), tab_spanner = c("Time spent in ED (min)",
                     "Ambulance handover time (min)",
                     "999 call Category 1 response time (min)",
                     "999 call Category 2 response time (min)",
                     "999 call Category 3 response time (min)"
                     )) %>%
  gtsummary::as_gt() %>%
  gt::gtsave("materials/upstream_reg_table.html")


# Downstream outputs

los_outputs <- readRDS("data/los_outputs.RDS")

x_lab <- "Time Spent DTA (hour)"
y_lab <- "Model-Standardised In-Hospital Length of Stay (day)"

los_outputs$p_std +
  labs(x = x_lab,
       y = y_lab,
       title = "",
       col = "") +
  # scale_y_continuous(breaks = breaks_width(60)) +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0, "cm"))


ggplot2::ggsave("materials/los_standard.png",
                ggplot2::last_plot(),
                bg = "white",
                height = 7.5,
                width = 10, 
                scale = 0.6)


# patchwork::wrap_plots(
#   handover_outputs$p_raw + labs(title = "Observed outcome, normalised dependent"),
#   handover_outputs$p_std + labs(title = "Standadised outcome, normalised dependent") + guides(shape = FALSE),
#   axes = "collect",
#   nrow = 1,
#   guides = "collect"
# ) & theme(legend.position = "bottom",
#           legend.margin = margin(l = 5, unit = "pt"),
#           legend.spacing.x = unit(0, "cm"))

# ggplot2::ggsave("materials/handover_raw_standard.png",
#                 ggplot2::last_plot(),
#                 height = 15,
#                 width = 20, 
#                 scale = 0.5)

ggplot2::ggsave("materials/handover_raw_transformed.png",
                handover_outputs$p_std_trans,
                bg = "white",
                height = 7.5,
                width = 10, 
                scale = 0.7)




patchwork::wrap_plots(resp_outputs$p_norm_raw + labs(title = "Observed outcome, normalised dependent"),
                      resp_outputs$p_norm_std + labs(title = "Standadised outcome, normalised dependent") + guides(shape = FALSE),
                      axes = "collect",
                      guides = "collect",
                      nrow = 1) & theme(legend.position = "bottom",
                                        legend.margin = margin(l = 5, unit = "pt"),
                                        legend.spacing.x = unit(0, "cm"))

ggplot2::ggsave("materials/response_time_raw_standard.png",
                ggplot2::last_plot(),
                height = 15,
                width = 20,
                scale = 0.5)


handover_model_plot_dat <- handover_outputs$mod_dat_row %>%
  select(-n_dta) %>%
  expand_grid(
              n_dta = seq(0, 35, 10),
              # n_dta = c(seq(0, 35, 5), seq(1, 36, 5)),
              ed_size = seq(10, 35, 1))  %>%
  ungroup() %>%
  mutate(row_id = 1:n(),
         n_dta_i = n_dta) %>%
  bind_rows(., mutate(., n_dta = n_dta + 1), .id = "id") %>%
  mutate(id = str_c("row_", id), n_dta_norm_trans = (n_dta / ed_size) ^ handover_outputs$trans_pow) %>%
  bind_cols(., pred = predict(handover_outputs$full_mod_trans, newdata = .)) %>% 
  select(id, row_id, n_dta_i, ed_size, pred) %>%
  pivot_wider(id_cols = c(ed_size, n_dta_i, row_id), names_from = id, values_from = pred) %>%
  mutate(diff = row_2 - row_1) 
  
  
  
  ggplot(handover_model_plot_dat, 
         aes(x = ed_size, y = diff, group = n_dta_i)) +
  geom_line() +
  scale_x_continuous(limits = c(2, NA)) +
  scale_y_continuous(position = "right") +
  directlabels::geom_dl(aes(label = str_c("DTA Queue: ", n_dta_i)),
                        method = list(directlabels::dl.combine("first.points"),
                                      directlabels::dl.trans(x=x-0.5)), cex = 1) +
  theme_minimal() +
  labs(x = "ED capacity (majors + resus)",
       y = str_wrap("Increase in handover time associated with 1 extra person on DTA Queue (min)", 50))
  
  ggplot2::ggsave("materials/handover_model_plot.png",
                  ggplot2::last_plot(),
                  bg = "white",
                  height = 7.5,
                  width = 10, 
                  scale = 0.6)
  
  
 

# +
#   facet_wrap(vars(n_dta_i))
  




map(resp_outputs$full_mod, broom::tidy) %>%
  bind_rows(.id = "call_category") %>%
  filter(term == "n_dta_norm") %>%
  select(call_category, estimate, std.error) %>%
  mutate(hi = estimate+std.error*qnorm(0.975), lo = estimate-std.error*qnorm(0.975)) %>%
  expand_grid(ed_size = seq(5, 30, 1), n = 1) %>%
  mutate(diff = (n/ed_size)*estimate,
         diff_lo = (n/ed_size)*lo,
         diff_hi = (n/ed_size)*hi,
         ) %>%
  ggplot(aes(x = ed_size, y = diff, col = factor(call_category))) +
  geom_ribbon(aes(ymin = diff_lo, ymax = diff_hi), alpha = 0.1) +
  geom_line() +
  theme_minimal()



resp_outputs$mod_dat_row %>%
  expand_grid(n_dta = 10:11, ed_size = seq(5, 35, 1)) %>%
  mutate(n_dta_norm = n_dta/ed_size) %>%
  group_by(ed_size) %>%
  nest() %>%
  mutate(pred = map(data, function(data) 
    map(resp_outputs$full_mod,
        ~predict(.x, data)) %>%
        # ~predict(.x, data, interval = "prediction", level = 0.5)) %>%
      as.list() %>% as.data.frame()
     )
    ) %>%
  unnest(pred) %>%
  select(-data) %>%
  pivot_longer(-ed_size, names_to = c("call_cat")) %>%
  group_by(ed_size, call_cat) %>%
  mutate(id = str_c("row_", 1:n())) %>%
  pivot_wider(id_cols = c(ed_size, call_cat), names_from = id, values_from = value) %>%
  mutate(value = row_2 - row_1) %>%
  ggplot(aes(x = ed_size, y = value, group = call_cat)) +
  geom_line() +
  scale_y_continuous(position = "right") +
  scale_x_continuous(limits = c(-5, NA), breaks = seq(0, 50, 10)) +
  directlabels::geom_dl(aes(label = str_c("Call category ", str_remove(call_cat, "cat"))),
                        method = list(directlabels::dl.combine("first.points"),
                                      directlabels::dl.trans(x=x-0.5)), cex = 1) +
  theme_minimal() +
  labs(x = "ED capacity (majors + resus)",
       y = str_wrap("Increase in call response time associated with 1 extra person on DTA Queue (min)", 50))
  
ggplot2::ggsave("materials/response_model_plot.png",
                ggplot2::last_plot(),
                bg = "white",
                height = 7.5,
                width = 10, 
                scale = 0.6)




ggplot2::ggsave("materials/response_model_plot_log_y.png",
                ggplot2::last_plot() +
                  scale_y_log10(position = "right") +
                  guides(y =  guide_axis_logticks(position = "right")),
                bg = "white",
                height = 7.5,
                width = 10, 
                scale = 0.6)
# DOWNSTREAM OUTPUTS

los_outputs <- readRDS("data/los_outputs.RDS")

patchwork::wrap_plots(los_outputs$p_raw + labs(title = "Observed outcome, observed dependent"),
                      los_outputs$p_std + labs(title = "Standardised outcome, observed dependent"),
                      axes = "collect",
                      guides = "collect",
                      nrow = 1) & theme(legend.position = "bottom",
                                        legend.margin = margin(l = 5, unit = "pt"),
                                        legend.spacing.x = unit(0, "cm"))

ggplot2::ggsave("materials/los_raw_standard.png",
                ggplot2::last_plot(),
                height = 15,
                width = 20, 
                scale = 0.5)


# smr_mort <- readRDS("data/smr_mort.RDS")
# smr_readm <- readRDS("data/smr_readm.RDS")
# smr_reatt <- readRDS("data/smr_reatt.RDS")
# smr_pathway <- readRDS("data/smr_pathway.RDS")
# smr_sc <- readRDS("data/smr_sc.RDS")
# 
# patchwork::wrap_plots(smr_mort,
#                       smr_reatt,
#                       smr_readm,
#                       smr_pathway,
#                       smr_sc,
#                       byrow = FALSE,
#                       ncol = 2, axes = "collect")
# 
# ggplot2::ggsave("materials/scratch_smr_multiplot.png",
#        ggplot2::last_plot(),
#        height = 15,
#        width = 20, 
#        scale = 0.5)


smr_mort <- readRDS("data/downstream_mort_outputs.RDS")
smr_readm <- readRDS("data/downstream_readm_outputs.RDS")
smr_reatt <- readRDS("data/downstream_reatt_outputs.RDS")
smr_pathway_1 <- readRDS("data/downstream_pathway_1_outputs.RDS")
smr_pathway_2 <- readRDS("data/downstream_pathway_2_outputs.RDS")

std_plots <- map(list(smr_mort,
         smr_readm,
         smr_reatt,
         smr_pathway_1,
         smr_pathway_2), ~pluck(.x, "p_standard"))

map2(std_plots, c("30-day mortality",
                  "30-day re-admission",
                  "30-day re-attenance",
                  "D2A P1 only",
                  "D2A P2 & P3 only"
                  ),
     ~.x + labs(title = .y)) %>%
patchwork::wrap_plots(byrow = FALSE,
                      ncol = 2, axes = "collect")

ggplot2::ggsave("materials/scratch_smr_multiplot_new.png",
                ggplot2::last_plot(),
                height = 15,
                width = 20, 
                scale = 0.5)

raw_plots <- map(list(smr_mort,
         smr_readm,
         smr_reatt,
         smr_pathway_1,
         smr_pathway_2), ~pluck(.x, "p_raw"))

map2(raw_plots, c("30-day mortality",
                  "30-day re-admission",
                  "30-day re-attenance",
                  "D2A P1 only",
                  "D2A P2 & P3 only"
                  ),
     ~.x + labs(title = .y, y = "Outcome percentage") + scale_y_continuous(labels = scales::percent)) %>%
patchwork::wrap_plots(byrow = FALSE,
                      ncol = 2, axes = "collect")

ggplot2::ggsave("materials/scratch_smr_multiplot_new_nonstandardised.png",
                ggplot2::last_plot(),
                height = 15,
                width = 20, 
                scale = 0.5)





