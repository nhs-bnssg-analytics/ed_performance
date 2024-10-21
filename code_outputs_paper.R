library(tidyverse)


# UPSTREAM OUTPUTS

handover_outputs <- readRDS("data/handover_outputs.RDS")
resp_outputs <- readRDS("data/resp_outputs.RDS")

bin_acc <- 0.1

x_lab <- "DTA Queue as a Percentage of Cubicle Capacity (Majors + Resus)."
y_lab <- "Model-Standardised Ambulance Handover Time\n(min)."

handover_outputs$p_std +
  labs(x = x_lab,
       y = y_lab,
       title = "",
       col = "") +
  scale_y_continuous(breaks = breaks_width(60)) +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0, "cm"))


ggplot2::ggsave("materials/handover_standard.png",
                ggplot2::last_plot(),
                bg = "white",
                height = 7.5,
                width = 10, 
                scale = 0.6)

x_lab <- "DTA Queue as a Percentage of Cubicle Capacity (Majors + Resus)."
y_lab <- "Model-Standardised Ambulance Response Time\n(min)."

resp_outputs$p_norm_std +
  labs(x = x_lab,
       y = y_lab,
       title = "",
       col = "") +
  # scale_y_continuous(breaks = breaks_width(60)) +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0, "cm"))


ggplot2::ggsave("materials/response_standard.png",
                ggplot2::last_plot(),
                bg = "white",
                height = 12.5,
                width = 10, 
                scale = 0.6)


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





