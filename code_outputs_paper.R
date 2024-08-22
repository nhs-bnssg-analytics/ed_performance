library(tidyverse)


# UPSTREAM OUTPUTS

handover_outputs <- readRDS("data/handover_outputs.RDS")
resp_outputs <- readRDS("data/resp_outputs.RDS")

patchwork::wrap_plots(
  handover_outputs$p_raw + labs(title = "Observed outcome, normalised dependent"),
  handover_outputs$p_std + labs(title = "Standadised outcome, normalised dependent") + guides(shape = FALSE),
  axes = "collect",
  nrow = 1,
  guides = "collect"
) & theme(legend.position = "bottom",
          legend.margin = margin(l = 5, unit = "pt"),
          legend.spacing.x = unit(0, "cm"))

ggplot2::ggsave("materials/handover_raw_standard.png",
                ggplot2::last_plot(),
                height = 15,
                width = 20, 
                scale = 0.5)

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





