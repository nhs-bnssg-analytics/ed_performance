mort_effect_output <- readRDS("data/mort_effects_output.RDS")
readm_effect_output <- readRDS("data/readm_effects_output.RDS")
mort_effect_plot <- mort_effect_output$mort_effects_plot
readm_effect_plot <- readm_effect_output$readm_effects_plot
mort_roc_lab <- mort_effect_output$mort_roc_lab
readm_roc_lab <- readm_effect_output$readm_roc_lab

los_effect_plot <- readRDS("data/los_effects_plot.RDS")

wrap <- 30

ylab_mort <- str_wrap("Model-Estimated 30-day Mortality Rate.", wrap)
ylab_readm <- str_wrap("Model-Estimated 30-day Readmission Rate.", wrap)
ylab_los <- str_wrap("Model-Estimated In-Hospital Length of Stay (day).", wrap)



patchwork::wrap_plots(mort_effect_plot + labs(y = ylab_mort),
                      readm_effect_plot + labs(y = ylab_readm),
                      los_effect_plot + labs(y = ylab_los),
                      ncol = 1, axes = "collect")

ggsave("materials/effects_plot_patch.png",
       last_plot(),
       bg = "white",
       height = 12.5,
       width = 10, 
       scale = 0.5)
mort_effect_output$mort_mod_raw %>% tidy() %>% View()
