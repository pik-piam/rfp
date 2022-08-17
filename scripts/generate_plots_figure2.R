#-- Section for running the script independently -------------
# Libraries
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(svglite)
# Functions
dump <- lapply(list.files("functions", pattern = ".R", full.names = TRUE), source)
# Data
load("data/data_rfp_step2.RData")
# Assumptions
u_horizon  <- 2050
u_dr       <- 0.05
u_region   <- "World"
u_models   <- c("GCAM5.3_NGFS", "MESSAGEix-GLOBIOM 1.1", "REMIND-MAgPIE 2.1-4.2") 
u_scenario <- "Net Zero 2050"
#-------------------------------------------------------------

v_sector <- c("Energy Supply|Production|Coal", 
              "Energy Supply|Electric Utilities|Coal", 
              "Energy Supply|Electric Utilities|Non-Biomass Renewables|Solar PV")


# Pathways
plots <-  list()
for (ksec in v_sector) {
  plots[[paste0("pathway_", ksec)]] <- plot_rfp_pathways_wtotal(data_rfp_abs_diff %>% filter(grepl(u_model, model)), 
                    ksec, 
                    u_scenario, u_region, HORIZON=u_horizon, DISCOUNT=u_dr, PRINT=FALSE, TOTAL=TRUE) + 
    ggtitle(paste0(ksec, " - (Disc: 5%)")) +
    theme(axis.text = element_text(size=20), axis.title = element_text(size=20))
}

ggarrange(plotlist=plots, common.legend = TRUE, legend = "bottom", align = c("hv"), ncol = 3)
ggsave(paste0("output/figure2/rfp_pathways_3sectors.svg"), width = 20, height = 6)


# Cumulative bar plot
for (ksec in v_sector) {
  tmp <- data_rfp_abs_diff %>% 
    filter(grepl("REMIND", model), region == u_region, scenario == u_scenario, period >= 2020, period <= 2050, sector == ksec) %>% 
    group_by(model, region, rfp, sector, baseline) %>% 
    mutate(dt = lead(period, default=2050) - period) %>% 
    summarise(
      value = sum(value*dt),
      diff_abs = sum(diff_abs*dt)/1000
    ) %>% 
    ungroup()
  
  plots[[paste0("cumdisc_", ksec)]] <- ggplot(tmp %>% mutate(rfp = factor(gsub("Diagnostics\\|RFP\\|", "", rfp), 
                                                                      levels=c("Direct emissions cost", "Indirect cost", "Low-carbon capital expenditure", "Revenue", "Total"),
                                                                      ordered = TRUE))) + 
    geom_col(aes(x=rfp, y=diff_abs, fill=rfp)) +
    geom_segment(aes(x=0.5, xend=5.5, y=0, yend=0)) +
    scale_fill_manual(name = "RFP", 
                       values = c(
                         "Direct emissions cost" = RColorBrewer::brewer.pal(4, "Set1")[1],
                         "Indirect cost" = RColorBrewer::brewer.pal(4, "Set1")[2],
                         "Low-carbon capital expenditure" = RColorBrewer::brewer.pal(4, "Set1")[3],
                         "Revenue" = RColorBrewer::brewer.pal(4, "Set1")[4],
                         "Total" = "#000000")) +
    ylab("[trillion US$]") + xlab("") +
    scale_x_discrete(breaks = c("Direct emissions cost", "Indirect cost", "Low-carbon capital expenditure", "Revenue", "Total"), labels = c("DEC", "IC", "LCE", "Rev", "Total")) +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(axis.text = element_text(size=20), axis.title = element_text(size=20))
  
}
ggarrange(plotlist=plots[4:6], common.legend = TRUE, legend = "bottom", align = c("hv"), ncol = 3)
ggsave(paste0("output/figure2/rfp_cumulativeDiscSum_3sectors.svg"), width = 20, height = 6)

ggarrange(plotlist=plots, align = c("hv"))
ggsave(paste0("output/figure2/rfp_PathwaysAndCumDiscSum_3sectors.svg"), width = 20, height = 12)


# Heat maps
svglite::svglite(file=paste("output/figure2/fig2c_heatmap_coalsupply.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Production|Coal", u_scenario, SUM = "TOTAL")
dev.off()

svglite::svglite(file=paste("output/figure2/fig2c_heatmap_coalutilities.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Electric Utilities|Coal", u_scenario, SUM = "TOTAL")
dev.off()

svglite::svglite(file=paste("output/figure2/fig2c_heatmap_solarpv.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Electric Utilities|Non-Biomass Renewables|Solar PV", u_scenario, SUM = "TOTAL")
dev.off()















tmp <- data_rfp_abs_diff %>% 
  filter(grepl("REMIND", model), region == u_region, scenario == u_scenario, period >= 2020, period <= 2050, sector == "Energy Supply|Production|Coal") %>% 
  group_by(model, region, rfp, sector, baseline) %>% 
  mutate(dt = lead(period, default=2050) - period) %>% 
  summarise(
    value = sum(value*dt),
    diff_abs = sum(diff_abs*dt)
  ) %>% 
  ungroup()

ggplot(tmp %>% filter(!grepl("Total", rfp)) %>% mutate(rfp = factor(gsub("Diagnostics\\|RFP\\|", "", rfp), 
                                                                    levels=c("Direct emissions cost", "Indirect emissions cost", "Low-carbon capital expenditure", "Revenue", "Overall"),
                                                                    ordered = TRUE))) + 
  geom_bar(aes(x=rfp, y=diff_abs, fill=rfp), stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("output/fig2b_cumbar_coalsupply.svg")

tmp <- data_rfp_abs_diff %>% 
  filter(grepl("REMIND", model), region == u_region, scenario == u_scenario, period >= 2020, period <= 2050, sector == "Energy Supply|Electric Utilities|Coal") %>% 
  group_by(model, region, rfp, sector, baseline) %>% 
  mutate(dt = lead(period, default=2050) - period) %>% 
  summarise(
    value = sum(value*dt),
    diff_abs = sum(diff_abs*dt)
  ) %>% 
  ungroup()

ggplot(tmp %>% filter(!grepl("Total", rfp)) %>% mutate(rfp = factor(gsub("Diagnostics\\|RFP\\|", "", rfp), 
                                                                    levels=c("Direct emissions cost", "Indirect emissions cost", "Low-carbon capital expenditure", "Revenue", "Overall"),
                                                                    ordered = TRUE))) + 
  geom_bar(aes(x=rfp, y=diff_abs, fill=rfp), stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("output/fig2b_cumbar_coalutilities.svg")

tmp <- data_rfp_abs_diff %>% 
  filter(grepl("REMIND", model), region == u_region, scenario == u_scenario, period >= 2020, period <= 2050, sector == "Energy Supply|Electric Utilities|Non-Biomass Renewables|Solar PV") %>% 
  group_by(model, region, rfp, sector, baseline) %>% 
  mutate(dt = lead(period, default=2050) - period) %>% 
  summarise(
    value = sum(value*dt),
    diff_abs = sum(diff_abs*dt)
  ) %>% 
  ungroup()

ggplot(tmp %>% filter(!grepl("Total", rfp)) %>% mutate(rfp = factor(gsub("Diagnostics\\|RFP\\|", "", rfp), 
                                                                    levels=c("Direct emissions cost", "Indirect emissions cost", "Low-carbon capital expenditure", "Revenue", "Overall"),
                                                                    ordered = TRUE))) + 
  geom_bar(aes(x=rfp, y=diff_abs, fill=rfp), stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("output/fig2b_cumbar_solarpv.svg")

# Heat map
svglite::svglite(file=paste("output/fig2c_heatmap_coalsupply.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Production|Coal", u_scenario)
dev.off()

svglite::svglite(file=paste("output/fig2c_heatmap_coalutilities.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Electric Utilities|Coal", u_scenario, SUM = "OVERALL")
dev.off()

svglite::svglite(file=paste("output/fig2c_heatmap_solarpv.svg"))
draw_rfp_heatmap_world(data_plot_diff_rel %>% filter(grepl("REMIND", model), region == u_region, scenario == u_scenario), 
                       "Energy Supply|Electric Utilities|Non-Biomass Renewables|Solar PV", u_scenario)
dev.off()

