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
u_sum_type_supply <- "TOTAL"
u_sum_type_demand <- "OVERALL"
#-------------------------------------------------------------

u_sectors_energysupply  <- rev(c("Energy Supply|Production|Oil", "Energy Supply|Production|Gas", 
                                 "Energy Supply|Production|Coal",  
                                 "Energy Supply|Production|Biomass", "Energy Supply|Electric Utilities|Coal", 
                                 "Energy Supply|Electric Utilities|Gas", "Energy Supply|Electric Utilities|Nuclear", 
                                 "Energy Supply|Electric Utilities|Biomass", 
                                 "Energy Supply|Electric Utilities|Non-Biomass Renewables",
                                 "Energy Supply|Refineries|Oil", "Energy Supply|Fischer-Tropsch Process|Biomass",
                                 "Energy Supply|Hydrogen Plants|Coal", "Energy Supply|Hydrogen Plants|Gas", 
                                 "Energy Supply|Hydrogen Plants|Biomass", "Energy Supply|Hydrogen Plants|Electricity"))

u_sectors_energydemand  <- rev(c("End-Use|Residential and Commercial", "End-Use|Industry", 
                                 "End-Use|Industry|Cement", "End-Use|Industry|Chemicals", 
                                 "End-Use|Industry|Steel", "End-Use|Transportation"))


#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_world_byModel(data_plot_diff_rel %>% filter(region == u_region, scenario == u_scenario), u_sectors_energysupply, u_models, SUM=u_sum_type_supply)

svglite(file=paste0("output/figure3/rfp_heatmap_3models_EnergySupply_",u_scenario,"_", u_region, "_dr", paste(u_dr*100), "pct.svg"), width = 20, height = 16)
draw_rfp_heatmap_world_byModel(data_plot_diff_rel %>% filter(region == u_region, scenario == u_scenario), u_sectors_energysupply, u_models, SUM=u_sum_type_supply)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
draw_rfp_heatmap_world_diff_abs_byModel(data_plot_diff_abs %>% filter(region == u_region, scenario == u_scenario), u_sectors_energydemand, u_models, SUM=u_sum_type_demand)

svglite(file=paste0("output/figure4/rfp_heatmap_3models_EnergyDemand_",u_scenario,"_", u_region, "_dr", paste(u_dr*100), "pct.svg"))
draw_rfp_heatmap_world_diff_abs_byModel(data_plot_diff_abs %>% filter(region == u_region, scenario == u_scenario), u_sectors_energydemand, u_models, SUM=u_sum_type_demand)
dev.off()