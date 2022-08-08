v_models   <- c("GCAM5.3_NGFS", "MESSAGEix-GLOBIOM 1.1", "REMIND-MAgPIE 2.1-4.2") 
v_scenario <- "Net Zero 2050"

v_sectors_energysupply  <- rev(c("Energy Supply|Production|Oil", "Energy Supply|Production|Gas", 
                                 "Energy Supply|Production|Coal",  
                                 "Energy Supply|Production|Biomass", "Energy Supply|Electric Utilities|Coal", 
                                 "Energy Supply|Electric Utilities|Gas", "Energy Supply|Electric Utilities|Nuclear", 
                                 "Energy Supply|Electric Utilities|Biomass", 
                                 "Energy Supply|Electric Utilities|Non-Biomass Renewables",
                                 "Energy Supply|Refineries|Oil", "Energy Supply|Fischer-Tropsch Process|Biomass",
                                 "Energy Supply|Hydrogen Plants|Coal", "Energy Supply|Hydrogen Plants|Gas", 
                                 "Energy Supply|Hydrogen Plants|Biomass", "Energy Supply|Hydrogen Plants|Electricity"))


v_sectors_energydemand  <- rev(c("End-Use|Residential and Commercial", "End-Use|Industry", 
                                 "End-Use|Industry|Cement", "End-Use|Industry|Chemicals", 
                                 "End-Use|Industry|Steel", "End-Use|Industry|Other", "End-Use|Transportation"))

#== World ======================================================================
v_region   <- "World"

#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_world_byModel(data_plot_diff_rel %>% filter(region == v_region, scenario == v_scenario), v_sectors_energysupply, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergySupply_",v_scenario,"_", v_region, "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_world_byModel(data_plot_diff_rel %>% filter(region == v_region, scenario == v_scenario), v_sectors_energysupply, v_models, SUM=sum_type)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
v_sectors  <- rev(c("End-Use|Residential and Commercial", "End-Use|Industry", "End-Use|Industry|Cement", "End-Use|Industry|Chemicals", "End-Use|Industry|Steel", "End-Use|Industry|other", "End-Use|Transportation"))

draw_rfp_heatmap_world_diff_abs_byModel(data_plot_diff_abs %>% filter(region == v_region, scenario == v_scenario), v_sectors_energydemand, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", v_region, "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_world_diff_abs_byModel(data_plot_diff_abs %>% filter(region == v_region, scenario == v_scenario), v_sectors_energydemand, v_models, SUM=sum_type)
dev.off()

svglite(file=paste0("output/legend_rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", v_region, "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_legend(c(-1000000, -20, -10, -5, -2.5, -1, 0, 1, 2.5, 5, 10, 20, 1000000), 
                        c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))
dev.off()

#== China ======================================================================
v_region <- c("GCAM5.3_NGFS|China", 
              "MESSAGEix-GLOBIOM 1.1|R11_CPA", 
              "REMIND-MAgPIE 2.1-4.2|CHA")
v_newregionname <- "China"

#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergySupply_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)
dev.off()



#== EU ======================================================================
v_region <- c("GCAM5.3_NGFS|EU-12", 
              "MESSAGEix-GLOBIOM 1.1|R11_WEU", 
              "REMIND-MAgPIE 2.1-4.2|EUR")
v_newregionname <- "EU"

#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergySupply_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)
dev.off()


#== India ======================================================================
v_region <- c("GCAM5.3_NGFS|India", 
              "MESSAGEix-GLOBIOM 1.1|R11_SAS", 
              "REMIND-MAgPIE 2.1-4.2|IND")
v_newregionname <- "India"

#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergySupply_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)
dev.off()


#== USA ========================================================================
v_region <- c("GCAM5.3_NGFS|USA", 
              "MESSAGEix-GLOBIOM 1.1|R11_NAM", 
              "REMIND-MAgPIE 2.1-4.2|USA")
v_newregionname <- "USA"

#-- Energy Supply sectors ------------------------------------------------------
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergySupply_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_byModel(data_plot_diff_rel %>% 
                                  filter(region %in% v_region, scenario == v_scenario) %>% 
                                  mutate(region = v_newregionname), 
                                v_sectors_energysupply, v_newregionname, v_models, SUM=sum_type)
dev.off()

#-- Energy Demand sectors ------------------------------------------------------
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)

svglite(file=paste0("output/rfp_heatmap_3models_EnergyDemand_",v_scenario,"_", gsub("'| ", "", v_newregionname), "_dr", paste(dr*100), "pct.svg"))
draw_rfp_heatmap_region_diff_abs_byModel(data_plot_diff_abs %>%
                                           filter(region %in% v_region, scenario == v_scenario) %>% 
                                           mutate(region = v_newregionname), 
                                         v_sectors_energydemand, v_newregionname, v_models, SUM=sum_type)
dev.off()

