
v_sectorcategory <- list(
  "PrimaryEnergyProduction" = c("Energy Supply|Production|Oil", "Energy Supply|Production|Gas", 
                                "Energy Supply|Production|Coal",  "Energy Supply|Production|Biomass"),
  "LiquidFuelProduction"    = c("Energy Supply|Refineries|Oil", "Energy Supply|Fischer-Tropsch Process|Biomass"),
  "ElectricUtilities"       = c("Energy Supply|Electric Utilities|Coal", "Energy Supply|Electric Utilities|Gas", 
                                "Energy Supply|Electric Utilities|Nuclear", "Energy Supply|Electric Utilities|Biomass", 
                                "Energy Supply|Electric Utilities|Non-Biomass Renewables"),
  "HydrogenProduction"      = c("Energy Supply|Hydrogen Plants|Coal", "Energy Supply|Hydrogen Plants|Gas", 
                                "Energy Supply|Hydrogen Plants|Biomass", "Energy Supply|Hydrogen Plants|Electricity"),
  "EndUseEnergyDemand"      = c("End-Use|Residential and Commercial", "End-Use|Industry", "End-Use|Industry|Cement", 
                                "End-Use|Industry|Chemicals", "End-Use|Industry|Steel", "End-Use|Transportation")
  
)

v_regioncategory <- list(
  "China" = c("GCAM5.3_NGFS|China", 
              "MESSAGEix-GLOBIOM 1.0|Centrally Planned Asia and China", 
              "REMIND-MAgPIE 2.1-4.2|China"),
  "EU"    = c("GCAM5.3_NGFS|EU-12", 
              "MESSAGEix-GLOBIOM 1.0|Western Europe", 
              "REMIND-MAgPIE 2.1-4.2|EU 28"),
  "India" = c("GCAM5.3_NGFS|India", 
              "MESSAGEix-GLOBIOM 1.0|South Asia", 
              "REMIND-MAgPIE 2.1-4.2|India"),
  "USA"   = c("GCAM5.3_NGFS|USA", 
              "MESSAGEix-GLOBIOM 1.0|North America", 
              "REMIND-MAgPIE 2.1-4.2|United States of America"))

v_regioncategory <- list(
  "China" = c("GCAM5.3_NGFS|China", 
              "MESSAGEix-GLOBIOM 1.1|R11_CPA", 
              "REMIND-MAgPIE 2.1-4.2|CHA"),
  "EU"    = c("GCAM5.3_NGFS|EU-12", 
              "MESSAGEix-GLOBIOM 1.1|R11_WEU", 
              "REMIND-MAgPIE 2.1-4.2|EUR"),
  "India" = c("GCAM5.3_NGFS|India", 
              "MESSAGEix-GLOBIOM 1.1|R11_SAS", 
              "REMIND-MAgPIE 2.1-4.2|IND"),
  "USA"   = c("GCAM5.3_NGFS|USA", 
              "MESSAGEix-GLOBIOM 1.1|R11_NAM", 
              "REMIND-MAgPIE 2.1-4.2|USA"))

v_modelshortname <- c(
  "GCAM"    = "GCAM5.3_NGFS",
  "MESSAGE" = "MESSAGEix-GLOBIOM 1.0",
  "REMIND"  = "REMIND-MAgPIE 2.1-4.2" )

v_models <- unique(data_rfp_abs_diff$model)


#-- Net Zero 2050 / 3 models / World (discount rate: 5%) -------------------
for (kmodel in v_models) {
  v_modelname <- names(v_modelshortname)[which(v_modelshortname == kmodel)]
  for (kseccat in names(v_sectorcategory)) {
    plots <- list()
    for (ksec in v_sectorcategory[[kseccat]]) {
      plots[[ksec]] <- plot_rfp_pathways(data_rfp_abs_diff %>% filter(grepl(kmodel, model)), 
                                         ksec, 
                                         u_scenario, u_region, HORIZON=u_horizon, DISCOUNT=u_dr, PRINT=FALSE) + 
        ggtitle(paste0(ksec, " - (Disc: 5%)"))
    }
    ggarrange(plotlist=plots, common.legend = TRUE, legend = "bottom", align = c("hv"))
    ggsave(paste0("output/rfp_pathways_", kseccat, "_", v_modelname, "_", u_scenario, "_", v_region, "_", round(u_dr*100, digits = 0), "pct.png"))
  }
}

#-- Net Zero 2050 / 3 models / 4 regions (China/EU/India/USA) (discount rate: 5%) -------------------
for (kregion in names(v_regioncategory)) {
  for (kmodel in v_models) {
    v_region <- grep(kmodel, v_regioncategory[[kregion]], value=TRUE)
    v_modelname <- names(v_modelshortname)[which(v_modelshortname == kmodel)]
    for (kseccat in names(v_sectorcategory)) {
      plots <- list()
      for (ksec in v_sectorcategory[[kseccat]]) {
        plots[[ksec]] <- plot_rfp_pathways(data_rfp_abs_diff %>% filter(grepl(kmodel, model)), 
                                           ksec, 
                                           u_scenario, v_region, HORIZON=u_horizon, DISCOUNT=u_dr, PRINT=FALSE) + 
          ggtitle(paste0(ksec, " - (Disc: 5%)"))
      }
      ggarrange(plotlist=plots, common.legend = TRUE, legend = "bottom", align = c("hv"))
      ggsave(paste0("output/rfp_pathways_", kseccat, "_", v_modelname, "_", u_scenario, "_", kregion, "_", round(u_dr*100, digits = 0), "pct.png"))
    }
  }
}


