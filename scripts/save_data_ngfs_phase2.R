#== Scenario and region mappings ===============================================
scen_names_ngfs <- c("Current Policies"                           = "h_cpol", 
                     "Nationally Determined Contributions (NDCs)" = "h_ndc", 
                     "Below 2°C"                                  = "o_2c", 
                     "Net Zero 2050"                              = "o_1p5c", 
                     "Delayed transition"                         = "d_delfrag",
                     "Divergent Net Zero"                         = "d_rap")

region_names_ngfs <- c(
  "REMIND-MAgPIE 2.1-4.2|CAZ" = "REMIND-MAgPIE 2.1-4.2|Canada, NZ, Australia",
  "REMIND-MAgPIE 2.1-4.2|CHA" = "REMIND-MAgPIE 2.1-4.2|China",
  "REMIND-MAgPIE 2.1-4.2|EUR" = "REMIND-MAgPIE 2.1-4.2|EU 28",
  "REMIND-MAgPIE 2.1-4.2|IND" = "REMIND-MAgPIE 2.1-4.2|India",
  "REMIND-MAgPIE 2.1-4.2|JPN" = "REMIND-MAgPIE 2.1-4.2|Japan",
  "REMIND-MAgPIE 2.1-4.2|LAM" = "REMIND-MAgPIE 2.1-4.2|Latin America and the Caribbean",
  "REMIND-MAgPIE 2.1-4.2|MEA" = "REMIND-MAgPIE 2.1-4.2|Middle East, North Africa, Central Asia",
  "REMIND-MAgPIE 2.1-4.2|NEU" = "REMIND-MAgPIE 2.1-4.2|Non-EU28 Europe",
  "REMIND-MAgPIE 2.1-4.2|OAS" = "REMIND-MAgPIE 2.1-4.2|other Asia",
  "REMIND-MAgPIE 2.1-4.2|REF" = "REMIND-MAgPIE 2.1-4.2|Countries from the Reforming Ecomonies of the Former Soviet Union",
  "REMIND-MAgPIE 2.1-4.2|SSA" = "REMIND-MAgPIE 2.1-4.2|Sub-saharan Africa",
  "REMIND-MAgPIE 2.1-4.2|USA" = "REMIND-MAgPIE 2.1-4.2|United States of America",
  "World" = "World"
)


scen_names_ngfs <- data.frame(scen_names_ngfs) %>% 
  rownames_to_column()
names(scen_names_ngfs) <- c("scenario", "c_scenario")

region_names_ngfs <- data.frame(region_names_ngfs) %>% 
  rownames_to_column()
names(region_names_ngfs) <- c("c_region", "region")

#== LOAD NGFS scenario data ====================================================
data_ngfs2_raw <- read_excel(u_ngfs_scenario_path)

names(data_ngfs2_raw) <- c("model", "scenario", "region", "variable", "unit", paste0(c(1990,1995,2000:2100)))
data_ngfs2_raw <- data_ngfs2_raw %>% 
  select(c("model", "scenario", "region", "variable", "unit", paste0(c(seq(2005,2060,5), seq(2070,2100,10)))))

data_ngfs2_raw <- data_ngfs2_raw %>% 
  filter(!grepl("_d[5-9][0-5]", scenario)) %>% 
  filter(!model %in% c("REMIND-MAgPIE 2.1-4.2 IntegratedPhysicalDamages (95th)", "REMIND-MAgPIE 2.1-4.2 IntegratedPhysicalDamages (median)")) %>% 
  filter(grepl("World|GCAM|MESSAGE|REMIND", region)) %>% 
  filter(!grepl("REMIND-MAgPIE 2.1-4.2 IntegratedPhysicalDamages", region)) %>% 
  pivot_longer(names_to="period", values_to="value", cols = c(paste0(c(seq(2005,2060,5), seq(2070,2100,10))))) %>% 
  mutate(period=as.numeric(period))

load("data/ngfs/load/data_ngfs_remind_capaddH2_addon.RData")

data_ngfs_remind_capaddH2_addon <- data_ngfs_remind_capaddH2_addon %>% 
  rename(c_scenario=scenario) %>% 
  left_join(scen_names_ngfs, by=c("c_scenario"))  %>% 
  left_join(region_names_ngfs, by=c("region")) %>% 
  select(-region) %>% 
  rename(region=c_region) %>% 
  select(model, scenario, region, variable, unit, period, value)

data_ngfs2 <- data_ngfs2_raw %>% #data_ngfs %>% 
  filter(model != "Reference") %>% 
  filter(!is.na(period), !is.na(value)) %>% 
  filter(period >= 2005) %>% 
  select(model, scenario, region, variable, unit, period, value) %>% 
  # Add H2 capacity data from REMIND
  rbind(data_ngfs_remind_capaddH2_addon %>% 
          select(model, scenario, region, variable, unit, period, value))


#-- Compute Secondary Energy Oil price without carbon price --------------------
# This is done to obtain a more realistic approximation of revenues for refineries
vars <- c("Final Energy|Transportation|Liquids|Oil",
          "Final Energy|Transportation|Liquids",
          "Final Energy|Industry|Liquids", 
          "Final Energy|Residential and Commercial|Liquids",
          "Price|Secondary Energy|Liquids|Oil",
          "Secondary Energy|Liquids|Oil",
          "Secondary Energy|Liquids",
          "Price|Carbon|Demand|Transportation",
          "Price|Carbon|Demand|Industry",
          "Price|Carbon|Demand|Residential and Commercial")

data_ngfs2 <- data_ngfs2 %>% 
  rbind(data_ngfs2 %>% 
          select(model, scenario, region, variable, period, value) %>% 
          filter(region != "World", variable %in% vars) %>% 
          spread(variable, value) %>% 
          mutate(ef = 18.4*44/12) %>% # GtC / ZJ -> GtCO2 / ZJ -> 1e3 MtCO2 / 1e3 EJ
          mutate(`Final Energy|Transportation|Liquids|Oil` = ifelse(is.na(`Final Energy|Transportation|Liquids|Oil`), `Final Energy|Transportation|Liquids`*`Secondary Energy|Liquids|Oil`/`Secondary Energy|Liquids`, `Final Energy|Transportation|Liquids|Oil`)) %>% 
          mutate(`FE|Oil` = `Final Energy|Transportation|Liquids|Oil` + `Final Energy|Industry|Liquids` + `Final Energy|Residential and Commercial|Liquids`) %>% #*(`Secondary Energy|Liquids|Oil`/`Secondary Energy|Liquids`)
          mutate(`Price|Carbon|Oil` = ef*(                                                                 # MtCO2/EJ
              `Price|Carbon|Demand|Transportation`             * `Final Energy|Transportation|Liquids|Oil`        /`FE|Oil`+
              `Price|Carbon|Demand|Industry`                   * `Final Energy|Industry|Liquids`                  /`FE|Oil` +
              `Price|Carbon|Demand|Residential and Commercial` * `Final Energy|Residential and Commercial|Liquids`/`FE|Oil`) * # US$ / tCO2 * EJ * MtCO2 / EJ -> 1e6 US$ / EJ -> 1e6 US$ / 1e6 TJ
              1e-3) %>% # US$/ GJ
          mutate(`Price|Secondary Energy|Liquids|Oil|w/o Carbon Price` = `Price|Secondary Energy|Liquids|Oil` - `Price|Carbon|Oil`) %>% 
          select(model, scenario, region, period, `Price|Secondary Energy|Liquids|Oil|w/o Carbon Price`, `Price|Secondary Energy|Liquids|Oil`, `Price|Carbon|Oil`) %>% 
          gather(variable, value, -model, -scenario,- region, -period) %>% 
          mutate(unit = "US$2010/GJ") %>% 
          select(model, scenario, region, variable, unit, period, value) %>% 
          filter(variable == "Price|Secondary Energy|Liquids|Oil|w/o Carbon Price")) 


# Add total secondary energy by energy source
data_ngfs2 <- data_ngfs2 %>% 
  rbind(data_ngfs2 %>% 
          select(model, scenario, region, variable, period, value) %>% 
          filter(grepl("^Secondary Energy.*Coal$", variable)) %>% 
          group_by(model, scenario, region, period) %>% 
          summarise(value = sum(value, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(variable = "Secondary Energy|Coal") %>% 
          mutate(unit = "EJ/yr") %>% 
          select(model, scenario, region, variable, unit, period, value)) %>% 
  rbind(data_ngfs2 %>% 
          select(model, scenario, region, variable, period, value) %>% 
          filter(grepl("^Secondary Energy.*Gas$", variable)) %>% 
          group_by(model, scenario, region, period) %>% 
          summarise(value = sum(value, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(variable = "Secondary Energy|Gas") %>% 
          mutate(unit = "EJ/yr") %>% 
          select(model, scenario, region, variable, unit, period, value)) %>% 
  rbind(data_ngfs2 %>% 
          select(model, scenario, region, variable, period, value) %>% 
          filter(grepl("^Secondary Energy.*Biomass$", variable)) %>% 
          group_by(model, scenario, region, period) %>% 
          summarise(value = sum(value, na.rm = TRUE)) %>% #, .groups="keep"
          ungroup() %>% 
          mutate(variable = "Secondary Energy|Biomass") %>% 
          mutate(unit = "EJ/yr") %>% 
          select(model, scenario, region, variable, unit, period, value)) 


# Add missing/new variables (these are approximations, it would be better to get these variables directly from the models)
data_ngfs2 <- bind_rows(
  data_ngfs2 %>% filter(grepl("GCAM", model), period %in% seq(2005,2100,5)) %>% 
    calc_addVariable_old_(
      list(
        "`Secondary Energy|Oil`" = "`Secondary Energy|Electricity|Oil` + `Secondary Energy|Liquids|Oil`"
      )
    ),
  data_ngfs2 %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Secondary Energy|Oil`" = "`Secondary Energy|Electricity|Oil` + `Secondary Energy|Liquids|Oil`"
      )
    ),
  data_ngfs2 %>% filter(grepl("REMIND", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Secondary Energy|Oil`" = "`Secondary Energy|Electricity|Oil` + `Secondary Energy|Liquids|Oil`"
      )
    ))

data_ngfs2 <- bind_rows(
  data_ngfs2 %>% filter(grepl("GCAM", model), period %in% seq(2005,2100,5)) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Coal|Electricity`"          = "`Primary Energy|Coal` * `Secondary Energy|Electricity|Coal`         / `Secondary Energy|Coal`",
        "`Primary Energy|Coal|Electricity|w/o CCS`"  = "`Primary Energy|Coal` * `Secondary Energy|Electricity|Coal|w/o CCS` / `Secondary Energy|Coal`",
        "`Primary Energy|Coal|Electricity|w/ CCS`"   = "`Primary Energy|Coal` * `Secondary Energy|Electricity|Coal|w/ CCS`  / `Secondary Energy|Coal`",
        "`Primary Energy|Oil|Electricity`"           = "`Primary Energy|Oil`  * `Secondary Energy|Electricity|Oil`          / `Secondary Energy|Oil`",
        "`Primary Energy|Gas|Electricity`"           = "`Primary Energy|Gas`  * `Secondary Energy|Electricity|Gas`          / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Electricity|w/o CCS`"   = "`Primary Energy|Gas`  * `Secondary Energy|Electricity|Gas|w/o CCS`  / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Electricity|w/ CCS`"    = "`Primary Energy|Gas`  * `Secondary Energy|Electricity|Gas|w/ CCS`   / `Secondary Energy|Gas`",
        "`Primary Energy|Biomass|Electricity`"       = "`Primary Energy|Biomass` * `Secondary Energy|Electricity|Biomass`   / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Electricity|w/o CCS`" = "`Primary Energy|Biomass` * `Secondary Energy|Electricity|Biomass|w/o CCS`  / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Electricity|w/ CCS`"  = "`Primary Energy|Biomass` * `Secondary Energy|Electricity|Biomass|w/ CCS`   / `Secondary Energy|Biomass`"
      )
    ),
  data_ngfs2 %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))),
  data_ngfs2 %>% filter(grepl("REMIND", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Oil|Electricity`"           = "`Primary Energy|Oil`  * `Secondary Energy|Electricity|Oil`          / `Secondary Energy|Oil`"
      )
    ))

data_ngfs2 <- bind_rows(
  data_ngfs2 %>% filter(grepl("GCAM", model), period %in% seq(2005,2100,5)) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Biomass|Liquids`"           = "`Primary Energy|Biomass`  * `Secondary Energy|Liquids|Biomass` / `Secondary Energy|Biomass`"
      )
    ),
  data_ngfs2 %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))),
  data_ngfs2 %>% filter(grepl("REMIND", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Biomass|Liquids`"           = "`Primary Energy|Biomass`  * `Secondary Energy|Liquids|Biomass` / `Secondary Energy|Biomass`"
      )
    ))

data_ngfs2 <- bind_rows(
  data_ngfs2 %>% filter(grepl("GCAM", model), period %in% seq(2005,2100,5)) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Coal|Hydrogen`"            = "`Primary Energy|Coal` * `Secondary Energy|Hydrogen|Coal` / `Secondary Energy|Coal`",
        "`Primary Energy|Gas|Hydrogen`"             = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/o CCS`"     = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/o CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/ CCS`"      = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/ CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Biomass|Hydrogen`"         = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/o CCS`" = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/o CCS` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/ CCS`"  = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/ CCS` / `Secondary Energy|Biomass`"
      )
    ),
  data_ngfs2 %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Coal|Hydrogen`"            = "`Primary Energy|Coal` * `Secondary Energy|Hydrogen|Coal` / `Secondary Energy|Coal`",
        "`Primary Energy|Gas|Hydrogen`"             = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/o CCS`"     = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/o CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/ CCS`"      = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/ CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Biomass|Hydrogen`"         = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/o CCS`" = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/o CCS` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/ CCS`"  = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/ CCS` / `Secondary Energy|Biomass`"
      )
    ),
  data_ngfs2 %>% filter(grepl("REMIND", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>% 
    calc_addVariable_old_(
      list(
        "`Primary Energy|Coal|Hydrogen`"            = "`Primary Energy|Coal` * `Secondary Energy|Hydrogen|Coal` / `Secondary Energy|Coal`",
        "`Primary Energy|Gas|Hydrogen`"             = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/o CCS`"     = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/o CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Gas|Hydrogen|w/ CCS`"      = "`Primary Energy|Gas`  * `Secondary Energy|Hydrogen|Gas|w/ CCS` / `Secondary Energy|Gas`",
        "`Primary Energy|Biomass|Hydrogen`"         = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/o CCS`" = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/o CCS` / `Secondary Energy|Biomass`",
        "`Primary Energy|Biomass|Hydrogen|w/ CCS`"  = "`Primary Energy|Biomass` * `Secondary Energy|Hydrogen|Biomass|w/ CCS` / `Secondary Energy|Biomass`"
      )
    ))


data_ngfs2 <- bind_rows(
  data_ngfs2 %>% filter(grepl("GCAM", model), period %in% seq(2005,2100,5)),
  data_ngfs2 %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))),
  data_ngfs2 %>%
  filter(grepl("REMIND", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))) %>%
    calc_addVariable_old_(
      list(
        "`Diagnostics|Investment|Energy Supply|Hydrogen|Biomass`"     = "`Diagnostics|Investment|Energy Supply|Hydrogen|Renewable` * ((`Capacity Additions|Hydrogen|Biomass|w/ CCS`*`Capital Cost|Hydrogen|Biomass|w/ CCS` + `Capacity Additions|Hydrogen|Biomass|w/o CCS`*`Capital Cost|Hydrogen|Biomass|w/o CCS`)/(`Capacity Additions|Hydrogen|Biomass|w/ CCS`*`Capital Cost|Hydrogen|Biomass|w/ CCS` + `Capacity Additions|Hydrogen|Biomass|w/o CCS`*`Capital Cost|Hydrogen|Biomass|w/o CCS` + `Capacity Additions|Hydrogen|Electricity`*`Capital Cost|Hydrogen|Electricity`))",
        "`Diagnostics|Investment|Energy Supply|Hydrogen|Electricity`" = "`Diagnostics|Investment|Energy Supply|Hydrogen|Renewable` * ((`Capacity Additions|Hydrogen|Electricity`*`Capital Cost|Hydrogen|Electricity`)/(`Capacity Additions|Hydrogen|Biomass|w/ CCS`*`Capital Cost|Hydrogen|Biomass|w/ CCS` + `Capacity Additions|Hydrogen|Biomass|w/o CCS`*`Capital Cost|Hydrogen|Biomass|w/o CCS` + `Capacity Additions|Hydrogen|Electricity`*`Capital Cost|Hydrogen|Electricity`))"
      )))

rm("data_ngfs2_raw", "data_ngfs_remind_capaddH2_addon")
gc()

save(data_ngfs2, file="data/data_ngfs2.RData")

