load("data/data_rfp_input.RData")

u_scenario_levels <- c(
  "Divergent Net Zero",
  "Delayed transition",
  "Net Zero 2050",
  "Below 2°C",
  "Nationally Determined Contributions (NDCs)",
  "Current Policies")


#== COMPUTE RFP ================================================================
data_rfp <- bind_rows(
  compute_rfp(data_rfp_input %>% filter(grepl("GCAM", model),    !grepl("REMIND|MESSAGE", region)), equations, assumptions, mapping_region_ngfs),
  compute_rfp(data_rfp_input %>% filter(grepl("MESSAGE", model), !grepl("GCAM|REMIND", region)),    equations, assumptions, mapping_region_ngfs),
  compute_rfp(data_rfp_input %>% filter(grepl("REMIND", model),  !grepl("GCAM|MESSAGE", region)),   equations, assumptions, mapping_region_ngfs)) %>% 
  left_join(data.frame(scenario = names(scen_baseline), baseline = paste(scen_baseline)),
            by="scenario")

# For transformation technologies, recalculate global indirect cost and revenues by 
# adding up regional ones for each model
#-- Update utility indirect cost for each model -------------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("GCAM", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("REMIND", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Electric Utilities", variable), grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
  )
  

#-- Update utility revenues for each model -------------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("GCAM", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("REMIND", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Electric Utilities", variable), grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update refineries indirect cost for each model ----------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("GCAM", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("REMIND", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Refineries|Oil", grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update refineries revenues for each model ----------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("GCAM", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("REMIND", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Refineries|Oil", grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update Bioenergy/Fischer-Tropsch plant indirect cost for each model -------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Indirect cost|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update Bioenergy/Fischer-Tropsch plant revenues for each model --------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(variable != "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model)),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(variable == "Diagnostics|RFP|Revenue|Energy Supply|Fischer-Tropsch Process|Biomass", grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update H2 plant indirect cost for each model -------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Indirect cost.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)

#-- Update H2 plant revenues for each model --------------------------------------
data_rfp <- bind_rows(
  # GCAM
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("GCAM", model), grepl("GCAM", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # MESSAGE
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("MESSAGE", model), grepl("MESSAGE", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline),
  # REMIND
  data_rfp %>%
    filter(!grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model)),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model), region != "World"),
  data_rfp %>% 
    filter(grepl("Diagnostics.RFP.Revenue.Energy Supply.Hydrogen Plants", variable), grepl("REMIND", model), grepl("REMIND", region)) %>% 
    group_by(model, scenario, variable, unit, period, baseline) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(region = "World") %>% 
    select(model, scenario, region, variable, unit, period, value, baseline)
)


scenario_baseline <- unique(data_rfp$baseline)[which(!is.na(unique(data_rfp$baseline)) & unique(data_rfp$baseline) != "NA")]

save(assumptions, equations, mapping_region_ngfs, data_rfp, scenario_baseline, scen_baseline, file="data/data_rfp_step1.RData")

# Clean data
data_rfp <- bind_rows(
  data_rfp %>% filter(grepl("GCAM", model),    period %in% seq(2005,2100,5)),
  data_rfp %>% filter(grepl("MESSAGE", model), period %in% c(seq(2005,2060,5), seq(2070,2100,10))),
  data_rfp %>% filter(grepl("REMIND", model),  period %in% c(seq(2005,2060,5), seq(2070,2100,10)))
)

data_rfp <- bind_rows(
  data_rfp %>% filter(period == 2020) %>% mutate(value = 0),
  data_rfp %>% filter(period != 2020))

# Compute Overall RFP effect
data_rfp_extended <- data_rfp %>% 
  mutate(value = ifelse(grepl("Direct emissions cost", variable), -value, value)) %>% 
  mutate(value = ifelse(grepl("Indirect cost", variable), -value, value)) %>%
  mutate(value = ifelse(grepl("Low-carbon capital expenditure", variable), -value, value)) %>% 
  mutate(rfp = "") %>% 
  mutate(rfp = ifelse(grepl("Direct emissions cost", variable),          "Diagnostics|RFP|Direct emissions cost", rfp)) %>% 
  mutate(rfp = ifelse(grepl("Indirect cost", variable),                  "Diagnostics|RFP|Indirect cost", rfp)) %>% 
  mutate(rfp = ifelse(grepl("Low-carbon capital expenditure", variable), "Diagnostics|RFP|Low-carbon capital expenditure", rfp)) %>% 
  mutate(rfp = ifelse(grepl("Revenue", variable),                        "Diagnostics|RFP|Revenue", rfp)) %>% 
  mutate(rfp = ifelse(grepl("Total Costs", variable),                    "Diagnostics|RFP|Total Costs", rfp)) %>% 
  mutate(sector = substr(variable, nchar(rfp)+2, nchar(variable))) %>% 
  select(-variable) %>% 
  pivot_wider(names_from = "rfp", values_from = "value") %>% 
  mutate(`Diagnostics|RFP|Total` = NAto0(`Diagnostics|RFP|Direct emissions cost`) + NAto0(`Diagnostics|RFP|Indirect cost`) +
           + NAto0(`Diagnostics|RFP|Revenue`)) %>% 
  mutate(`Diagnostics|RFP|Overall` = NAto0(`Diagnostics|RFP|Direct emissions cost`) + NAto0(`Diagnostics|RFP|Indirect cost`) +
           NAto0(`Diagnostics|RFP|Low-carbon capital expenditure`) + NAto0(`Diagnostics|RFP|Revenue`)) %>% 
  pivot_longer(names_to = "rfp", values_to = "value", cols = c("Diagnostics|RFP|Direct emissions cost",
                                                               "Diagnostics|RFP|Indirect cost",
                                                               "Diagnostics|RFP|Low-carbon capital expenditure",
                                                               "Diagnostics|RFP|Revenue",
                                                               "Diagnostics|RFP|Total",
                                                               "Diagnostics|RFP|Overall")) 

data_rfp_abs_diff <- data_rfp_extended %>% 
  left_join(data_rfp_extended %>% 
              filter(scenario %in% scenario_baseline) %>% 
              select(-baseline) %>% 
              rename(value_base=value),
            by=c("model"="model", "baseline"="scenario", "region"="region", "rfp"="rfp", "sector"="sector", "period"="period")) %>% 
  mutate(diff_abs=(value-value_base)*1e-3) %>% 
  select(model, scenario, region, rfp, sector, period, value, value_base, diff_abs, baseline) %>% 
  mutate(scenario = factor(scenario, 
                           levels = rev(u_scenario_levels), ordered = TRUE))


data_rfp_npv <- data_rfp_extended %>% 
  left_join(data_rfp_extended %>% 
              filter(scenario %in% scenario_baseline) %>% 
              select(-baseline) %>% 
              rename(value_base=value),
            by=c("model"="model", "baseline"="scenario", "region"="region", "rfp"="rfp", "sector"="sector", "period"="period", "unit"="unit")) %>% 
  rename(value_scen=value) %>%
  mutate(value=(value_scen-value_base)*1e-6) %>% 
  compute_npv(R=u_dr, TIMEHORIZON=2050) %>% 
  rename(diff_abs=value) %>% 
  left_join(data_rfp_extended %>%
              filter(scenario %in% scenario_baseline, grepl("Revenue", rfp)) %>%
              compute_npv(R=u_dr) %>%
              select(-rfp, -baseline) %>%
              mutate(value=value*1e-6) %>% 
              rename(value_norm=value),
            by=c("model"="model", "baseline"="scenario", "region"="region", "sector"="sector")) %>% 
  mutate(diff_rel = diff_abs/value_norm*100) %>% 
  select(model, scenario, region, rfp, sector, value_norm, diff_abs, diff_rel, baseline) %>%  #value_scen, value_base, 
  mutate(scenario = factor(scenario, 
                           levels = rev(u_scenario_levels), ordered = TRUE))

data_plot_diff_rel <- data_rfp_npv %>%
  mutate(variable = paste0(rfp, "|", sector)) %>% 
  select(model, scenario, region, variable, diff_rel) 

data_plot_diff_abs <- data_rfp_npv %>%
  mutate(variable = paste0(rfp, "|", sector)) %>% 
  select(model, scenario, region, variable, diff_abs) 

save(data_rfp_extended, data_rfp_abs_diff, data_rfp_npv, data_plot_diff_abs, data_plot_diff_rel, file="data/data_rfp_step2.RData")


rm("assumptions", "equations", "mapping_region_ngfs", "data_rfp_input")
gc()
