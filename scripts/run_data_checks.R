library(tidyverse)
library(rlang)
library(lazyeval)

source("functions/rfp.R")
source("functions/tool_functions.R")
source("functions/check_data_availability.R")
load("data/ngfs/load/data_ngfs2.RData")

scen_baseline <- c(
  "h_cpol"    = NA,
  "h_ndc"     = "h_cpol",
  "o_2c"      = "h_cpol",
  "o_1p5c"    = "h_cpol",
  "d_delfrag" = "h_cpol",
  "d_rap"     = "h_cpol"
)

equations <- readxl::read_excel("data/rfp_definitions.xlsx", sheet = "equations")

mapping_region_ngfs <- read.csv2("data/ngfs/regional_mapping_ngfs.csv")

assumptions <- list(
  "Direct emissions cost" = list(
    "Unit conversion" = inline.data.frame(c(
      "EJ to GJ conversion; t to Mt; $BN to $; 2015 to 2010 USD; 2007 to 2010 USD; 2005 to 2010 USD; kt to t; kt to Mt; C to CO2",
      "1e9;                 1e6;     1e9;      0.920;            1.052;            1.117;            1e3;     1e-3;     3.666667")),
    "CO2 emission factors" = inline.data.frame(c(
      "coal_mining; oil_production; gas_production; coal_noccs; coal_ccs; oil_noccs; oil_ccs; gas_noccs; gas_ccs",
      "10.12327;    10.55672;       12.15683;       26.1;       2.6;      18.4;      1.8;     15.3;      1.5"))
  ),
  "Indirect cost" = list(
    "Unit conversion" = inline.data.frame(c(
      "EJ to GJ conversion; t to Mt; $BN to $; 2015 to 2010 USD; 2007 to 2010 USD; 2005 to 2010 USD; kt to t; C to CO2",
      "1e9;                 1e6;     1e9;      0.920;            1.052;            1.117;            1e3;     3.666667"))
  ),
  "Low-carbon capital expenditure" = list(
    "Unit conversion" = inline.data.frame(c(
      "EJ to GJ conversion; t to Mt; $BN to $; 2015 to 2010 USD; 2007 to 2010 USD; 2005 to 2010 USD; kt to t; C to CO2",
      "1e9;                 1e6;     1e9;      0.920;            1.052;            1.117;            1e3;     3.666667")),
    "Share of low-carbon energy efficiency investments for the three end-use sectors" = data.frame(
      region                      = c("CHN", "EUR", "IND", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF", "USA", "World"),
      ee_inv_ratio_industry       = c(0.30,  0.20,  0.30,   0.30,    0.30,    0.30,     0.20,         0.30,     0.20,  0.30),
      ee_inv_ratio_buildings      = c(0.35,  0.40,  0.35,   0.35,    0.35,    0.35,     0.40,         0.35,     0.40,  0.35),
      ee_inv_ratio_transportation = c(0.35,  0.40,  0.35,   0.35,    0.35,    0.35,     0.40,         0.35,     0.40,  0.35))
  ),
  "Revenue" = list(       
    "Unit conversion" = inline.data.frame(c(
      "EJ to GJ conversion; t to Mt; $BN to $; 2015 to 2010 USD; 2007 to 2010 USD; 2005 to 2010 USD; kt to t; C to CO2",
      "1e9;                 1e6;     1e9;      0.920;            1.052;            1.117;            1e3;     3.666667"))
  )
)

checks <- check_data_availability(data_ngfs2, equations, assumptions)

missing_data <- lapply(checks,
                       function(x) {
                         lapply(x$`equation information`, 
                                function(y) {
                                  lapply(y, 
                                         function(z) {
                                           z$`missing data`
                                         }
                                  ) %>% 
                                    do.call("rbind", .)
                                }
                         ) %>% do.call("rbind", .) 
                       }) %>% do.call("rbind", .) 
rownames(missing_data) <- NULL
missing_data <- missing_data %>% 
  unite("all", model, scenario, region, variable, period, remove = FALSE) %>% 
  filter(!duplicated(all)) %>% 
  select(-all)


save(checks, missing_data, scen_baseline, mapping_region_ngfs, assumptions, equations, file = "data/data_checks.RData")
