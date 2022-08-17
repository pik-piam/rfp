#== USER SECTION ===============================================================
u_dr       <- 0.05             # Discount rate (e.g. 5%) used for NPV calculations
u_horizon  <- 2050             # Time horizon for calculating NPVs
u_scenario <- "Net Zero 2050"  # Chosen scenario for plotting RFPs
u_region   <- "World"          # Chosen region for plotting RFPs
u_model    <- "REMIND"         # Chosen model for plotting RFPs (e.g. GCAM, MESSAGE, REMIND)
# Path to NGFS scenario data
u_ngfs_scenario_path <- "data/ngfs/1624888306124-NGFS_Scenario_Data_IAM_outputs_V2.2.xlsx"


#== INITIALISE =================================================================
#-- Load libraries -------------------------------------------------------------
library(tidyverse)
library(rlang)
library(lazyeval)
library(readxl)
library(ggpubr)
library(gridExtra)
library(svglite)

#-- Load own functions ---------------------------------------------------------
source("functions/check_data_availability.R")
source("functions/tool_functions.R")
source("functions/rfp.R")
source("functions/plotting_functions.R")


#== READ AND PROCESS SCENARIO DATA =============================================
source("scripts/save_data_ngfs_phase2.R")
# Run time: approx. 1 min
source("scripts/read_and_process_scenario_data.R")


#== COMPUTE Risk Factor Pathways ===============================================
# Run time: 1 min
source("scripts/compute_rfp.R")


#== PLOT RFPs ==================================================================
source("scripts/plot_RFPs.R")


#== PLOT HEATMAPS ==============================================================
source("scripts/plot_heatmaps.R")


#== PLOT FIGURES FOR MANUSCRIPT ================================================
source("scripts/generate_plots_figure2.R")
source("scripts/generate_plots_figures3and4.R")


#== CREATE REPORTS =============================================================
# RFP equations report
#(see and run markdown/rfp_report_var_avail_html.Rmd)
#(see and run markdown/rfp_report_var_avail_docx.Rmd)
# RFP pathways report
#(see and run markdown/rfp_report_html.Rmd)
#(see and run markdown/rfp_report_docx.Rmd)
# RFP heatmap report
#(see and run markdown/rfp_heatmap_report_docx.Rmd)