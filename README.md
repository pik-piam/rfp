# RFPs: Risk Factor Pathways

## About
This repository contains a set of scripts and functions to compute Risk Factor Pathways (RFPs). 
RFPs translate climate mitigation scenario data into costs and revenues for several economic sectors that can more easily be integrated into risk assessment frameworks employed by banks and other financiers.
Here they are applied to the [NGFS scenarios](https://www.ngfs.net/ngfs-scenarios-portal).
They are coded in R.

## How to run
### Download the NGFS scenario dataset
Go to the [the IIASA scenario explorer](https://data.ene.iiasa.ac.at/ngfs). You can login as guest or create an account. Then go to the Downloads section (accessible from the top blue bar). Click on the link named "Phase 2 NGFS Scenario outputs from the GCAM, REMIND-MAGPIE and MESSAGEix-GLOBIOM models (correction of several REMIND variables for Net Zero 2050 Scenario) V2.2" (it should be the 5th one).

### Where to place the NGFS scenario dataset?
Place the file 1623321063428-NGFS_Scenario_Data_IAM_outputs_V2.1.xlsx in the folder data/ngfs
 
 ### Run
The script main.R calls in a sequential manner all the scripts and functions to transform the NGFS scenario data, compute RFPs, create line plots and heatmaps and generate reports. 

## Data
We use version 2.1 of the NGFS scenario data which is freely available [here](https://data.ene.iiasa.ac.at/ngfs).
