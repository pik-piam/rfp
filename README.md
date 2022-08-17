# RFPs: Risk Factor Pathways

## About
This repository contains a set of scripts and functions to compute Risk Factor Pathways (RFPs). 
RFPs translate climate mitigation scenario data into costs and revenues for several economic sectors that can more easily be integrated into risk assessment frameworks employed by banks and other financiers.
Here they are applied to the [NGFS scenarios](https://www.ngfs.net/ngfs-scenarios-portal).
They are coded in R.

## How to run
The script main.R calls in a sequential manner all the scripts and functions to transform the NGFS scenario data, compute RFPs, create line plots and heatmaps and generate reports. 

## Data
We use version 2.1 of the NGFS scenario data which is freely available [here](https://data.ene.iiasa.ac.at/ngfs).
