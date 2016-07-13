# README
Marcus W. Beck, beck.marcus@epa.gov  

### Files

Files marked with * were created in `sf_trends.RProj`, all others created in `R\dat_proc.R`

`clams.RData`* clam data for D7 from Crauder 2016, includes sample date, water year (`yr` starting in October), biomass in g/m2, clams per sample (`clams_smp`) in no/m2, depth (m), growth rate (`gr`) in m3/m2/d, mean size (mm), number of grabs (`no_grabs`), recruitment per unit area (`recruit_area`) in recruit/0.05 m2, and species as corbicula or potamocorbula.

`delt_dat.RData`* Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results

`dinc10.RData` WRTDS model for DIN at C10

`flow_dat.RData`* time series of daily flow estimates for the delta, input stations from Novick et al (Fig 2) were used

`h3dat.RData`* mean models to support third hypothesis/case study in manuscript, wrtds mean models for sio2, din, chla at c10, d7

`mods_nolag.RData`* complete dataset for wrtds, including model results.  This is the same file as `mods_lag.RData` except the matched flow variables are not lagged. Nested, do not load without loading tidyr.

`potw_load.RData`* nutrient load data from Stockton and Tracy wwtp, kg/day
