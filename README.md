# README
Marcus W. Beck, beck.marcus@epa.gov  

### Files

Files were created in `sf_trends.RProj`

`clams.RData` clam data for D7 from Crauder 2016, includes sample date, water year (`yr` starting in October), biomass in g/m2, clams per sample (`clams_smp`) in no/m2, depth (m), growth rate (`gr`) in m3/m2/d, mean size (mm), number of grabs (`no_grabs`), recruitment per unit area (`recruit_area`) in recruit/0.05 m2, and species as corbicula or potamocorbula.

`delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results

`delt_map.RData` SpatialPolygon object of generalized delta map

`flow_dat.RData` time series of daily flow estimates for the delta, input stations from Novick et al (Fig 2) were used

`h1dat.RData` mean models to support first hypothesis/case study in manuscript, wrtds mean models at c10 

`h2dat.RData` mean models to support second hypothesis/case study in manuscript, wrtds mean models for no23, nh at p8

`h3dat.RData` mean models to support third hypothesis/case study in manuscript, wrtds mean models for sio2, din, chla at c10, d7

`mods.RData` complete dataset for wrtds, including model results.

`stock_conc.RData` summarized effluent data from Stockton wwtp, mg/L

`stock_load.RData` nutrient load data from Stockton, kg/day

`trnds_ave.RData` trend summary of averages by annual, monthly categories

`trnds_chg.RData` trend summary of percent changes by annual, monthly categories

`trnds_sk.RData` trend summary using seasonal kendall on flow-normalized results
