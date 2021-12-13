#extracting daily environmental data to topp data

library(tidyverse)
library(glue)
library(parallel)
library(raster)
library(foreach)
library(tibble)

## presences
source("/Users/EcoCast/Dropbox/OLE/github/OLE_Projects/utilities/extracto_OLE.R")

## 
master=read.csv("/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_validation_09_23_21.csv")
mast=master %>% rename(dt=date) %>% mutate(date=dt,lon=long360)  %>% filter(date<"2021-01-01")
extraction=multivariate_extraction_custom_resFCN(df=mast,res=69375,ncores=6, type="hw_validation")
write.csv(extraction,"/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_validation_bkgd_envt_09_23_21.csv")
