#extracting daily environmental data to topp data

library(tidyverse)
library(glue)
library(parallel)
library(raster)
library(foreach)
library(tibble)

source("/Users/EcoCast/Dropbox/OLE/github/OLE_Projects/utilities/extracto_OLE.R")
master=read.csv("/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_03_22_21.csv")
# mast=master %>% rename(dt=date,date=date2,lon=long360) 
mast=master %>% rename(dt=date2) %>% mutate(date=dt,lon=long360)
extraction=multivariate_extraction_custom_resFCN(df=mast[1:50,],res=69375,ncores=6, type="TOPP")
write.csv(extraction,"/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_bkgd_envt_A_03_22_21.csv")
