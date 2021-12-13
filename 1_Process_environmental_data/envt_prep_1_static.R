# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"

# bathymetry and rugosity ####
bathy=raster("/Volumes/Triple_Bottom_Line/Data/GlobalData/ETOPO180_Global/etopo180_4558_b013_12d8.nc",varname="altitude")
bathy2=shift(rotate(shift(bathy,dx=180)),dx=180)
bathy_res=raster::resample(bathy2,template)
bathy_res[values(bathy_res)>=0]=NA
bathy_res_sd=focal(bathy_res,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)

folder_list=list.files(dailyDir,full.names = T) %>% .[14600:length(.)]

writeRasterBathy=function(folder,bathy_res,bathy_res_sd){
  print(folder)
  writeRaster(bathy_res,glue("{folder}/bathy.grd"),overwrite=T)
  writeRaster(bathy_res_sd,glue("{folder}/bathy_sd.grd"),overwrite=T)
}

lapply(folder_list,FUN=writeRasterBathy,bathy_res = bathy_res,bathy_res_sd = bathy_res_sd)

# distance to shore, NTA, EEZ ####
# these files are taken from Catena/GFW work: create_prediction_layers_non_dynamic_2017.R
# eez="/Users/EcoCast/Dropbox/IUU_GRW/Environmental_data/rasters_2017/static/dist_eez.grd" %>% raster()
# nta="/Users/EcoCast/Dropbox/IUU_GRW/Environmental_data/rasters_2017/static/dist_NTA.grd" %>% raster()
# shore="/Users/EcoCast/Dropbox/IUU_GRW/Environmental_data/rasters/static/dist_shore.grd" %>% raster()
# 
# eez2=shift(rotate(shift(eez,x=180)),x=180)
# nta2=shift(rotate(shift(nta,x=180)),x=180)
# shore2=shift(rotate(shift(shore,x=180)),x=180)
# 
# eez_res=raster::resample(eez2,template)
# nta_res=raster::resample(nta2,template)
# shore_res=raster::resample(shore2,template)

# folder_list=list.files(dailyDir,full.names = T)

eez_res=glue("{folder_list[1]}/dist_eez.grd") %>% raster()
nta_res=glue("{folder_list[1]}/dist_nta.grd") %>% raster()
shore_res=glue("{folder_list[1]}/dist_shore.grd") %>% raster()

writeRasterDistance=function(folder,eez_res,nta_res,shore_res){
  print(folder)
  writeRaster(eez_res,glue("{folder}/dist_eez.grd"),overwrite=T)
  writeRaster(nta_res,glue("{folder}/dist_nta.grd"),overwrite=T)
  writeRaster(shore_res,glue("{folder}/dist_shore.grd"),overwrite=T)
}

lapply(folder_list[2:length(folder_list)],FUN=writeRasterDistance,eez_res = eez_res,nta_res = nta_res,shore_res = shore_res)
