# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_Waves"
Files=list.files(fileDir,full.names = T,recursive = T) 

writeRasterTemp=function(file,outdir,var){
  tryCatch(
    expr ={
  if(grepl("Historical",file)){
    date=substr(file,start=84,stop=91) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("NRT",file)){
    date=substr(file,start=81,stop=88) %>% as.Date(.,format="%Y%m%d")
  }
  
  savename=glue("{outdir}/{date}/{var}.grd")
  # savename_sd=glue("{outdir}/{date}/{var}_sd.grd")
  print(date)
  
  if(!file.exists(savename)){
  print("file doesn't exist, writing out")
  r=brick(file,varname="VHM0") %>% mean(.,na.rm=T)
  r2=shift(rotate(shift(r,x=180)),x=180) 
  r_res=raster::resample(r2,template)
  # r_sd=focal(r_res,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)
  
  writeRaster(r_res,savename,overwrite=T)
  # writeRaster(r_sd,savename_sd,overwrite=T)
  }
  
    },
  error = function(e){
    message(glue("Something went wrong"))
    print(e)
  }
  )
 
}

# mclapply(Files,FUN=writeRasterTemp,outdir=dailyDir,mc.cores=6,var="sst")
lapply(Files,FUN=writeRasterTemp,outdir=dailyDir,var="wave")
