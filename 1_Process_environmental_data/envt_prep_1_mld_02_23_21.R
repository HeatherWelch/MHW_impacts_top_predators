# written by Heather Welch 02.23.21
# purpose of this script is to 1. delete all MLD files 2015 onwards, 
#2. edit code to uptake new netcdfs from Steph
# doing this because there is a new NRT product that fits historical better than what we used previously

## deleting old rasters ####
# dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
# Files=list.files(dailyDir,full.names = T,recursive = T)
# F2=grep("mld",Files,value=T)
# 
#  toMatch=c("2015","2016","2017","2018","2019","2020")
# 
# F3=unique(grep(paste(toMatch,collapse = "|"),F2,value=T))
# file.remove(F3)

## adding new data ####
library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir1="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_MLD_New/Historical"
fileDir2="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_MLD_New/NRT_New"
Files1=list.files(fileDir1,full.names = T,recursive = T)
Files2=list.files(fileDir2,full.names = T,recursive = T)
Files=list(Files1,Files2) %>% unlist()

writeRasterTemp=function(file,outdir,var){
  tryCatch(
    expr ={
      
  if(grepl("Historical",file)){
    date=substr(file,start=107,stop=114) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("NRT",file)){
    date=substr(file,start=102,stop=109) %>% as.Date(.,format="%Y%m%d")
  }
  
  savename=glue("{outdir}/{date}/{var}.grd")
  # savename_sd=glue("{outdir}/{date}/{var}_sd.grd")
  print(date)
  
    if(!file.exists(savename)){
      print("mld doesn't exist, writing out")
  r=raster(file,varname="mlotst") 
  if(extent(r)[1]<(-100)){
    r2=raster::shift(raster::rotate(raster::shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
  } else {r2=r}
  # r2=shift(rotate(shift(r,x=180)),x=180) # convert from kelvin to celsius and make 0-360
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

 mclapply(Files,FUN=writeRasterTemp,outdir=dailyDir,mc.cores=6,var="mld")
# lapply(Files,FUN=writeRasterTemp,outdir=dailyDir,var="mld")
