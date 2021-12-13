# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_MLD_New"
Files=list.files(fileDir,full.names = T,recursive = T) %>% 
grep("degree_OLD",.,invert=T,value = T)

writeRasterTemp=function(file,outdir,var){
  tryCatch(
    expr ={
      print(file)
      
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
