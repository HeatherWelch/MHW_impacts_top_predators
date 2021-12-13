# written by Heather Welch 07.01.20

##### NEED TO DECIDE DEPTH LEVEL/S
### NEED TO DOUBLE CHECK ALL DATES

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_Oxygen"
Files=list.files(fileDir,full.names = T,recursive = T) 

writeRasterTemp=function(file,outdir,var){
  tryCatch(
    expr ={
      if(grepl("Historical",file)){
        date=substr(file,start=147,stop=154) %>% as.Date(.,format="%Y%m%d")
      } else if (grepl("NRT",file)){
        date=substr(file,start=102,stop=109) %>% as.Date(.,format="%Y%m%d")
      }
  
  savename200m=glue("{outdir}/{date}/{var}200m.grd")
  # savename_sd=glue("{outdir}/{date}/{var}_sd.grd")
  print(date)
  
  if(!file.exists(savename200m)){
    print("oxy200m doesn't exist, writing out")
    if(date>="2017-01-01") {r=raster(file,varname="o2",lvar=1,level=26)}
    if(date<"2017-01-01") {r=raster(file,varname="o2",lvar=1,level=31)} 
    r2=shift(rotate(shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
    r_res=raster::resample(r2,template)
    # r_sd=focal(r_res,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)
    
    writeRaster(r_res,savename200m,overwrite=T)
  }
  
    },
  
  error = function(e){
    message(glue("Something went wrong"))
    print(e)
  }
  )
 
}

mclapply(Files,FUN=writeRasterTemp,outdir=dailyDir,mc.cores=4,var="oxy")
# lapply(Files,FUN=writeRasterTemp,outdir=dailyDir,var="oxy")
