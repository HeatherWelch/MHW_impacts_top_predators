# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_Wind"
Files=list.files(fileDir,full.names = T,recursive = T) 

writeRasterTemp=function(file,outdir,var){
  tryCatch(
    expr ={
  if(grepl("_REP-",file)){
    date=substr(file,start=118,stop=125) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("NRT",file)){
    date=substr(file,start=118,stop=125) %>% as.Date(.,format="%Y%m%d")
  }
  
  savename=glue("{outdir}/{date}/{var}.grd")
  # savename_sd=glue("{outdir}/{date}/{var}_sd.grd")
  print(date)
  
  if(!file.exists(savename)){
  print("file doesn't exist, writing out")
    datelist=grep(paste0("/",gsub("-","",date)),Files,value=T)
    empty=list()
    for(d in datelist){
      r1=raster(d,var="eastward_wind")
      r1a=raster(d,var="northward_wind")
      r=sqrt(r1^2+r1a^2)
      r2=raster::shift(raster::rotate(raster::shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
      r_res=raster::resample(r2,template)
      empty[[length(empty)+1]]=r_res
    }
    
    r_final=stack(empty) %>% calc(.,fun=mean,na.rm=T)
    writeRaster(r_final,savename,overwrite=T)

  }
  
    },
  error = function(e){
    message(glue("Something went wrong"))
    print(e)
  }
  )
 
}

# mclapply(Files,FUN=writeRasterTemp,outdir=dailyDir,mc.cores=6,var="sst")
lapply(Files,FUN=writeRasterTemp,outdir=dailyDir,var="wind")
