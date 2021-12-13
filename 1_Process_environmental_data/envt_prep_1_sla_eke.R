# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_SSH_uv"
Files=list.files(fileDir,full.names = T,recursive = T) 

writeRasterSLA_EKE=function(file,outdir){
  if(grepl("dt_global",file) & grepl("/2019/",file)){
    date=substr(file,start=90,stop=97) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("dt_global",file)){
    date=substr(file,start=87,stop=94) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("nrt_global",file)){
    date=substr(file,start=103,stop=110) %>% as.Date(.,format="%Y%m%d")
  }
  
  savename=glue("{outdir}/{date}/sla.grd")
  savename_sd=glue("{outdir}/{date}/sla_sd.grd")
  savename_eke=glue("{outdir}/{date}/eke.grd")
  savename_adt=glue("{outdir}/{date}/adt.grd")
  savename_adt_sd=glue("{outdir}/{date}/adt_sd.grd")
  print(date)
  
  if(!file.exists(savename)){
  print("eke/adt/sla doesn't exist, writing out")
    
  #sla
  r=raster(file,varname="sla")
  if(extent(r)[1]<0){
  r2=shift(rotate(shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
  } else {r2=r}
  r_res=raster::resample(r2,template)
  r_sd=focal(r_res,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)
  writeRaster(r_res,savename,overwrite=T)
  writeRaster(r_sd,savename_sd,overwrite=T)
  
  #adt
  rADT=raster(file,varname="adt")
  if(extent(rADT)[1]<0){
    r2ADT=shift(rotate(shift(rADT,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
  } else {r2ADT=rADT}
  # r2ADT=shift(rotate(shift(rADT,x=180)),x=180) # convert from kelvin to celsius and make 0-360
  r_resADT=raster::resample(r2ADT,template)
  r_sdADT=focal(r_resADT,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)
  writeRaster(r_resADT,savename_adt,overwrite=T)
  writeRaster(r_sdADT,savename_adt_sd,overwrite=T)
  
  #eke
  ugosa=raster(file,varname="ugosa") 
  vgosa=raster(file,varname="vgosa")  
  eke=1/2*(ugosa^2+vgosa^2)
  if(extent(eke)[1]<0){
    eke2=shift(rotate(shift(eke,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
  } else {eke2=eke}
  # eke2=shift(rotate(shift(eke,x=180)),x=180)
  eke_res=raster::resample(eke2,template)
  l.eke=log10(eke_res + .001)
  writeRaster(l.eke,savename_eke,overwrite=T)
  }
 
}

mclapply(Files,FUN=writeRasterSLA_EKE,outdir=dailyDir,mc.cores=2)
