# written by Heather Welch 07.01.20

library(raster) 
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_Chla_4km_daily"
Files=list.files(fileDir,full.names = T,recursive = T)
writeRasterChla=function(file,outdir,var){
  tryCatch(
    expr ={
      
      if(grepl("Historical",file)){
        
        date=substr(file,start=119,stop=126) %>% as.Date(.,format="%Y%m%d")
        
      } else if (grepl("NRT",file)){
        date=substr(file,start=170,stop=177) %>% as.Date(.,format="%Y%m%d")
        
      }
      savename=glue("{outdir}/{date}/{var}.grd")
      # savename_sd=glue("{outdir}/{date}/{var}_sd.grd")
      print(date)
      
      if(!file.exists(savename)){
       
        r=raster(file,varname="CHL") 
        r2=shift(rotate(shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
        r_res=raster::resample(r2,template)
        r_res_log=log10(r_res+0.001)
        
        # r_sd=focal(r_res,w=matrix(1,nrow=3,ncol=3), fun=sd,na.rm=TRUE)
        writeRaster(r_res_log,savename,overwrite=T)

        # writeRaster(r_sd,savename_sd,overwrite=T)
        
      }
      
    },
    
    error = function(e){

      message(glue("Something went wrong"))

      print(e)

    }
  )

}


# mclapply(Files,FUN=writeRasterChla,outdir=dailyDir,mc.cores=3,var="l.chl")

lapply(Files,FUN=writeRasterChla,outdir=dailyDir,var="l.chl")