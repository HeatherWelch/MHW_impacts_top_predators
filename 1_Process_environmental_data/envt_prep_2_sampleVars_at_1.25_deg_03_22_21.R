
# written by Heather Welch 02.23.21
## CHECK envt_prep_1_mld_02_23_21.R
# purpose of this script is to 1. delete all MLD files 2015 onwards, 
#2. edit code to uptake new netcdfs from Steph
# doing this because there is a new NRT product that fits historical better than what we used previously

# deleting old rasters ####
# dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE1.25"
# Files=list.files(dailyDir,full.names = T,recursive = T)
# F2=grep("mld",Files,value=T)
# 
#  toMatch=c("2015","2016","2017","2018","2019","2020")
# 
# F3=unique(grep(paste(toMatch,collapse = "|"),F2,value=T))
# file.remove(F3)


### code to get all environmental variables at 1.25 res
library(raster)
library(tidyverse)
library(glue)
library(foreach)
library(lubridate)
library(doParallel, quietly = TRUE)
outdir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE1.25";dir.create(outdir)
dirdatess=list.dirs("/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE",recursive = T,full.names = F) %>% .[3:length(.)]
 dirdates=lapply(dirdatess,function(x)gsub("daily/","",x)) %>% unlist() %>% .[7306:length(.)]
# dirdates=lapply(dirdatess,function(x)gsub("daily/","",x)) %>% unlist() %>% .[14611:length(.)]

## dynamic variables first, will do static variables and their sd later
registerDoParallel(4)
foreach(i=1:length(dirdates),.packages = c("tidyverse","glue","raster"),.verbose=T,.export = c("outdir")) %dopar% {
  print(dirdates[i])
  savedir=glue("{outdir}/{dirdates[i]}");dir.create(savedir)
  dir=glue("/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily/{dirdates[i]}")
  ras=list.files(dir,pattern=".grd",full.names = T) %>% grep("_sd",.,value = T,invert = T) %>% grep("bathy",.,value = T,invert = T) %>%
    grep("dist",.,value = T,invert = T) %>%  grep("oxycline",.,value = T,invert = T)
   for(ii in 1:length(ras)){
     print(ras[ii])
     rasname=gsub(glue("{dir}/"),"",ras[ii])
       if(!file.exists(glue("{savedir}/{rasname}"))){
  rasfile=raster(ras[ii])
  if(grepl("oxycline",ras[ii])){
    print("This is oxycline")
    rasfile[values(rasfile)<0]=NA
  }
   r_mean=focal(rasfile,w=matrix(1,nrow=5,ncol=5), fun=mean,na.rm=TRUE)
   writeRaster(r_mean,glue("{savedir}/{rasname}"),overwrite=T)
  print("This needs mean")
  toMatch <- c("adt", "sla", "sst")
  if(unique (grepl(paste(toMatch,collapse="|"), ras[ii]))){
    print("This need sd")
   r_sd=focal(rasfile,w=matrix(1,nrow=5,ncol=5), fun=sd,na.rm=TRUE)
   rasnameSD=gsub(".grd","",rasname) %>% glue("_sd.grd")
   writeRaster(r_sd,glue("{savedir}/{rasnameSD}"),overwrite=T)
  }
  }
   }
 }


# ## code to add daylight
# library(insol)
# datelist=c("2012-02-01", "2012-10-01", "2015-02-01", "2015-10-01")
# template=raster("/Users/heatherwelch/Dropbox/OLE/spatial_data/template.grd") %>% coordinates() %>% as.data.frame() 
# for(i in 1:length(datelist)){
# daylight=daylength(template$y,template$x,JD(as.Date(datelist[i])),-9) %>% as.data.frame()
# template2=raster("/Users/heatherwelch/Dropbox/OLE/spatial_data/template.grd")
# template2[]=daylight$daylen
# writeRaster(template2,glue("/Users/heatherwelch/Dropbox/OLE/data/environmental_rasters1.25/{datelist[i]}/daylight.grd"))
# }
# 
## code to add day of year
# dirdates=list.files("/Users/heatherwelch/Dropbox/OLE/data/environmental_rasters") %>% grep("-01",.,value = T)
# template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
# for(i in 1:length(dirdates)){
#   print(dirdates[i])
#   daylight=yday(dirdates[i])
#   template[]=daylight
#   writeRaster(template,glue("{outdir}/{dirdates[i]}/day.grd"),overwrite=T)
# }
