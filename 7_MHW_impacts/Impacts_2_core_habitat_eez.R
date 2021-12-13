## Core habitat redistribution across EEZs

dropboxpath="/Users/EcoCast"
library(glue)
library(dismo)
library(SDMTools)
library(sf)
source(glue("{dropboxpath}/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R"))

modDir=glue("{dropboxpath}/Dropbox/OLE/models/species/brt")
outdir=glue("{dropboxpath}/Dropbox/OLE/plots/plots_09_23_21");dir.create(outdir)
predir="/Volumes/Triple_Bottom_Line/Heather_working/model_predictions/species/species_full_03_22_21"
statdir=glue("{dropboxpath}/Dropbox/OLE/stats/stats_09_23_21");dir.create(statdir)
bbox_outdir=glue("{dropboxpath}/Dropbox/OLE/bounding_boxes/species_03_22_21")
eez=st_read(glue("{dropboxpath}/Dropbox/OLE/spatial_data/World_EEZ_v11_20191118_HR_0_360/eez_v11_0_360.shp")) %>% 
  st_simplify(preserveTopology=TRUE, dTolerance = .2) 
eex2=eez %>% filter(SOVEREIGN1=="Mexico"|SOVEREIGN1=="United States"|SOVEREIGN1=="Canada")

date="03_22_21"
mod_species=list.files(modDir,pattern=date,full.names = T)  %>% grep("species",.,invert=F,value = T) %>% grep(paste("_cc_|_trans_|Albatross_TOPP|loggerhead"),.,value = T,invert=T)
names=mod_species%>% 
  gsub(modDir,"",.) %>% 
  gsub(glue("/species_bernoulli_{date}_step_lr0.01_tc3_bf0.6_tol1e-05_bernoulli_"),"",.) %>% 
  gsub(".rds","",.) %>% gsub("_restricted2","",.)

## thresholds
dat=read.csv(glue("/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_bkgd_envt_{date}.csv"))
dat2=dat %>% filter(presAbs==1) %>% mutate(random=1:nrow(.),day=yday(date))
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(8)
thresholds=foreach(i=1:length(names),.export = c("mod_species","names","dat2"),.combine=rbind,.packages = c("gbm","glue","tidyverse","raster"),.verbose=T) %dopar%{
  sp_dat=dat2 %>% filter(id==names[i])
  mod=readRDS(mod_species[i])
  pred_points=predict.gbm(mod,sp_dat,n.trees=mod$gbm.call$best.trees,type="response",na.rm=F) %>% 
    quantile(.,c(.75,.7,.6,.5,.4)) %>% t() %>% as.data.frame() %>% 
    mutate(species=names[i])
  
  colnames(pred_points)=c("25_threshold","30_threshold","40_threshold","50_threshold","60_threshold","species")
  rownames(pred_points)=i
  return(pred_points)
}

## frag stats
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(9)

whatThresh="50_threshold"

master2=foreach(i=1:length(names),.export = c("predir","names","thresholds","bbox_outdir","whatThresh","eez","eex2"),.packages = c("raster","glue","tidyverse"),.combine=rbind,.verbose=T) %dopar% {
  print(names[i])
  sp_bbox=st_read(glue("{bbox_outdir}/{names[i]}.shp"))
   thresh=thresholds %>% filter(species==names[i]) %>% pull(whatThresh)
  ras=list.files(predir,pattern = names[i],full.names = T) %>% grep(".grd",.,value=T) %>% grep(paste("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020"),.,value = T,invert=F) %>% 
    grep(paste("-08-|-09-|-10-"),.,value = T,invert=F)# %>% .[1:1]
  
  
  dates=list.files(predir,pattern = names[i],full.names = F)%>% grep(".grd",.,value=T) %>% gsub(glue("{names[i]}_"),"",.) %>% 
    gsub(".grd","",.)%>% grep(paste("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020"),.,value = T,invert=F) %>% 
    grep(paste("-08-|-09-|-10-"),.,value = T,invert=F)# %>% .[1:1]
  
  # names(rasthresh)=dates
  
  empty=list()
  for(ii in 1:length(dates)){
  ras_date=ras[ii] %>% raster() %>% 
      mask(.,sp_bbox)
  names(ras_date)=dates[ii]
  print(names(ras_date))
  
  ras_date[values(ras_date)>=thresh]=1
  ras_date[values(ras_date)<thresh]=0
  
  ras_date2=ras_date
  ras_date2[values(ras_date2)==0]=NA
  
  raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
  
  if(nrow(raspnt)>0){
  coordinates(raspnt)=~x+y
  raspnt2=st_as_sf(raspnt)
  st_crs(raspnt2)=st_crs(eex2)
  test=st_intersection(raspnt2,eex2)
  test2=test %>% as.data.frame() %>% group_by(GEONAME) %>% summarise(n=n())
  total=cellStats(ras_date2,sum)
  high_seas=total-nrow(test)
  
  master=test2 %>% mutate(date=dates[ii]) %>%mutate(species=names[i])
  master[nrow(master) + 1,] = list("Total",total,dates[ii],names[i])
  master[nrow(master) + 1,] = list("High seas",high_seas,dates[ii],names[i])

  
  empty[[length(empty)+1]]=master
  }
  }
  together=do.call("rbind",empty)
  return(together)
}

write.csv(master2,glue("{statdir}/eez_{whatThresh}.csv"))
