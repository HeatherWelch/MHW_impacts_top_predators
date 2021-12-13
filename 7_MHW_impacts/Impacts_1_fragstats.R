## reclassifying rasters +fragstats
dropboxpath="/Users/EcoCast"
library(glue)
library(dismo)
library(SDMTools)
library(sf)
source(glue("{dropboxpath}/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R"))

modDir=glue("{dropboxpath}/Dropbox/OLE/models/species/brt")
outdir=glue("{dropboxpath}/Dropbox/OLE/plots/plots_03_22_21")
predir="/Volumes/Triple_Bottom_Line/Heather_working/model_predictions/species/species_full_03_22_21"
statdir=glue("{dropboxpath}/Dropbox/OLE/stats/stats_03_22_21");dir.create(statdir)
bbox_outdir=glue("{dropboxpath}/Dropbox/OLE/bounding_boxes/species_03_22_21")

date="03_22_21"
mod_species=list.files(modDir,pattern=date,full.names = T)  %>% grep("species",.,invert=F,value = T) %>% grep(paste("_cc_|_trans_|Albatross_TOPP"),.,value = T,invert=T)
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
registerDoParallel(6)

whatThresh="50_threshold"

master2=foreach(i=1:length(names),.export = c("predir","names","thresholds","bbox_outdir","whatThresh"),.packages = c("raster","glue","tidyverse"),.combine=rbind,.verbose=T) %dopar% {
  print(names[i])
   thresh=thresholds %>% filter(species==names[i]) %>% pull(whatThresh)
  ras=list.files(predir,pattern = names[i],full.names = T) %>% grep(".grd",.,value=T)# %>% .[1:5]
  
  dates=list.files(predir,pattern = names[i],full.names = F)%>% grep(".grd",.,value=T) %>% gsub(glue("{names[i]}_"),"",.) %>% 
    gsub(".grd","",.) #%>% .[1:5]
  
  empty=list()
  for(ii in 1:length(dates)){
  ras_date=ras[ii] %>% raster() %>% 
      mask(.,st_read(glue("{bbox_outdir}/{names[i]}.shp")))
  names(ras_date)=dates[ii]
  print(names(ras_date))
  
  ras_date[values(ras_date)>=thresh]=1
  ras_date[values(ras_date)<thresh]=0
  
  ras_date2=ras_date
  ras_date2[values(ras_date2)==0]=NA
  
  raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
  if(nrow(raspnt)>0){
  datt=data.frame(maxX=max(raspnt$x),
                  minX=min(raspnt$x),
                  maxY=max(raspnt$y),
                  minY=min(raspnt$y))
  
  dattt=data.frame(X25=quantile(raspnt$x)[2],
                   X75=quantile(raspnt$x)[4],
                   Y25=quantile(raspnt$y)[2],
                   Y75=quantile(raspnt$y)[4])
  rownames(dattt)=ii

  sat=ClassStat(ras_date,cellsize = 27750,bkgd=NA,latlon = TRUE) %>% filter(class==1)
  grav=COGravity(ras_date2) %>% as.data.frame() %>% t() %>% as.data.frame()
  stat2=cbind(sat,grav) %>% mutate(date=dates[ii],species=names[i],thresh=whatThresh)
  stat3=cbind(stat2,datt)
  stat4=cbind(stat3,dattt)
  empty[[length(empty)+1]]=stat4
  }
  }
  together=do.call("rbind",empty)
  return(together)
}


write.csv(master2,glue("{statdir}/fragstats_{whatThresh}.csv"))
      