source("/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects_new/utilities/BRTs_ModelEvaluation_fcns.R")
source("/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R")

date="03_22_21"
modDir="/Users/heatherwelch/Dropbox/OLE/models/species/brt"
outdir=glue("/Users/heatherwelch/Dropbox/OLE/plots/plots_{date}")
dat=read.csv(glue("/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_bkgd_envt_{date}.csv")) %>% 
  mutate(day=yday(date))

mod_species=list.files(modDir,pattern=date,full.names = T)  %>% grep("species",.,invert=F,value = T) %>%
  grep("restricted",.,invert=T,value = T)%>% grep(paste("_cc_|_trans_|Albatross_TOPP|loggerheadTurtle"),.,value = T,invert=T)
names=mod_species%>% 
  gsub(modDir,"",.) %>% 
  gsub(glue("/species_bernoulli_{date}_step_lr0.01_tc3_bf0.6_tol1e-05_bernoulli_"),"",.) %>% 
  gsub(".rds","",.) 

# full validation ####
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)

a<-foreach(i=1:length(names),.export = c("mod_species","names","dev_eval3","eval_full_heather_fixed","ratio","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse"),.verbose=T) %dopar% {
  print(names[i])
  full=readRDS(mod_species[i]) %>% dev_eval3()
  
  mod=readRDS(mod_species[i])
  
  main=dat%>%filter(id==names[i]) %>% 
    dplyr::select(c(species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
  
  main=main %>% mutate(random=sample(1:nrow(main)))
  main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
  
  # 75/25
  
  gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
  family="bernoulli"
  if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
  if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
  tc=mod$interaction.depth
  bf=0.6
  tolerance = .00001
  a75_25=eval_full_heather_fixed(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs")
  
  rat=ratio(main,mod,response = "presAbs")
  # "Deviance","AUC","TSS","True_positive_rate","True_negative_rate"
  
  dat2=data.frame(species=as.character(names[i]),
                  deviance=as.numeric(full),
                  ratio=as.numeric(rat),
                  deviance7525=as.numeric(a75_25$Deviance),
                  AUC7525=as.numeric(a75_25$AUC),
                  TSS7525=as.numeric(a75_25$TSS),
                  TNR=as.numeric(a75_25$TNR),
                  TPR=as.numeric(a75_25$TPR),
                  AveragePredAbs7525=as.numeric(a75_25$AveragePredAbs),
                  AveragePredPres7525=as.numeric(a75_25$AveragePredPres),
                  stringsAsFactors = F)
  
}


write.csv(a,glue("{outdir}/model_performance_eval_full.csv"))

# 75:25 cross validation * 50 ####
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(7)

a<-foreach(i=1:length(names),.export = c("mod_species","names","dev_eval3","eval_7525_heather_fixed_50","ratio","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse"),.verbose=T) %dopar% {
  print(names[i])
  full=readRDS(mod_species[i]) %>% dev_eval3()
  
  mod=readRDS(mod_species[i])
  
  main=dat%>%filter(id==names[i]) %>% 
    dplyr::select(c(species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
  
  main=main %>% mutate(random=sample(1:nrow(main)))
  main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
  
  # 75/25
  
  gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
  family="bernoulli"
  if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
  if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
  tc=mod$interaction.depth
  bf=0.6
  tolerance = .00001
  a75_25=eval_7525_heather_fixed_50(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs")
  
  rat=ratio(main,mod,response = "presAbs")
  # "Deviance","AUC","TSS","True_positive_rate","True_negative_rate"
  
  dat2=data.frame(species=as.character(names[i]),
                  deviance=as.numeric(full),
                  ratio=as.numeric(rat),
                  deviance7525=as.numeric(a75_25$Deviance),
                  AUC7525=as.numeric(a75_25$AUC),
                  TSS7525=as.numeric(a75_25$TSS),
                  TNR=as.numeric(a75_25$TNR),
                  TPR=as.numeric(a75_25$TPR),
                  AveragePredAbs7525=as.numeric(a75_25$AveragePredAbs),
                  AveragePredPres7525=as.numeric(a75_25$AveragePredPres),
                  stringsAsFactors = F)
  
}


write.csv(a,glue("{outdir}/model_performance_eval_7525_50.csv"))

# LOO cross validation ####
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)

  LOO<-foreach(ii=1:length(names),.export = c("mod_species","names","dev_eval3","LOO_eval_heather","ratio","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse"),.verbose=T) %dopar% {
    print(names[ii])
    mod=readRDS(mod_species[ii])
    
    main=dat%>%filter(id==names[ii]) %>% 
      dplyr::select(c(date,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
    
    main=main %>% mutate(random=sample(1:nrow(main)))
    main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
    
    # LOO
    gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
    family="bernoulli"
    if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
    if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
    tc=mod$interaction.depth
    bf=0.6
    tolerance = .00001
    aLOO=LOO_eval_heather(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs")
    aLOO=aLOO %>% mutate(species=names[ii])
    }

write.csv(LOO,glue("{outdir}/model_performance_eval_LOO.csv"))

# SOO cross validation ####
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)
system.time(print(
  SOO<-foreach(ii=1:length(names),.export = c("mod_species","names","dev_eval3","SOO_eval_heather","ratio","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse"),.verbose=T) %dopar% {
    print(names[ii])
    mod=readRDS(mod_species[ii])
    
    main=dat%>%filter(id==names[ii]) %>% 
      dplyr::select(c(date,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
    
    main=main %>% mutate(random=sample(1:nrow(main)))
    main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
    
    # LOO
    gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
    family="bernoulli"
    if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
    if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
    tc=mod$interaction.depth
    bf=0.6
    tolerance = .00001
    aSOO=SOO_eval_heather(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs")
    aSSO=aSOO %>% mutate(species=names[ii])
  }
))

write.csv(SOO,glue("{outdir}/model_performance_eval_SOO.csv"))

# leave space out cross validation ####
#lme from here: https://www.sciencebase.gov/catalog/item/55c77722e4b08400b1fd8244
#longhurst from here: https://hub.arcgis.com/datasets/34f1a9c0e4b74b2887e6b23c584e1f2d_0

lme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
  filter(LME_NAME=="East Bering Sea"|LME_NAME=="Gulf of Alaska"|LME_NAME=="California Current"|LME_NAME=="Aleutian Islands"|LME_NAME=="Gulf of California"|LME_NAME=="Insular Pacific-Hawaiian")
longh=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/Longhurst_Biogeographical_Provinces-shp/Longhurst_Biogeographical_Provinces.shp") %>% 
  filter(ProvCode=="NPPF"|ProvCode=="PSAE"|ProvCode=="NPTG")

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)

  LSO<-foreach(ii=1:length(names),.export = c("mod_species","names","lme","LSO_eval_heather","longh","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse","sf","sp"),.verbose=T) %dopar% {
    # 1:length(names)
    print(names[ii])
    mod=readRDS(mod_species[ii])
    
    main=dat%>%filter(id==names[ii]) %>% 
      dplyr::select(c(date,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
    
    main=main %>% mutate(random=sample(1:nrow(main)))
    main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
    
    # LOO
    gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
    family="bernoulli"
    if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
    if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
    tc=mod$interaction.depth
    bf=0.6
    tolerance = .00001
    aLSO=LSO_eval_heather(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs",lme=lme,longh=longh)
    aLSO=aLSO %>% mutate(species=names[ii])
    }


write.csv(LSO,glue("{outdir}/model_performance_eval_LSO.csv"))


# Subset by space  cross validation ####
#lme from here: https://www.sciencebase.gov/catalog/item/55c77722e4b08400b1fd8244
#longhurst from here: https://hub.arcgis.com/datasets/34f1a9c0e4b74b2887e6b23c584e1f2d_0

lme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
  filter(LME_NAME=="East Bering Sea"|LME_NAME=="Gulf of Alaska"|LME_NAME=="California Current"|LME_NAME=="Aleutian Islands"|LME_NAME=="Gulf of California"|LME_NAME=="Insular Pacific-Hawaiian")
longh=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/Longhurst_Biogeographical_Provinces-shp/Longhurst_Biogeographical_Provinces.shp") %>% 
  filter(ProvCode=="NPPF"|ProvCode=="PSAE"|ProvCode=="NPTG")

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)
  SBS<-foreach(ii=1:length(names),.export = c("mod_species","names","lme","SBS_eval_heather","longh","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse","sf","sp"),.verbose=T) %dopar% {
    # 1:length(names)
    print(names[ii])
    mod=readRDS(mod_species[ii])
    
    main=dat%>%filter(id==names[ii]) %>% 
      dplyr::select(c(date,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
    
    main=main %>% mutate(random=sample(1:nrow(main)))
    main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
    
    # LOO
    gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
    family="bernoulli"
    if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
    if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
    tc=mod$interaction.depth
    bf=0.6
    tolerance = .00001
    aSBS=SBS_eval_heather(main,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs",lme=lme,longh=longh)
    aSBS=aSBS %>% mutate(species=names[ii])
  }


write.csv(SBS,glue("{outdir}/model_performance_eval_SBS.csv"))

# novel data cross validation by year, MHW months only ####
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(6)

absences=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_validation_absences_bkgd_envt_03_22_21.csv") %>% 
  mutate(presAbs=0)

test=absences %>% group_by(id) %>% summarise(n=n())
presences=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_validation_bkgd_envt_03_22_21.csv")%>% 
  mutate(presAbs=1) %>% filter(as.numeric(number)>0)

full_validation=rbind(absences,presences) %>% mutate(domain=gsub("_full","",domain)) %>% mutate(day=yday(dt)) %>% 
mutate(domain=replace(domain,domain=="TOPP_cc","cc_TOPP")) %>% mutate(domain=replace(domain,domain=="TOPP_trans","trans_TOPP")) %>% mutate(species_domain=glue("{species}_{domain}"))

a<-foreach(i=1:length(names),.export = c("mod_species","names","full_validation","SOO_eval_validation_heather","ratio","date","dat"),.combine=rbind,.packages = c("gbm","glue","tidyverse"),.verbose=T) %dopar% {
  print(names[i])
  
  mod=readRDS(mod_species[i])
  
  main=full_validation%>%filter(species_domain==names[i]) %>% 
    dplyr::select(c(dt,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst,day))
   if(nrow(main %>% filter(presAbs==0))>0){
    main=main %>% mutate(random=sample(1:nrow(main)))
    main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]

    gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
    family="bernoulli"
    if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
    if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
    tc=mod$interaction.depth
    bf=0.6
    tolerance = .00001
    a75_25=SOO_eval_validation_heather(main,mod=mod,family=family,gbm.y="presAbs",lr=lr,tc=tc,tolerance.method="fixed",tolerance=tolerance,gbm.x=gbm.x,response = "presAbs")
   
    if(!is.null(a75_25)){
    dat2=a75_25 %>% mutate(species=names[i])
    return(dat2)
    }
}
}
aa=a

write.csv(a,glue("{outdir}/model_performance_eval_validation__year.csv"))





