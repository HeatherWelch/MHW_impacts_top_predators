#Predicting models

source("/Users/EcoCast/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R")
library(pals)
library(sf)
to360 <- function(x) {x %% 360}

modDir="/Users/EcoCast/Dropbox/OLE/models/species/brt"
boundingboxdir="/Users/EcoCast/Dropbox/OLE/bounding_boxes"
date="03_22_21"
dat=read.csv(glue("/Users/EcoCast/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_bkgd_envt_{date}.csv"))
rasterdir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE1.25"
datelist=list.files(rasterdir) %>% grep(paste("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010"),.,value=T) %>% 
  grep(paste("-08-|-09-|-10-"),.,value=T)
outdir="/Volumes/Triple_Bottom_Line/Heather_working/model_predictions/species/species_full_03_22_21";dir.create(outdir)
pacific=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/full_pacific.grd")
e <- as(extent(to360(-180), to360(-100), 10, 62), 'SpatialPolygons')
pacific=crop(pacific,e)
template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
cclme=st_read("/Users/EcoCast/Dropbox/OLE/spatial_data/lme/lme.shp") 


# grab restricted2 models ####
mod_species=list.files(modDir,pattern=date,full.names = T)  %>% grep("species",.,invert=F,value = T) %>% grep(paste("_cc_|_trans_|Albatross_TOPP|loggerheadTurtle"),.,value = T,invert=T)
names_sp=mod_species%>% 
  gsub(modDir,"",.) %>% 
  gsub(glue("/species_bernoulli_{date}_step_lr0.01_tc3_bf0.6_tol1e-05_bernoulli_"),"",.) %>% 
  gsub(".rds","",.) %>% gsub("_restricted2","",.)

# new: removing non-dallas albatross and cc + trans tunas ####

## calculating species thresholds
dat2=dat %>% filter(presAbs==1) %>% mutate(random=1:nrow(.),day=yday(date))
library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(8)
thresh=foreach(i=1:length(names_sp),.export = c("mod_species","names_sp","rasterdir","dat2"),.combine=rbind,.packages = c("gbm","glue","tidyverse","raster"),.verbose=T) %dopar%{
  sp_dat=dat2 %>% filter(id==names_sp[i])
  mod=readRDS(mod_species[i])
  pred_points=predict.gbm(mod,sp_dat,n.trees=mod$gbm.call$best.trees,type="response",na.rm=F) %>% 
   quantile(.,c(.75,.7,.6,.5,.4)) %>% t() %>% as.data.frame() %>% 
    mutate(species=names_sp[i])
  colnames(pred_points)=c("25_threshold","30_threshold","40_threshold","50_threshold","60_threshold","species")
  rownames(pred_points)=i
  return(pred_points)
}

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(9)
system.time(print(
  foreach(i=1:length(datelist),.export = c("thresh","mod_species","names_sp","rasterdir","pacific","template","boundingboxdir","dat","cclme"),.combine=rbind,.packages = c("gbm","glue","tidyverse","raster","gridExtra"),.verbose=T) %dopar% {
    print(datelist[i])
     if(!file.exists(glue("{outdir}/yellowfinTuna_TOPP_{datelist[i]}.grd"))){
      tryCatch(
        expr ={
          mntt=datelist[i] %>% month() %>% str_pad(.,2,side="left",pad="0")
          vars=readRDS(mod_species[1])$var.name 
          vars2=lapply(vars,function(x)paste0(x,".grd")) %>% unlist()
          co_stack=list.files(glue("{rasterdir}/{datelist[i]}"),pattern=".grd",full.names=T) 
          
          names=list.files(glue("{rasterdir}/{datelist[i]}"),pattern=".grd",full.names=T)  %>% gsub(glue("{rasterdir}/{datelist[i]}/"),"",.) %>% gsub(".grd","",.)
          
          ## adding this in as random
          co_stack=stack(co_stack,template)  #%>% crop(.,pacific)
          b=list(names,"random" ) %>% unlist() %>% unique()
          names(co_stack)=b
          
          if("oxycline" %in% b){
            pos=grep("oxycline",b)
            co_stack[[pos]][values(co_stack[[pos]])<(0)]=NA
          }
          
          for(ii in 1:length(mod_species)){
            sp_d=dat %>% filter(id==names_sp[ii]) %>% mutate(mnt=month(date)) %>% mutate(mnt=str_pad(mnt,2,side="left",pad="0")) %>%
              mutate(random=sample(1:nrow(.))) %>% mutate(day=yday(date))%>% 
              filter(presAbs==1)
            
            sp_dat=sp_d %>%  
              filter(mnt==mntt) %>% 
              filter(presAbs==1)
            
            # spatial predictions
            boxx=st_read(glue("{boundingboxdir}/species_03_22_21/{names_sp[ii]}.shp")) #%>% st_as_sf()
            mod=readRDS(mod_species[ii])
            co_stack2=co_stack %>% mask(.,boxx)
            pred=predict(co_stack2,mod,n.trees=mod$gbm.call$best.trees,type="response",na.rm=F)
            pred2=mask(pred,boxx)
            if(grepl("trans",mod_species[ii])){
              ras2=rotate(pred2)
              ras_e=mask(ras2,cclme,inverse=T)
              pred2=raster::shift(raster::rotate(raster::shift(ras_e,dx=180)),dx=180)
              }
            writeRaster(pred2,glue("{outdir}/{names_sp[ii]}_{datelist[i]}"),overwrite=T)
           
            thresh2=thresh %>% filter(species==names_sp[ii])

            thresh25=thresh2$`25_threshold`
            thresh30=thresh2$`30_threshold`
            thresh40=thresh2$`40_threshold`
            thresh50=thresh2$`50_threshold`
            thresh60=thresh2$`60_threshold`
            
            # continuous raster
            df_maphigh=rasterToPoints(pred2)%>% as.data.frame()
            colnames(df_maphigh)=c("rows","cols","value")
            
            plot_sp=ggplot()+
              geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=value))+
              geom_point(data=sp_dat,aes(x=lon,y=lat),color="red",size=.3)+
              scale_fill_gradientn(colours = pals::parula(100),na.value="black")+
              geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
              # geom_polygon(data = fortify(boxx,plot=FALSE,fill=TRUE), aes(x=long, y = lat, group=group),color="red",fill=NA)+
              # geom_polygon(data=fortify(boxx,plot=F,fill=F), aes(x=long, y = lat, group=group),color="black",fill=NA,size=1)+
              geom_sf(data=boxx,color="grey",fill=NA)+
              theme_classic()+xlab(NULL)+ylab(NULL)+
              coord_sf(xlim = c(180, 260), ylim = c(10,62),expand=F)+
              ggtitle(glue("{names_sp[ii]}_{datelist[i]} continuous"))
            
            # reclassified raster
            pred3=pred2
            pred3[values(pred3)>=thresh25]=25
            pred3[values(pred3)<thresh25&values(pred3)>=thresh30]=30
            pred3[values(pred3)<thresh30&values(pred3)>=thresh40]=40
            pred3[values(pred3)<thresh40&values(pred3)>=thresh50]=50
            pred3[values(pred3)<thresh50&values(pred3)>=thresh60]=60
            pred3[values(pred3)<thresh60]=0
            
            df_maphigh=rasterToPoints(pred3)%>% as.data.frame()
            colnames(df_maphigh)=c("rows","cols","value")
            
            plot_sp_b=ggplot()+
              geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=as.factor(value)))+
              geom_point(data=sp_dat,aes(x=lon,y=lat),color="black",size=.3,alpha=.7)+
              scale_fill_manual("threshold",values=c("0"="lightgrey","25"="mediumvioletred","30"="seagreen","40"="seagreen1","50"="lightsalmon","60"="moccasin"))+
              geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
              # geom_polygon(data = fortify(boxx,plot=FALSE,fill=TRUE), aes(x=long, y = lat, group=group),color="red",fill=NA)+
              # geom_polygon(data=fortify(boxx,plot=F,fill=F), aes(x=long, y = lat, group=group),color="black",fill=NA,size=1)+
              geom_sf(data=boxx,color="grey",fill=NA)+
              theme_classic()+xlab(NULL)+ylab(NULL)+
              coord_sf(xlim = c(180, 260), ylim = c(10,62),expand=F)+
              ggtitle(glue("{names_sp[ii]}_{datelist[i]} reclassified"))
            
            
            # plot_sp
            
            png(glue("{outdir}/{names_sp[ii]}_{datelist[i]}.png"),width=64,height=22,units='cm',res=400,type = "cairo")
            par(ps=10)
            par(mar=c(4,4,1,1))
            par(cex=1)
            print({grid.arrange(plot_sp,plot_sp_b,ncol=2)})
            # gg_hm
            dev.off()
            
          }
        },
        error = function(e){
          message(glue("Model not working"))
          print(e)
        }
      )
    }
   }
))


