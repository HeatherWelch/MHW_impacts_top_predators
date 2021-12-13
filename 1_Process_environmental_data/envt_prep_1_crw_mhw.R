# written by Heather Welch 07.01.20

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)
to360 <- function(x) {x %% 360}

source("/Users/EcoCast/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R")
library(pals)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/crw_mhw_pacific"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CRW_MHW"
Files=list.files(fileDir,full.names = T,recursive = T) 
e <- as(extent(to360(-180), to360(-100), 10, 62), 'SpatialPolygons')

writeRasterCRW_MHW=function(file,outdir){
  
  date=substr(file,start=85,stop=92) %>% as.Date(.,format="%Y%m%d")
  savename=glue("{outdir}/crw_mhw_{date}.grd")
  print(date)
  
  if(!file.exists(savename)){
  print("crw mhw doesn't exist, writing out")
    
  #sla
  r=raster(file)
  r_pnts=rasterToPoints(r) %>% as.data.frame()
  r_pnts_fixed=r_pnts %>% mutate(x=x/20,y=y/20) %>% mutate(y=y-90,x=x-180)
  
  r_raster=rasterFromXYZ(r_pnts_fixed[,c(1,2,3)])
  r_raster_shift=raster::shift(raster::rotate(raster::shift(r_raster,dx=180)),dx=180)
  # ee=mask(cc,e) %>% crop(.,e)
  r_res=raster::resample(r_raster_shift,template)%>% crop(.,e) 
  writeRaster(r_res,savename,overwrite=T)
  
  df_maphigh=rasterToPoints(r_res)%>% as.data.frame() 
  colnames(df_maphigh)=c("rows","cols","value")
  df_maphigh=df_maphigh %>% mutate(value=round(value))
  
  cls=pals::parula(5)
  
  plot_sp=ggplot()+
    geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=as.character(value)))+
    scale_fill_manual("MHW category",values=c("5"="#F9FB0E","4"="#D1BA58","3"="#33B7A0","2"="#1283D4","1"="#352A87","0"="lightgrey","-1"="darkgrey"),
                      labels=c("-1"="Sea ice","0"="No MHW","1"="1: Moderate","2"="2: Strong","3"="3: Severe","4"="4: Extreme","5"="5: Beyond extreme"),
                      na.value="black")+
    geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    theme_classic()+xlab(NULL)+ylab(NULL)+
    coord_sf(xlim = c(180, 260), ylim = c(10,62),expand=F)+
    ggtitle(glue("Marine Heatwaves {date}"))
  
  png(glue("{outdir}/crw_mhw_{date}.png"),width=32,height=22,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print(plot(plot_sp))
  # gg_hm
  dev.off()
  
  }
}

mclapply(Files,FUN=writeRasterCRW_MHW,outdir=dailyDir,mc.cores=2)
