## creating psuedo_absences for validation data
source("utilities/load_libraries.R")
library(sf)
require("rgdal")
require("rgeos")
require("dplyr")
library(grid)
to360 <- function(x) {x %% 360}
library(ggExtra)
library(gridExtra)
library(maptools)

# read in data  ####
# https://www.marineregions.org/gazetteer.php?p=details&id=1908
dat=read.csv("/Users/heatherwelch/Dropbox/OLE/data/cleaned_validation_data/ebird_npdb_cleaned_09_23_21.csv") %>% 
  mutate(long360=lon)  %>% rename(idd=id) %>% mutate(id=glue("{idd}"))
sp=unique(dat$id) %>% grep("leatherbackTurtle_dgn_observer",.,value = T,invert=T)
## just changing to see what happened with the cc and trans tuna domains
# sp=grep(unique(paste("tuna|Tuna")),sp,value=T)
outdir="/Users/heatherwelch/Dropbox/OLE/plots/plots_09_23_21/validationData";dir.create(outdir)

template=raster("/Users/heatherwelch/Dropbox/OLE/spatial_data/template.grd")
res(template)=1.25
string=seq(1:ncell(template))
template[]=string

data=maps::map("world2",fill=T)
IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
wrld_simpl <- map2SpatialPolygons(data, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
wrld=SpatialPolygons(wrld_simpl@polygons,proj4string=wrld_simpl@proj4string) %>% 
  gBuffer(., byid=TRUE, width=0)


goclme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
  filter(LME_NAME=="Gulf of California"|LME_NAME=="California Current") %>% st_union(.) %>% st_shift_longitude(.) %>% as_Spatial()
crs(goclme)=crs(wrld)

xcord=c(235,255,255,235)
ycord=c(45,45,30,30)
xym <- cbind(xcord, ycord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps,add=T)
crs(sps)=crs(wrld)

xcord=c(240,255,255,240)
ycord=c(31,31,23,23)
xym <- cbind(xcord, ycord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps2 = SpatialPolygons(list(ps))
plot(sps2,add=T)
crs(sps2)=crs(wrld)

# create minimum bounding polygon for each species and pseudo-absences####

empty=list()
for(i in 1:length(sp)){
  print(sp[i])
test_full=dat %>% filter(id==sp[i])%>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,.[,c(4,5)])) %>%
  mutate(unique2=glue("{date}{unique}"))#%>%
  # distinct(.,date2,unique,presAbs,.keep_all = T)
test=dat %>% filter(id==sp[i]) %>% dplyr::select(c(long360,lat))
ch <- chull(x=test$long360,y=test$lat)
coords <- test[c(ch, ch[1]), ]
plot(test, pch=19)
lines(coords, col="red")

sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
crs(sp_poly)=crs(wrld)
# test2=crop(sp_poly,spydf_pac)

test2=erase(sp_poly,wrld) 
if(grepl("_trans",sp[i])){
  test2= erase(test2,goclme)
  test2= erase(test2,sps)
  test2= erase(test2,sps2)
}

abs=spsample(test2, nrow(test)*3, type="random") %>% as.data.frame() 
absence=test_full %>% rbind(test_full)%>% rbind(test_full) %>% mutate(long360=to360(abs$x),lat=abs$y)%>% mutate(unique=raster::extract(template,.[,c(4,5)])) %>% 
  mutate(unique2=glue("{date}{unique}")) %>% 
  filter(!(unique2 %in%test_full$unique2)) %>% .[sample(nrow(.),nrow(test_full)),] %>% mutate(presAbs=0)

trial=rbind(test_full,absence) %>% dplyr::select(-lon) %>% mutate(presAbs=as.character(presAbs)) %>% arrange(presAbs)

minx =to360(-180)
maxx = to360(-100)
miny = to360(10)
maxy = to360(62)

# plot_sp=ggplot()+
#   geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
#   geom_point(data=trial,aes(x=long360,y=lat,color=as.factor(presAbs),group=presAbs),size = .5, shape = 21)+
#   scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
#   coord_cartesian(xlim = c(minx, maxx), ylim = c(miny,maxy))+
#   ggtitle(glue("{sp[i]} npre={nrow(dat)} (1:1 ratio)"))

# test2_sf=st_as_sf(test2)
# st_write(test2_sf,glue("/Users/heatherwelch/Dropbox/OLE/bounding_boxes/species_03_22_21/{sp[i]}.shp"),layer_options = "OVERWRITE=true")

x <- fortify(maps::map("world2",
                       plot=FALSE,fill=TRUE))

plot_sp2=ggplot(trial)+
  geom_point(aes(x = long360, y = lat,color=as.factor(presAbs),group=presAbs), 
             alpha = 0.2) +
  scale_colour_manual("",values = c("red", "blue")) +
  geom_map(data=x,map=x,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  geom_polygon(data=fortify(test2,plot=F,fill=F), aes(x=long, y = lat, group=group),color="black",fill=NA,size=1)+
  xlim(minx,maxx)+ylim(miny,maxy)+
  xlab("longitude")+ylab("latitude")+
  ggtitle(glue("{sp[i]} npre={nrow(trial)/2} (1:1 ratio)")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.9),
        legend.background = element_blank(),
        legend.key=element_blank())

plot1_full <-  ggMarginal(
  plot_sp2, 
  x = "long360", y = "lat",
  groupColour = TRUE, groupFill = F,
  type = "density",
  alpha = .3)
  

png(glue("{outdir}/{sp[i]}_presAbs.png"),width=22,height=22,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
grid.arrange(plot1_full,ncol=1)
dev.off()

empty[[length(empty)+1]]=trial

}

master=do.call("rbind",empty)
write.csv(master,"/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_validation_09_23_21.csv")

test=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_validation_09_23_21.csv") %>% 
  mutate(year=year(date)) %>% group_by(species,year) %>% summarise(n=n())


## THIS IS NOW JUNK I THINK
## adding in new tuna domains from 01 08 21
dat=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkgd_envtData/sp_dat_bkgd_envt_01_08_21.csv")
sp_cc=unique(dat$species) %>% grep("_cc",.,value=T)
sp_trans=unique(dat$species) %>% grep("_trans",.,value=T)
sp=list(sp_cc,sp_trans) %>% unlist()

for(i in 1:length(sp)){
  print(sp[i])
  # test_full=dat %>% filter(species==sp[i])%>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,.[,c(11,6)])) %>% 
  #   mutate(unique2=glue("{date2}{unique}"))#%>% 
  # distinct(.,date2,unique,presAbs,.keep_all = T)
  test=dat %>% filter(species==sp[i]) %>% dplyr::select(c(long360,lat))
  ch <- chull(x=test$long360,y=test$lat)
  coords <- test[c(ch, ch[1]), ]
  plot(test, pch=19)
  lines(coords, col="red")
  
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  crs(sp_poly)=crs(wrld)
  # test2=crop(sp_poly,spydf_pac)
  test2=erase(sp_poly,wrld)
  test2_sf=st_as_sf(test2)
  st_write(test2_sf,glue("/Users/heatherwelch/Dropbox/OLE/bounding_boxes/species_12_03_20/{sp[i]}.shp"),layer_options = "OVERWRITE=true")
}

### maybe extra stuff? #####

combind=test_full %>% mutate(long360A=to360(abs$x),latA=abs$y) %>% mutate(dif_long=abs(long360-long360A)) %>% 
  mutate(dif_lat=abs(lat-latA)) %>% filter((dif_lat<.5&dif_long<.5))


%>%  mutate(unique=raster::extract(template,.[,c(3,2)])) %>% 
  distinct(.,date,unique,presAbs,.keep_all = T)

# test=absences %>% group_by(date,unique)%>% summarise(n=n())

# trial=rbind(dat,absences) %>% dplyr::select(-long) %>% mutate(presAbs=as.character(presAbs))
trial=rbind(test_full,absences) %>% dplyr::select(-long) %>% mutate(presAbs=as.character(presAbs)) %>% 
  distinct(.,date,unique,presAbs,.keep_all = T)



# set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
# e.g. CRS("+proj=longlat +datum=WGS84")
sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
writeOGR(sp_poly_df, "chull", layer="chull", driver="ESRI Shapefile")

dat2 <- matrix(stats::rnorm(2000), ncol = 2)
ch <- chull(dat2)
coords <- dat2[c(ch, ch[1]), ]  # closed polygon

plot(dat, pch=19)
lines(coords, col="red")

baselayer=raster("/Users/heatherwelch/Dropbox/OLE/spatial_data/template_atlantic_clip.grd")
string=seq(1:ncell(baselayer))
baselayer[]=string
template=raster("/Users/heatherwelch/Dropbox/OLE/spatial_data/template_atlantic_clip.grd")
# template=raster::shift(rotate(raster::shift(template,dx=180)),dx=180)
baselayer=mask(baselayer,template)
template[]=string
outdir="/Users/heatherwelch/Dropbox/OLE/plots/plots_10_02_20"

## clean data ####
dat=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/topp_10_02_20.csv")
codes=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/sp_codes.csv")
dat1=left_join(dat,codes,by=c("species_id"="TOPP_code")) %>% dplyr::select(-c(sex,qc,include,alb_postbreed,X)) %>% 
  filter(!is.na(species_id)) %>% filter(!is.na(species)) %>% mutate(species= gsub(" ","",species)) %>%
  mutate(date2=mdy(gsub(" 0:00","",date))) %>% .[complete.cases(.),]%>% filter(species!="thresherShark"&species!="finWhale"&species!="mola"&species!="spermWhale"&species!="juvenileWhiteShark"&species!="humpbackWhale"&species!="northernFurSeal")
a=dat1 %>% mutate(long360=to360(long)) %>%
  filter(species!="finWhale"&species!="mola"&species!="spermWhale"&species!="juvenileWhiteShark"&species!="humpbackWhale"&species!="northernFurSeal")  %>%
  filter(species!="leatherbackTurtle" &species!="elephantSeal" &species!="sootyShearwater" &species!="pacificBluefinTuna" &species!="blueShark" &species!="salmonShark" &species!="black-footedAlbatross" &species!="laysanAlbatross")
lbst=dat1%>% mutate(long360=to360(long)) %>% filter(species=="leatherbackTurtle") %>% 
  filter(!(long360>228&lat<14)) %>% 
  filter(!(long360>249&lat<26)) %>% 
  filter(!(long360>223&lat<8&long360<230)) %>% 
  filter(eventid!=1079&eventid!=1078) %>% 
  filter(long360<300&long360>100)
eseal=dat1%>% mutate(long360=to360(long)) %>% filter(species=="elephantSeal") %>% 
  filter(long360>120&lat>0&long360<260) 
shear=dat1%>% mutate(long360=to360(long)) %>% filter(species=="sootyShearwater") %>% 
  filter(long360>100&lat<90) 
PBT=dat1%>% mutate(long360=to360(long)) %>% filter(species=="pacificBluefinTuna") %>% 
  filter(lat>0) 
blsh=dat1%>% mutate(long360=to360(long)) %>% filter(species=="blueShark") %>% 
  filter(long360>190&long360<300) 
sash=dat1%>% mutate(long360=to360(long)) %>% filter(species=="salmonShark") %>% 
  filter(long360<265&lat>10) 
bfal=dat1%>% mutate(long360=to360(long)) %>% filter(species=="black-footedAlbatross") %>% 
  filter(lat<90) 
laal=dat1%>% mutate(long360=to360(long)) %>% filter(species=="laysanAlbatross") %>% 
  filter(lat<90&lat>10) 
master=do.call("rbind",list(a,lbst,blsh,eseal,shear,PBT,sash,bfal,laal))

## generate pseudo_absences ####
sp=master$species %>% unique() %>% as.character()

empty=list()
for(i in 1:length(sp)){
  print(sp[i])
  dat=master %>% filter(species==sp[i]) %>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,.[,c(9,5)])) %>% 
    distinct(.,date,unique,presAbs,.keep_all = T)
  
  # dat=master %>% filter(species==sp[i]) %>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,.[,c(9,5)])) %>% 
  #   group_by(date,unique,eventid) %>% summarise(n=n())
  
  minx = min(dat$long360)
  maxx = max(dat$long360)
  miny = min(dat$lat)
  maxy = max(dat$lat)
  
  minpoly2=matrix(c(minx,miny, 
                    minx,maxy,
                    maxx,maxy,
                    maxx,miny,
                    minx,miny),
                  ncol=2,byrow = T)
  p = Polygon(minpoly2)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  # plot(sps)
  
  newras=mask(baselayer,sps)
  newras[values(newras)>1]=1
  
  newpoly=newras%>% rasterToPolygons(dissolve = T)
  
  abs=spsample(newpoly, nrow(dat), type="random") %>% as.data.frame() 
  absences=dat %>% mutate(long360=abs$x,lat=abs$y,presAbs=0)%>% mutate(unique=raster::extract(template,.[,c(9,5)])) %>% 
    distinct(.,date,unique,presAbs,.keep_all = T)
  
  # test=absences %>% group_by(date,unique)%>% summarise(n=n())
  
  # trial=rbind(dat,absences) %>% dplyr::select(-long) %>% mutate(presAbs=as.character(presAbs))
  trial=rbind(dat,absences) %>% dplyr::select(-long) %>% mutate(presAbs=as.character(presAbs)) %>% 
    distinct(.,date,unique,presAbs,.keep_all = T)
  
  plot_sp=ggplot()+
    geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    geom_point(data=trial,aes(x=long360,y=lat,color=as.factor(presAbs),group=presAbs),size = .5, shape = 21)+
    scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
    coord_cartesian(xlim = c(minx, maxx), ylim = c(miny,maxy))+
    ggtitle(glue("{sp[i]} npre={nrow(dat)} (1:1 ratio)"))
  
  plot_sp2=ggplot()+
    #geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    geom_point(data=trial,aes(x=long360,y=lat,color=as.factor(presAbs),group=presAbs),size = .5, shape = 21)+
    scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
    coord_cartesian(xlim = c(minx, maxx), ylim = c(miny,maxy))
  x=ggMarginal(plot_sp2,groupColour = TRUE, groupFill = F,type="density") 

  png(glue("{outdir}/{sp[i]}_presAbs.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  grid.arrange(plot_sp,x,ncol=2)
  dev.off()
  
  empty[[length(empty)+1]]=trial
  
}

full=do.call("rbind",empty)
write.csv(full,"/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_10_02_20.csv")

