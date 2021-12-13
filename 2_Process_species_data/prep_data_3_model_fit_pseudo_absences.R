## Creates psuedo_absences and minimum bounding polygons for model fitting data

source("utilities/load_libraries.R")
library(sf)
require("rgdal")
require("rgeos")
require("dplyr")
to360 <- function(x) {x %% 360}
library(ggExtra)
library(maptools)
library(adehabitatHR)

# read in data - this is oldstuff from 12-03-20 ####
# https://www.marineregions.org/gazetteer.php?p=details&id=1908
dat=read.csv("/Users/heatherwelch/Dropbox/OLE/data/cleaned_species_data/sp_dat_modelFit_03_22_21.csv") %>% 
  mutate(long360=lon)  %>% mutate(id=glue("{species}_{data}"))
sp=unique(dat$id)
outdir="/Users/heatherwelch/Dropbox/OLE/plots/plots_03_22_21"

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
test_full=dat %>% filter(id==sp[i])%>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,.[,c(8,3)])) %>%
  mutate(unique2=glue("{date2}{unique}"))#%>%
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

abs=spsample(test2, nrow(test)*2, type="random") %>% as.data.frame() 
absence=test_full %>% rbind(test_full) %>% mutate(long360=to360(abs$x),lat=abs$y)%>% mutate(unique=raster::extract(template,.[,c(8,3)])) %>% 
  mutate(unique2=glue("{date2}{unique}")) %>% 
  filter(!(unique2 %in%test_full$unique2)) %>% .[sample(nrow(.),nrow(test_full)),] %>% mutate(presAbs=0)

trial=rbind(test_full,absence) %>% dplyr::select(-lon) %>% mutate(presAbs=as.character(presAbs)) %>% arrange(presAbs)

minx =to360(-180)
maxx = to360(-100)
miny = to360(10)
maxy = to360(62)

test2_sf=st_as_sf(test2)
st_write(test2_sf,glue("/Users/heatherwelch/Dropbox/OLE/bounding_boxes/species_03_22_21/{sp[i]}.shp"),layer_options = "OVERWRITE=true")

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
write.csv(master,"/Users/heatherwelch/Dropbox/OLE/data/species_data_w_bkdg/sp_dat_bkgd_03_22_21.csv")



