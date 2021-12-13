## Create master dataframe for modelling

source("utilities/load_libraries.R")
to360 <- function(x) {x %% 360}
outdir="/Users/heatherwelch/Dropbox/OLE/plots/plots_03_22_21"; dir.create(outdir)
source("/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R")

# read in TOPP data ####
dat=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/topp/topp_10_02_20.csv")
codes=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/topp/sp_codes.csv")
dat1=left_join(dat,codes,by=c("species_id"="TOPP_code")) %>% dplyr::select(-c(sex,qc,include,alb_postbreed,X)) %>% 
   filter(!is.na(species_id)) %>% filter(!is.na(species)) %>% mutate(species= gsub(" ","",species)) %>%
   mutate(date2=mdy(gsub(" 0:00","",date))) %>% .[complete.cases(.),]%>% filter(species!="finWhale"&species!="mola"&species!="spermWhale"&species!="juvenileWhiteShark"&species!="humpbackWhale"&species!="northernFurSeal"&species!="thresherShark")

## remove first 10 points for each species
dat2=dat1%>% arrange(eventid,date2) %>% group_by(eventid)  %>% mutate(rank=1:n()) %>% as.data.frame() %>% 
   filter(rank>10) %>% dplyr::select(-c(date,species_id,segment)) %>% rename(lon=long) %>% mutate(data="TOPP")
head(dat2,50)

# read in new data from dallas / scott ####
bfal_df <- list.files(path="/Users/heatherwelch/Dropbox/OLE/data/species_data/Midway_Tern_datashare/Midway/Postbreeding_only/BFAL", 
                      full.names = TRUE,recursive = T) %>% 
   lapply(read_csv) %>% 
   bind_rows %>% mutate(species="black-footedAlbatross")

laal_df <- list.files(path="/Users/heatherwelch/Dropbox/OLE/data/species_data/Midway_Tern_datashare/Midway/Postbreeding_only/LAAL", 
                      full.names = TRUE,recursive = T) %>% 
   lapply(read_csv) %>% 
   bind_rows %>% mutate(species="laysanAlbatross")

midway=rbind(bfal_df,laal_df) %>% mutate(colony="Midway") %>% rename(lat=Latitude,long=Longitude) %>% mutate(date2=as.Date(dtime)) %>% 
   dplyr::select(-c(dtime))

# Melinda Conners modeled the Tern data included here, and this data set is just the postbreeding period
tern=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/Midway_Tern_datashare/Tern/Conners_TEIS_allmodeledGLS.csv") %>% 
   mutate(sp=case_when(species=="28"~"black-footedAlbatross",
                       species=="29"~"laysanAlbatross")) %>% mutate(date2=as.Date(GMT_YYYY.MM.DD)) %>% mutate(lat=zm.lat) %>% 
   mutate(long=zm.lon) %>% mutate(colony="Tern") %>% dplyr::select(sp,lat,long,date2,colony) %>% rename(species=sp)

everything=rbind(midway,tern)  %>% 
   mutate(lon=case_when(long > 180 ~ -360 + long,
                                              long<=180 ~ long))  %>% mutate(rank=1:n()) %>% mutate(eventid=20000) %>% 
   dplyr::select(-c(long,colony)) %>% mutate(data="Dallas")


# combine it all and subdivide bluefin and albacore data data ####
library(sf)
master_full=do.call("rbind",list(dat2,everything)) %>% mutate(long360=to360(lon))

cclme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/lme/lme.shp") 
goclme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
   filter(LME_NAME=="Gulf of California"|LME_NAME=="California Current") %>% st_union(.)
st_crs(goclme)=st_crs(cclme)

xcord=c(-125,-105,-105,-125)
ycord=c(45,45,30,30)
xym <- cbind(xcord, ycord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps)
sps=st_as_sf(sps)
st_crs(sps)=st_crs(cclme)

xcord=c(-120,-105,-105,-120)
ycord=c(31,31,23,23)
xym <- cbind(xcord, ycord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps2 = SpatialPolygons(list(ps))
# plot(sps2,add=T)
sps2=st_as_sf(sps2)
st_crs(sps2)=st_crs(cclme)

blfn=master_full %>% filter(species=="pacificBluefinTuna") %>% mutate(llat=lat,llon=lon)# %>% mutate(lon180=case_when(lon > 180 ~ -360 + lon,
                                                                                                             # lon<=180 ~ lon))
coordinates(blfn)=~llon+llat
blfn=st_as_sf(blfn)
st_crs(blfn)=st_crs(cclme)
cc=st_intersection(blfn,cclme)
trans=st_difference(blfn,goclme) #%>% st_difference(.,sps2)
t=st_difference(trans,sps2)%>% st_difference(.,sps)

alb=master_full %>% filter(species=="albacoretuna") %>% mutate(llat=lat,llon=lon) #%>% mutate(lon180=case_when(lon > 180 ~ -360 + lon,
                                                                                                      #lon<=180 ~ lon))
coordinates(alb)=~llon+llat
alb=st_as_sf(alb)
st_crs(alb)=st_crs(cclme)
cc_alb=st_intersection(alb,cclme)
trans_alb=st_difference(alb,goclme) #%>% st_difference(.,sps2)%>% st_difference(.,sps)
t_a=st_difference(trans_alb,sps2)%>% st_difference(.,sps)
   
setdiff(names(cc_alb),names(dat))
cc_alb2=cc_alb %>% mutate(species="albacoretuna_cc") %>% as.data.frame() %>% dplyr::select(c(species,eventid,lon,lat,date2,rank,long360)) 
trans_alb2=t_a %>% mutate(species="albacoretuna_trans") %>% as.data.frame()%>% dplyr::select(c(species,eventid,lon,lat,date2,rank,long360)) 
cc2=cc %>% mutate(species="pacificBluefinTuna_cc") %>% as.data.frame()%>% dplyr::select(c(species,eventid,lon,lat,date2,rank,long360)) 
trans2=t %>% mutate(species="pacificBluefinTuna_trans") %>% as.data.frame()%>% dplyr::select(c(species,eventid,lon,lat,date2,rank,long360)) 

master=do.call("rbind",list(cc_alb2,trans_alb2,cc2,trans2)) %>% mutate(data="TOPP")
# master2=master[,1:32] %>% rename(lat=llat)
# setdiff(names(dat),names(master2))

dat3=rbind(master_full,master) %>% mutate(lon180=lon) %>% dplyr::select(-lon) %>% rename(lon=long360) %>% 
   filter(lon180>(-180),lon180<(-100),lat>10,lat<62)

## write out dataset
write.csv(dat3,"/Users/heatherwelch/Dropbox/OLE/data/cleaned_species_data/sp_dat_modelFit_03_22_21.csv")


 