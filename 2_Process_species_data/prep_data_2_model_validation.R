## Processing and integrating novel species datasets

source("/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects/utilities/load_libraries.R")
library(sf)
library(splitstackshape)
to360 <- function(x) {x %% 360}
library(sf)
outdir="/Users/heatherwelch/Dropbox/OLE/plots/plots_09_23_21";dir.create(outdir)
boundingboxdir="/Users/heatherwelch/Dropbox/OLE/bounding_boxes"

# North Pacific Data Base https://www.usgs.gov/centers/asc/science/north-pacific-pelagic-seabird-database?qt-science_center_objects=0#qt-science_center_objects
NPDB=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/NPPSD_v3.0/tbl_DATA_OBS.csv")
NPDB_L=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/NPPSD_v3.0/tbl_LOCATION.csv")

# species=NPDB %>% filter(Common.Name=="Northern Elephant Seal"|Common.Name=="Black-footed Albatross"|Common.Name=="Blue Whale"|Common.Name=="Laysan Albatross"|Common.Name=="California Sea Lion"|Common.Name=="Sooty Shearwater"|Common.Name=="Murphy's Petrel"|Common.Name=="Flesh-Footed Shearwater")
species=NPDB %>% filter(Common.Name=="Northern Elephant Seal"|Common.Name=="Black-footed Albatross"|Common.Name=="Blue Whale"|Common.Name=="Laysan Albatross"|Common.Name=="California Sea Lion"|Common.Name=="Sooty Shearwater")
NPDB_master=left_join(species,NPDB_L,by="Master.Key") %>% 
  dplyr::select(Common.Name,Year,Month,Day,Lon,Lat,Number) %>% 
  rename(species=Common.Name,lat=Lat,lon=Lon,number=Number) %>% mutate(Month=str_pad(Month,2,side="left",pad="0")) %>% 
  mutate(date=glue("{Year}-{Month}-{Day}"))  %>% 
  # expandRows(.,count=7)%>% 
  dplyr::select(-c(Year,Month,Day))%>% mutate(species=as.character(species)) %>% 
  mutate(species=replace(species,species=="Black-footed Albatross","black-footedAlbatross")) %>% 
  mutate(species=replace(species,species=="Sooty Shearwater","sootyShearwater")) %>% 
  mutate(species=replace(species,species=="Laysan Albatross","laysanAlbatross")) %>% 
  mutate(data="npdb") %>% 
  mutate(species=replace(species,species=="Blue Whale","blueShark")) %>% 
  mutate(species=replace(species,species=="California Sea Lion","californiaSeaLion")) %>% 
  mutate(species=replace(species,species=="Flesh-Footed Shearwater","flesh-footedShearwater")) %>% 
  mutate(species=replace(species,species=="Murphy's Petrel","murphysPetrel")) %>% 
  mutate(species=replace(species,species=="Northern Elephant Seal","elephantSeal"))

# EBird https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html
a=read.delim("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird/ebd_bkfalb_201201_202010_relSep-2020/ebd_bkfalb_201201_202010_relSep-2020.txt")
b=read.delim("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird/ebd_layalb_201201_202010_relSep-2020/ebd_layalb_201201_202010_relSep-2020.txt")
c=read.delim("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird/ebd_sooshe_201201_202010_relSep-2020/ebd_sooshe_201201_202010_relSep-2020.txt")
# d=read.delim("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird/ebd_flfshe_relDec-2020/ebd_flfshe_relDec-2020.txt")
# e=read.delim("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird/ebd_murpet_relDec-2020/ebd_murpet_relDec-2020.txt")
speciesEbird=do.call("rbind",list(a,b,c))
speciesEbird=speciesEbird %>% filter(
  # effort filters
  DURATION.MINUTES <= 5 * 60,
  EFFORT.DISTANCE.KM <= 5,
  # last 10 years of data
  # year >= 2010,
  # 10 or fewer observers
  NUMBER.OBSERVERS <= 10)

ebird_master=speciesEbird %>% dplyr::select(COMMON.NAME,OBSERVATION.DATE,LONGITUDE,LATITUDE,OBSERVATION.COUNT) %>% 
  rename(lon=LONGITUDE,lat=LATITUDE,date=OBSERVATION.DATE,species=COMMON.NAME) %>% mutate(species=as.character(species)) %>% 
  # expandRows(.,count=5) %>% mutate(species=as.character(species)) %>% 
  mutate(species=replace(species,species=="Black-footed Albatross","black-footedAlbatross")) %>% 
  mutate(species=replace(species,species=="Sooty Shearwater","sootyShearwater")) %>% 
  mutate(species=replace(species,species=="Laysan Albatross","laysanAlbatross")) %>% 
  mutate(species=replace(species,species=="Flesh-footed Shearwater","flesh-footedShearwater")) %>% 
  mutate(species=replace(species,species=="Murphy's Petrel","murphysPetrel")) %>% 
  mutate(data="ebird") %>% rename(number=OBSERVATION.COUNT)

## tuna data from Barb ####
log=readRDS("/Users/heatherwelch/Dropbox/OLE/data/species_data/tuna_barb/logbooksCleaned_19982019.rds") %>% mutate(species="albacoretuna") %>% mutate(data="log") %>% rename(number=logCPUE,lon180=lon) %>% 
  dplyr::select(-c(vessel)) %>% mutate(lon=lon360) %>% rename(long360=lon360) %>%  mutate(date=as.Date(date))
tag=readRDS("/Users/heatherwelch/Dropbox/OLE/data/species_data/tuna_barb/tagsNOAAonly.rds")%>% mutate(species="albacoretuna") %>% mutate(data="tag") %>% mutate(number=1) %>% dplyr::select(-tag)%>% mutate(lon180=case_when(lon > 180 ~ -360 + lon,
                                                                                                                                                                                                                             lon<=180 ~ lon)) %>% 
  mutate(long360=lon) %>% mutate(date=as.Date(date))

# iattc=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/tuna_barb/PublicPSTuna/PublicPSTunaFlag.csv") %>% 
#   rename(albacoretuna=ALB,pacificBluefinTuna=PBF,yellowfinTuna=YFT,lat=LatC1) %>% mutate(lon180=LonC1,long360=to360(LonC1),lon=long360)

tunas=rbind(log,tag)

master=rbind(ebird_master,NPDB_master) %>%mutate(long360=to360(lon)) %>%  mutate(lon180=lon) %>% mutate(lon=to360(lon)) %>% 
  mutate(number=as.character(number))%>% mutate(date=as.Date(date))

## dgn observer data from steph ####
dgn=readRDS("/Users/heatherwelch/Dropbox/OLE/data/species_data/dgn_observer/DGN_speciesdata_forHW_v3.rds")
dgn2=dgn %>% mutate(species=as.character(CommonName)) %>% 
mutate(species=replace(species,species=="Shark, Shortfin Mako","makoShark")) %>% 
  mutate(species=replace(species,species=="Tuna, Yellowfin","yellowfinTuna")) %>% 
  mutate(species=replace(species,species=="Tuna, Bluefin","pacificBluefinTuna")) %>% 
  mutate(species=replace(species,species=="Tuna, Albacore","albacoretuna")) %>% 
  mutate(species=replace(species,species=="Shark, Salmon","salmonShark")) %>% 
  mutate(species=replace(species,species=="Seal, Northern Elephant","elephantSeal")) %>% 
  mutate(species=replace(species,species=="Turtle, Loggerhead","loggerheadTurtle")) %>%
  mutate(species=replace(species,species=="Shark, White","whiteShark")) %>%
  mutate(species=replace(species,species=="Sea Lion, California","californiaSeaLion")) %>%
  mutate(species=replace(species,species=="Shark, Blue","blueShark")) %>%
  mutate(species=replace(species,species=="Turtle, Leatherback","leatherbackTurtle")) %>% 
  mutate(lon180=lon,lon=to360(lon*-1),long360=lon) %>% rename(number=TotCat) %>% mutate(date=as.Date(date)) %>% 
  dplyr::select(-c(TripNumber_Set,Soak,SpCd,ScientificName,CommonName)) %>% mutate(data="dgn_observer") %>% 
  .[complete.cases(.),]

## bluewhales from OSU ####
osu=read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/OSU/dat_trks_2014_2017.csv")
test=osu %>% mutate(date=as.Date(date)) %>% mutate(year=year(date)) %>% group_by(year) %>% summarise(n=n())
osu2=osu %>% mutate(date=as.Date(date)) %>% mutate(species="blueWhale") %>% rename(lon=long) %>% 
  mutate(lon180=lon) %>% mutate(long360=to360(lon)) %>% mutate(lon=long360) %>% mutate(number=1) %>% mutate(data="OSU") %>% 
  filter(radflag=="GOOD") %>%
  dplyr::select(date,lat,lon,number,species,lon180,long360,data)

## salmon shark from Jordan ####
jw=readRDS("/Users/heatherwelch/Dropbox/OLE/data/species_data/Jordan_Watson/salmon_sharks.RDS")
test=jw %>% mutate(date=as.Date(RETRIEVAL_DATE)) %>% mutate(year=year(date)) %>% group_by(year) %>% summarise(n=n())
jw2=jw %>% mutate(date=as.Date(RETRIEVAL_DATE)) %>% mutate(species="salmonShark") %>% 
  mutate(lon180=RETRIEVAL_LONGITUDE_DD) %>% 
  mutate(lon=to360(RETRIEVAL_LONGITUDE_DD)) %>% mutate(long360=lon) %>% mutate(number=1) %>% mutate(data="akfin") %>%
  rename(lat=RETRIEVAL_LATITUDE_DD) %>% 
  dplyr::select(date,lat,lon,number,species,lon180,long360,data)

master2=rbind(master,tunas,dgn2,osu2,jw2) %>% filter(date>=as.Date("2000-01-01")) %>%  
  mutate(id=glue("{species}_{data}")) %>%   .[complete.cases(.),]

# fortommp=rbind(master,tunas,dgn2) %>% filter(species=="black-footedAlbatross"|species=="laysanAlbatross"|species=="sootyShearwater")
# write.csv(fortommp,"seabirds_forTommy.csv")

## just yellowfin and bluewhales and salmonshark
# master2=master2 %>% filter(species=="blueWhale"|species=="yellowfinTuna"|species=="salmonShark")
# test=master2 %>% mutate(year=year(date)) %>% group_by(year,species) %>% summarise(n=n())
## filter dataset to only within model domain: FULL domain and TOPP and Dallas only ####

sp=master2$id %>% unique()
empty=list()
for(i in 1:length(sp)){
  print(sp[i])
  if(grepl("ebird",sp[i])){spp=gsub("_ebird","",sp[i])}
  if(grepl("OSU",sp[i])){spp=gsub("_OSU","",sp[i])}
  if(grepl("npdb",sp[i])){spp=gsub("_npdb","",sp[i])}
  if(grepl("log",sp[i])){spp=gsub("_log","",sp[i])}
  if(grepl("tag",sp[i])){spp=gsub("_tag","",sp[i])}
  if(grepl("dgn_observer",sp[i])){spp=gsub("_dgn_observer","",sp[i])}
  if(grepl("_akfin",sp[i])){spp=gsub("_akfin","",sp[i])}
  print(spp)
  test=master2 %>% filter(id==sp[i]) %>% mutate(llat=lat,llong360=long360) 
  if(spp=="black-footedAlbatross"|spp=="laysanAlbatross"){
    boxx=st_read(glue("{boundingboxdir}/species_03_22_21/{spp}_Dallas.shp")) %>% st_as_sf()
  } else {
  boxx=st_read(glue("{boundingboxdir}/species_03_22_21/{spp}_TOPP.shp")) %>% st_as_sf()
  }
  
  coordinates(test)=~llong360+llat
  test=st_as_sf(test)
  st_crs(test)=st_crs(boxx)
  test_intersect=st_intersection(test,boxx)%>% as.data.frame(.,row.names = NULL)
  
  test2=test_intersect %>% dplyr::select(c(species,date,lon,lat,data,long360,id,number)) 
  rownames(test2)=NULL
  empty[[length(empty)+1]]=test2
}

all_combined=do.call("rbind",empty)
test=all_combined %>% mutate(year=year(date)) %>% group_by(year,species) %>% summarise(n=n())

## getting rid of hawaii midway tern
ablatross=all_combined %>% filter(grepl("Albatross",species)) %>% filter((lon<180|lon>210|lat<20|lat>30)) # getting rid of hawaii
non_ablatross=all_combined %>% filter(!grepl("Albatross",species)) 
all_combined=rbind(ablatross,non_ablatross)

## removing albacore cpue <0
alb_log=all_combined %>% filter(id=="albacoretuna_log") %>% filter(number>0)
non_alb_log=all_combined %>% filter(id!="albacoretuna_log") 
all_combined=rbind(alb_log,non_alb_log)

write.csv(all_combined,"/Users/heatherwelch/Dropbox/OLE/data/cleaned_validation_data/ebird_npdb_cleaned_09_23_21.csv")
