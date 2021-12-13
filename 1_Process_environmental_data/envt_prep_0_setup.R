# written by Heather Welch 07.01.20

#1. generate template, .25 degrees, 0-360, global
#2. create daily folders

source("utilities/load_libraries.R")

#1. generate template, .25 degrees, 0-360, global ####
sla=raster("/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_SSH_uv/1993/dt_global_allsat_phy_l4_19930101_20190101.nc",varname="sla") #%>% rotate()
values(sla)=1
writeRaster(sla,"/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")

#2. create daily folders ####
daydates=seq(as.Date("1980-01-01"), as.Date("2021-01-01"),by="day")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily";dir.create(dailyDir)

create_folder=function(directory,date){
  a=glue("{directory}/{date}");dir.create(a)
}

lapply(daydates,FUN=create_folder,directory=dailyDir)
