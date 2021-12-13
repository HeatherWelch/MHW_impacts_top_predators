#Code to get distance from seamounts
#Code from Nima, updated by Steph 9 Dec 2020
#CAUTION: this code takes ~8rs to run 

library(rgdal)
library(tidyverse)
library(raster)

#Get global file as a template
r <- raster("/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily/1980-01-01/bathy.grd")
r <- rotate(r) #needs to be -180 to 180

#read in seamount shapefile
seamounts_sf<-readOGR("/Volumes/Triple_Bottom_Line/Data/GlobalData/Seamounts Shapefile/Seamounts.shp")

#creating a distance to point raster
d1 <- distanceFromPoints(object = r, xy = seamounts_sf) #CAUTION: THIS TAKES 8HRS

#need to mask d1 with the environmental raster so values over the land is taken out
seamount_dis<-mask(d1,r)
seamount_dis<-seamount_dis*0.001 #converting to km
seamount_dis <- shift(rotate(shift(seamount_dis, dx=180)), dx=180) #rotate to match OLE dims
writeRaster(seamount_dis, "/Volumes/Triple_Bottom_Line/Data/GlobalData/Distance_to_Seamounts")

#Now save the raster to every daily folder in OLE global rasters project
seamount_dis <- raster("/Volumes/Triple_Bottom_Line/Data/GlobalData/Distance_to_Seamounts.grd")

dirs <- list.files('/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily/', recursive = FALSE, full.names = TRUE)
for (d in dirs){
  print(d)
  writeRaster(seamount_dis, paste0(d,"/","dist_seamount"), overwrite=T)
}

