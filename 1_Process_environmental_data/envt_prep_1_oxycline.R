# written by Heather Welch 07.01.20

##### NEED TO DECIDE DEPTH LEVEL/S
### NEED TO DOUBLE CHECK ALL DATES

library(raster)
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)
library(foreach)

template=raster("/Users/EcoCast/Dropbox/OLE/spatial_data/template.grd")
dailyDir="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily"
fileDir="/Volumes/Triple_Bottom_Line/Data/GlobalData/CMEMS_Oxygen"
Files=list.files(fileDir,full.names = T,recursive = T)

writeRasterTemp=function(file,outdir,var){
  system.time(
  tryCatch(
    expr ={
  if(grepl("Historical",file)){
    date=substr(file,start=147,stop=154) %>% as.Date(.,format="%Y%m%d")
  } else if (grepl("NRT",file)){
    date=substr(file,start=102,stop=109) %>% as.Date(.,format="%Y%m%d")
  }
  
  savename=glue("{outdir}/{date}/{var}.grd")
  print(date)
  
  if(!file.exists(savename)){
  print("oxycline doesn't exist, writing out")
    
  # extractPISCES35o2 <- function(myNC) { #### old combo function from barb and mike
  #     # geovars are longitude (-180 - 180), latitude, depth, time, one netcdf per date
  #     dataFile <- nc_open(myNC)
  #     lat <- ncvar_get(dataFile, 'latitude')
  #     lon <- ncvar_get(dataFile, 'longitude')
  #     depth <- ncvar_get(dataFile, 'depth')
  #     o2all <- ncvar_get(dataFile, "o2") / 44.661
  #     oxycline <- data.frame(matrix(-99, nrow = length(lon), ncol = length(lat)))
  #     for (i in 1:length(lon)) {
  #       for (j in 1:length(lat)) {
  #         # Extracting each from netcdf is much slower!
  #         # o2 <- ncvar_get(dataFile, "o2", start = c(i, j, 1, 1), count = c(1, 1, length(depth), 1), verbose = FALSE)
  #         o2 <- o2all[i,j,]
  #         # If not enough o2 points to interpolate, (e.g. on land), skip to next point
  #         if (sum(!is.na(o2)) < 3) {next}
  #         # In the North Pacific, o2 often decreases with depth and then increases again.
  #         # We only want the shallower minimum, so here I'm trimming o2 to only values above the o2 minimum depth
  #         depthMin <- which.min(o2)
  #         depthToInterp <- depth[depthMin-1:depthMin]
  #         o2ToInterp <- o2[depthMin-1:depthMin]
  #         # Making sure there's still enough values to interpolate
  #         if (sum(!is.na(o2ToInterp)) < 3) {next}
  #         o2Interp <- approx(o2ToInterp, depthToInterp, 3.5, ties = mean)
  #         oxycline[i,j] <- o2Interp$y
  #       }
  #     }
  #     nc_close(dataFile)
  #     return(oxycline)
  # }
  
  extractPISCES35o2 <- function(myNC) {
    # geovars are longitude (-180 - 180), latitude, depth, time, one netcdf per date
    dataFile <- nc_open(myNC)
    lat <- ncvar_get(dataFile, 'latitude')
    lon <- ncvar_get(dataFile, 'longitude')
    depth <- ncvar_get(dataFile, 'depth')
    o2all <- ncvar_get(dataFile, "o2") / 44.661
    oxycline <- data.frame(matrix(-99, nrow = length(lon), ncol = length(lat)))
    for (i in 1:length(lon)) {
      for (j in 1:length(lat)) {
        # Extracting each from netcdf is much slower!
        # o2 <- ncvar_get(dataFile, "o2", start = c(i, j, 1, 1), count = c(1, 1, length(depth), 1), verbose = FALSE)
        o2 <- o2all[i,j,]
        # If not enough o2 points to interpolate, (e.g. on land), or if all values > 3.5, skip to next point
        if (sum(!is.na(o2)) < 3) {next}
        if(min(o2, na.rm = TRUE) > 3.5) {next}
        # In the North Pacific, o2 often decreases with depth and then increases again.
        # We only want the shallower minimum, so here I'm trimming o2 to only values above the o2 minimum depth
        depthMin <- which.min(o2)
        depthAboveMin <- depth[1:depthMin]
        o2AboveMin <- o2[1:depthMin]
        # To save processing time, just interpolate at the point closest to 3.5, and the two either side of it
        depthMin35 <- which.min(abs(o2AboveMin - 3.5)) 
        depthToInterp <- depth[(depthMin35 - 1) : (depthMin35 + 1)]
        o2ToInterp <- o2[(depthMin35 - 1) : (depthMin35 + 1)]
        # Making sure there's still enough values to interpolate
        if (sum(!is.na(o2ToInterp)) < 3) {next}
        o2Interp <- approx(o2ToInterp, depthToInterp, 3.5, ties = mean)
        # Occasionally the closest depth to 3.5 is a local minima/wiggle, and interpolation will return NA (~ 4200 points)
        # In that case, use all depths/o2 values above o2min for the interpolation
        if(is.na(o2Interp$y)) {
          o2Interp <- approx(o2AboveMin, depthAboveMin, 3.5, ties = mean)
        }
        oxycline[i,j] <- o2Interp$y
      }
    }
    nc_close(dataFile)
    return(oxycline)
  }
  
  dataFile <- nc_open(file)
  lat <- ncvar_get(dataFile, 'latitude')
  lon <- ncvar_get(dataFile, 'longitude')
  nc_close(dataFile)
  
  # startTime <- Sys.time()
  oxyclineTest <- extractPISCES35o2(file)
  # endTime <- Sys.time()
  # timeTaken <- endTime - startTime
  
  templatenative=raster(file)
  colnames(oxyclineTest)=lat
  master=oxyclineTest %>% mutate(lon=lon) %>% gather(lat,oxy,-c(lon)) %>% mutate(lat=as.numeric(lat))
  field=master$oxy
  r= master %>% dplyr::select(lon,lat) %>% rasterize(.,templatenative,fun=mean,field=field)
  r2=shift(rotate(shift(r,dx=180)),dx=180) # convert from kelvin to celsius and make 0-360
  r_res=raster::resample(r2,template)
  
  writeRaster(r_res,savename,overwrite=T)
 
  }
  
    },
  error = function(e){
    message(glue("Something went wrong"))
    print(e)
  }
  )
  )
}

mclapply(Files,FUN=writeRasterTemp,outdir=dailyDir,mc.cores=5,var="oxycline")
#system.time(lapply(Files,FUN=writeRasterTemp,outdir=dailyDir,var="oxycline"))

# a=difftime(as.Date("2020-04-01"),as.Date("1992-01-01"), units = c("days")) %>% as.numeric()
# minutes=a*6
# days=minutes/60/24

## a couple of other attempts at doing this, saving for future reference ####
### best attempt kdslfakjdklsa
# dname="o2"
# nc.data=nc_open(file)
# lat <- ncvar_get(nc.data,'latitude')
# lon <- ncvar_get(nc.data,'longitude')
# nlon <- dim(lon)
# nlat <- dim(lat)
# depth <- ncvar_get(nc.data,'depth')
# ndepth=dim(depth)
# 
# tmp.array <- ncvar_get(nc.data, dname)
# fillvalue <- ncatt_get(nc.data, dname, "_FillValue")
# nt <- dim(depth)
# tmp.array <- ncvar_get(nc.data, dname)
# tmp.array[tmp.array==fillvalue$value]=NA
# tmp.vec.long <- as.vector(tmp.array)
# tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
# lonlat <- expand.grid(lon, lat)
# names(lonlat) <- c("lon","lat")
# tmp.df02 <- data.frame(tmp.mat)
# names(tmp.df02) <- depth
# tmp.df02 <- cbind(lonlat, tmp.df02)
# 
# library(doParallel, quietly = TRUE)
# registerDoParallel(6)
# surface=foreach(i=1:nrow(tmp.df02),.export = c("tmp.df02","lat","depth","dname","nc.data"),.combine=rbind,.packages = c("glue","tidyverse","foreach"),.verbose=T) %dopar% {
#   vect=tmp.df02[i,3:ncol(tmp.df02)] 
#   b= which.min(abs(vect - 156.3135)) %>% names() %>% as.numeric() %>% round(.,1)
#   if(length(b)<1){b=NA}
#   
#   c=data.frame(lon=as.numeric(tmp.df02[i,1]),
#                lat=as.numeric(tmp.df02[i,2]),
#                depth=as.numeric(b,1))
# }

###end best attempt dksalfjdks
# surface<-foreach(i=lat,.export = c("lon","lat","depth","dname","nc.data"),.combine=rbind,.packages = c("ncdf4","glue","tidyverse","foreach","doParallel","parallel"),.verbose=T) %:%
#   # print(lat[i])
#   # for(lo in 1:length(lon)){
#   foreach(ii=lon,.export = c("lon","lat","depth","dname","nc.data"),.combine=rbind,.packages = c("ncdf4","glue","tidyverse","foreach","doParallel","parallel"),.verbose=T) %dopar% {
#     print(glue("*{ii} {i}*"))
#     # for(la in 1:2){
#     #   for(lo in 1:2){
#     # tryCatch(
#     #   expr ={
#     # a=ncvar_get(nc.data,dname,start=c(lon[ii],lat[i],1,1), 
#     #           count=c(1,1,-1,1),verbose=FALSE) 
#     lo=grep(ii,lon)
#     la=grep(i,lat)
#     print(glue("Start is ({lo},{la},1,1"))
#     
#     a=ncvar_get(nc.data,dname,start=c(lo,la,1,1), 
#                 count=c(1,1,-1,1),verbose=F) 
#     
#     b= which.min(abs(a - 156.3135))
#     d=depth[b]
#     if(length(d)<1){d=NA}
#     # c=data.frame(lon=as.numeric(lon[ii]),
#     #              lat=as.numeric(lat[i]),
#     #              depth=as.numeric(d))
#     c=data.frame(lon=as.numeric(ii),
#                  lat=as.numeric(i),
#                  depth=as.numeric(d))
#     # empty[[length(empty)+1]]<-c
#     #   },
#     # error = function(e){
#     #   message(glue("Model not working {lo} {la}"))
#     #   # c=data.frame(lon=as.numeric(lon[ii]),
#     #   #              lat=as.numeric(lat[i]),
#     #   #              depth=NA)
#     #   c=data.frame(lon=as.numeric(ii),
#     #                lat=as.numeric(i),
#     #                depth=NA)
#     #   print(e)
#     # }
#     # )
#     
#   }
# 
# surface<-foreach(i=600:601,.export = c("lon","lat","depth","dname","nc.data"),.combine=rbind,.packages = c("ncdf4","glue","tidyverse","foreach","doParallel","parallel"),.verbose=T) %dopar% {
#   # print(lat[i])
#   # for(lo in 1:length(lon)){
#   foreach(ii=600:600,.export = c("lon","lat","depth","dname","nc.data","i"),.combine=rbind,.packages = c("ncdf4","glue","tidyverse","foreach","doParallel","parallel"),.verbose=T) %dopar% {
#     print(glue("*{ii} {i}*"))
#     # for(la in 1:2){
#     #   for(lo in 1:2){
#     # tryCatch(
#     #   expr ={
#     # a=ncvar_get(nc.data,dname,start=c(lon[ii],lat[i],1,1), 
#     #           count=c(1,1,-1,1),verbose=FALSE) 
#     # lo=grep(ii,lon)
#     # la=grep(i,lat)
#     # print(glue("Start is ({lo},{la},1,1"))
#     
#     a=ncvar_get(nc.data,dname,start=c(ii,i,1,1), 
#                 count=c(1,1,-1,1),verbose=T) 
#     
#     b= which.min(abs(a - 156.3135))
#     d=depth[b]
#     if(length(d)<1){d=NA}
#     # c=data.frame(lon=as.numeric(lon[ii]),
#     #              lat=as.numeric(lat[i]),
#     #              depth=as.numeric(d))
#     c=data.frame(lon=as.numeric(lon[ii]),
#                  lat=as.numeric(lat[i]),
#                  depth=as.numeric(d))
#     # empty[[length(empty)+1]]<-c
#     #   },
#     # error = function(e){
#     #   message(glue("Model not working {lo} {la}"))
#     #   # c=data.frame(lon=as.numeric(lon[ii]),
#     #   #              lat=as.numeric(lat[i]),
#     #   #              depth=NA)
#     #   c=data.frame(lon=as.numeric(ii),
#     #                lat=as.numeric(i),
#     #                depth=NA)
#     #   print(e)
#     # }
#     # )
#     
#   }
# }