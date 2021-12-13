#extracto OLE - function to extract environmental data to xy species and vessel data
#written by Heather Welch 08.22.20
library(tidyverse)
library(glue)
library(parallel)
library(raster)
library(foreach)
library(tibble)

# how the function expects the dataframe to be formatted ##
# column 'date' -> YYYY-MM-DD in *character* format (will likely need to be reformatted from original data)
# column 'lon' -> 0 to 360 longitude column in numeric format (will likely need to be transformed from -180 to 180 formated in original data)
# column 'lat' -> -90 to 90 latitude column in numeric format (will likely be in correct format in original data)

# # generate some dummy data, comment this out before using, just here to demonstrate how functions work
# date=sample(seq(as.Date("2017-01-10"),as.Date("2018-02-10"),by="day"),size=30,replace = T) %>% as.character()
# lon=sample(seq(159,260,by=.25),size=length(date))
# lat=sample(seq(10,50,by=.25),size=length(date))
# othervar1=rep("test",length(date)) # added to make sure function carries thru other attributes
# othervar2=rep("test2",length(date))# added to make sure function carries thru other attributes
# df=data.frame(lon=as.numeric(lon),
# lat=as.numeric(lat),
# date=as.character(date),
# size=as.character(othervar1),
# vessel_class=as.character(othervar2),
# stringsAsFactors = F)

## function to extract all variables at once
## will only get mean +sd values, use uniivariate_extraction_custom_resFCN to get cvs
## will get all variables at specified resolutoin
# res=69375 # the radius in meters of the buffer to be extracted, here roughly 1.25/2 degree
#when type=="TOPP", it runs HW data; "fleets" runs TF data; "albacore" runs Barb Muhling data
multivariate_extraction_custom_resFCN=function(df,res,ncores, type){
  rootpath="/Volumes/Triple_Bottom_Line/Data/GloblaDataRasters_OLE/daily/"
  
  fullist=c(adt_sd= NA_real_,adt= NA_real_,bathy_sd= NA_real_,bathy= NA_real_,dist_eez= NA_real_,dist_nta= NA_real_,dist_shore= NA_real_,eke= NA_real_,l.chl= NA_real_,
            oxy200m= NA_real_,sla_sd= NA_real_,sla= NA_real_,sst_sd= NA_real_,sst= NA_real_,wave= NA_real_,wind= NA_real_,PPupper200m= NA_real_,
            oxycline=NA_real_,mld=NA_real_,dist_seamount=NA_real_)
  
  library(doParallel, quietly = TRUE) ## use 6 cores, but do it quietly. Note: use detectCores() to see how many available
  registerDoParallel(ncores) ## cores your computer has, and make sure you don't use all of them for this
  get_data=foreach(i=1:length(unique(df$date)),.export = c("df","rootpath","fullist"),.combine=rbind,.packages = c("glue","tidyverse","raster"),.verbose=T) %dopar% {
    # for(i in 1:length(unique(df$date))){
    newmean=function(x){
      mean(x,na.rm=T)}
    newsd=function(x){
      sd(x,na.rm=T)}
    
    get_date=unique(df$date)[i]
    print(get_date)
    rasdir=glue("{rootpath}{get_date}")
    rasnames1=list.files(rasdir,full.names = T,pattern = "grd") %>% grep("_sd",.,value=T,invert=T)
    rasstack=stack(rasnames1) # create daily stack and name each layer
    rasnames=rasnames1 %>% gsub(".grd","",.) %>% gsub(rasdir,"",.) %>% gsub("/","",.)
    names(rasstack)=rasnames
    
    if("oxycline" %in% rasnames){
      pos=grep("oxycline",rasnames)
      rasstack[[pos]][values(rasstack[[pos]])<(0)]=NA
    }
    extraction=raster::extract(rasstack,df %>% filter(date==get_date) %>% dplyr::select(lon,lat),buffer=res,fun=newmean,na.rm=T) %>% as.data.frame() # extract points from raster stack
    #print(ncol(extraction))
    toMatch <- c("adt", "sla", "bathy","sst")
    subset_rasstack=unique (grep(paste(toMatch,collapse="|"), 
                                 rasnames1, value=TRUE)) %>% stack()
    
    names_subset_rasstack=unique (grep(paste(toMatch,collapse="|"), 
                                 rasnames1, value=TRUE)) %>% gsub(".grd","",.) %>% gsub(rasdir,"",.) %>% gsub("/","",.) %>% lapply(.,function(x)glue("{x}_sd")) %>% unlist()
    
    names(subset_rasstack)=names_subset_rasstack
    extraction_sd=raster::extract(subset_rasstack,df %>% filter(date==get_date) %>% dplyr::select(lon,lat),buffer=res,fun=newsd,na.rm=T) %>% as.data.frame()
    
    extraction_full=cbind(extraction,extraction_sd)
    extraction_full=add_column(extraction_full, !!!fullist[setdiff(names(fullist),names(extraction_full))]) # add in any missing columns (in case a variable w.n. available for a specific day)
    
    if (type == "fleets"){ #to account for differences in input data
      master=cbind((df %>% filter(date==get_date)),extraction_full) %>% dplyr::select(c(date,lon,lat,Year,mmsi,fishing_hours,Name_F,Identifier,type,adt,bathy,dist_eez,dist_nta,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla,sst,wave,wind,adt_sd,bathy_sd,sla_sd,sst_sd,dist_seamount)) # steph's code line
    } 
    if (type == "TOPP"){ #to account for differences in input data
      master=cbind((df %>% filter(date==get_date)),extraction_full) %>% dplyr::select(c(data,eventid,id,dt,lat,species,date,lon,rank,presAbs,unique,unique2,adt,bathy,dist_eez,dist_nta,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla,sst,wave,wind,adt_sd,bathy_sd,sla_sd,sst_sd)) # heather's code line
    }
    if (type == "albacore"){
      master=cbind((df %>% filter(date==get_date)),extraction_full) %>% dplyr::select(c(sst_supplied, bathym_supplied, province, id,
                                                                                        id,lat,date,lon,adt,bathy,dist_eez,dist_nta,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla,sst,wave,wind,adt_sd,bathy_sd,sla_sd,sst_sd,dist_seamount)) # steph's code line
    }
    if (type == "hw_validation"){ #to account for differences in input data
      master=cbind((df %>% filter(date==get_date)),extraction_full) %>% dplyr::select(c(number,data,species,id,dt,lat,date,lon,adt,bathy,dist_eez,dist_nta,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla,sst,wave,wind,adt_sd,bathy_sd,sla_sd,sst_sd)) # heather's code line
    }
    return(master)
    #print(ncol(master))
  }
  return(get_data)
} 

# test=multivariate_extraction_custom_resFCN(df=dat_temp2[1:100,],res=69375,ncores=10, fleets = TRUE)
# test=multivariate_extractionFCN(df=df)
