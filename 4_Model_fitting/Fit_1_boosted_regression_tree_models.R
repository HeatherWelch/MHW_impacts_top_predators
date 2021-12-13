### fitting species data

source("/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects_new/utilities/load_libraries.R")
# library(insol)

## multi-variable BRTs ####
# # dropping out sla_sd
# library(foreach)
# library(doParallel, quietly = TRUE)
# 
# dat=read.csv("/Users/heatherwelch/Downloads/sp_dat_bkgd_envt_03_22_21.csv") %>%
#   mutate(day=yday(date))
#   
# sp=dat$id %>% unique()
# 
# registerDoParallel(4)
# system.time(print(
#   foreach(i=1:length(sp),.packages = c("gbm","glue"),.verbose=T,.export = c("dat")) %dopar% {
#     print(sp[i])
#     
#     tryCatch(
#       expr ={
#         
#           main=dat%>%filter(id==sp[i]) %>% 
#             dplyr::select(c(id,date,day,species,lat,lon,presAbs,adt_sd,adt,bathy_sd,bathy,dist_shore,eke,l.chl,mld,oxy200m,oxycline,PPupper200m,sla_sd,sla,sst_sd,sst)) 
#             daylight=daylength(main$lat,main$lon,JD(main$date),-9) %>% as.data.frame()
#             main=main %>%  mutate(daylight=daylight$daylen)
# 
#         main=main %>% mutate(random=sample(1:nrow(main)))
#         main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
#         
#         #full model no reception
#         gbm.x=c("bathy_sd","eke","l.chl","mld","oxy200m","PPupper200m","sla","sst_sd","random","sst","bathy","day")
#         family="bernoulli"
#         if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.001}
#         if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.01}
#         tc=3
#         bf=0.6
#         tolerance = .00001
#         type=glue("{sp[i]}")
#         
#         
#         tryCatch(
#           expr ={
#             gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="presAbs",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
#             
#             if(gap_brt_poiss_step$n.trees<2000){
#               print("N trees too low, refitting with smaller LR")
#               lr=lr/10*5
#               gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
#             }
#             
#             if(gap_brt_poiss_step$n.trees<2000){
#               print("N trees too low, refitting with smaller LR")
#               lr=lr/10*5
#               gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
#             }
#             
#             name=glue("/Users/heatherwelch/Dropbox/OLE/models/species/brt/species_bernoulli_03_22_21_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_{type}.rds")
#             write_rds(gap_brt_poiss_step,name)
#           },
#           error = function(e){
#             message(glue("Model not working"))
#             print(e)
#           }
#         )
#       },
#       error = function(e){
#         message(glue("No data"))
#         print(e)
#       }
#     )
#   }
# ))
# 

## sst-only BRTs ####
# dropping out sla_sd
library(foreach)
library(doParallel, quietly = TRUE)

dat=read.csv("/Users/heatherwelch/Downloads/sp_dat_bkgd_envt_03_22_21.csv") %>%
  mutate(day=yday(date))

sp=dat$id %>% unique()

registerDoParallel(8)
system.time(print(
  foreach(i=1:length(sp),.packages = c("gbm","glue"),.verbose=T,.export = c("dat")) %dopar% {
    print(sp[i])
    
    tryCatch(
      expr ={
        
        main=dat%>%filter(id==sp[i]) %>% 
          dplyr::select(c(id,date,day,species,lat,lon,presAbs,sst)) 
        # daylight=daylength(main$lat,main$lon,JD(main$date),-9) %>% as.data.frame()
        # main=main %>%  mutate(daylight=daylight$daylen)
        
        # main=main %>% mutate(random=sample(1:nrow(main)))
        main=main %>% mutate(presAbs=as.integer(round(presAbs))) %>% .[complete.cases(.),]
        
        #full model no reception
        gbm.x=c("sst")
        family="bernoulli"
        if(main %>% filter(presAbs==1) %>% nrow()<500){lr=0.00001}
        if(main %>% filter(presAbs==1) %>% nrow()>=500){lr=0.0001}
        tc=3
        bf=0.6
        tolerance = .00001
        type=glue("{sp[i]}")
        
        
        tryCatch(
          expr ={
            gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="presAbs",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
            
            if(gap_brt_poiss_step$n.trees<2000){
              print("N trees too low, refitting with smaller LR")
              lr=lr/10*5
              gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
            }
            
            if(gap_brt_poiss_step$n.trees<2000){
              print("N trees too low, refitting with smaller LR")
              lr=lr/10*5
              gap_brt_poiss_step = gbm.fixed(main,gbm.x=gbm.x,gbm.y="sum_gaps",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf,n.trees = 2000)
            }
            
            name=glue("/Users/heatherwelch/Dropbox/OLE/models/species/brt/species_bernoulli_04_30_21_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}_{type}_SST.rds")
            write_rds(gap_brt_poiss_step,name)
          },
          error = function(e){
            message(glue("Model not working"))
            print(e)
          }
        )
      },
      error = function(e){
        message(glue("No data"))
        print(e)
      }
    )
  }
))

