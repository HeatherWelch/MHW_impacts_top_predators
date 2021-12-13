#BRTs Model Evaluation

#1. full validation ####
eval_full_heather_fixed <- function(dataInput, gbm.x, gbm.y, lr, tc=tc,tolerance.method,tolerance,family,response){
  DataInput <- dataInput
  if(family=="bernoulli"){
    Evaluations_7525 <- as.data.frame(matrix(data=0,nrow=1,ncol=7))
    colnames(Evaluations_7525) <- c("Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres")
  }
  
  if(family=="poisson"){
    Evaluations_7525 <- as.data.frame(matrix(data=0,nrow=1,ncol=5))
    colnames(Evaluations_7525) <- c("pearson","spearman","Deviance","RMSE","AVE")
  }
  
  DataInput_train<- DataInput
  DataInput_test<- DataInput
  DataInput.kfolds <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                family=family, tree.complexity=tc,
                                learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
  preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
  position=grep(response,names(dataInput))
  null <- DataInput.kfolds$self.statistics$null.deviance
  res <- DataInput.kfolds$self.statistics$resid.deviance
  dev=((null - res)/null)*100 
  # dev <- calc.deviance(obs=DataInput_test[[response]], pred=preds, calc.mean=TRUE,family = family)
  d <- cbind(DataInput_test[[response]], preds)
  test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
  if(family=="bernoulli"){
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    e <- evaluate(p=pres, a=abs)
    thresh=threshold(e)$equal_sens_spec
    ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
    Evaluations_7525[1,1] <- dev
    Evaluations_7525[1,2] <- e@auc
    Evaluations_7525[1,3] <- max(e@TPR + e@TNR-1)
    Evaluations_7525[1,4] <- e@TPR[ind]
    Evaluations_7525[1,5] <- e@TNR[ind]
    Evaluations_7525[1,6] <- test[1,2]
    Evaluations_7525[1,7] <- test[2,2]
  }
  if(family=="poisson"){
    colnames(d) <- c("observed","predicted")
    d <- as.data.frame(d)
    pear=cor(d$predicted, d$observed, use="na.or.complete",method = "pearson")
    spear=cor(d$predicted, d$observed, use="na.or.complete",method = "spearman")
    # lm(d$observed~d$predicted)
    rmse=sqrt(mean((d$observed - d$predicted)^2)) #RMSE
    ave=mean(d$observed - d$predicted) #AVE
    
    Evaluations_7525[1,1] <- pear
    Evaluations_7525[1,2] <- spear
    Evaluations_7525[1,3] <- dev
    Evaluations_7525[1,4] <- rmse
    Evaluations_7525[1,5] <- ave
  }
  
  return(Evaluations_7525)}



#2. 75:25 cross validation * 50 ####
eval_7525_heather_fixed_50 <- function(dataInput, gbm.x, gbm.y, lr, tc=tc,tolerance.method,tolerance,family,response){
  DataInput <- dataInput
  if(family=="bernoulli"){
    Evaluations_7525 <- as.data.frame(matrix(data=0,nrow=1,ncol=7))
    colnames(Evaluations_7525) <- c("Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres")
  }
  
  if(family=="poisson"){
    Evaluations_7525 <- as.data.frame(matrix(data=0,nrow=1,ncol=5))
    colnames(Evaluations_7525) <- c("pearson","spearman","Deviance","RMSE","AVE")
  }
  for(i in 1:50){
    DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
    DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
    DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
    DataInput.kfolds <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                  family=family, tree.complexity=tc,
                                  learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
    preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                         n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
    position=grep(response,names(dataInput))
    # dev <- calc.deviance(obs=DataInput_test[[response]], pred=preds, calc.mean=TRUE,family = family)
    null <- DataInput.kfolds$self.statistics$null.deviance
    res <- DataInput.kfolds$self.statistics$resid.deviance
    dev=((null - res)/null)*100 
    
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    if(family=="bernoulli"){
      pres <- d[d[,1]==1,2]
      abs <- d[d[,1]==0,2]
      e <- evaluate(p=pres, a=abs)
      thresh=threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_7525[i,1] <- dev
      Evaluations_7525[i,2] <- e@auc
      Evaluations_7525[i,3] <- max(e@TPR + e@TNR-1)
      Evaluations_7525[i,4] <- e@TPR[ind]
      Evaluations_7525[i,5] <- e@TNR[ind]
      Evaluations_7525[i,6] <- test[1,2]
      Evaluations_7525[i,7] <- test[2,2]
    }
    if(family=="poisson"){
      colnames(d) <- c("observed","predicted")
      d <- as.data.frame(d)
      pear=cor(d$predicted, d$observed, use="na.or.complete",method = "pearson")
      spear=cor(d$predicted, d$observed, use="na.or.complete",method = "spearman")
      # lm(d$observed~d$predicted)
      rmse=sqrt(mean((d$observed - d$predicted)^2)) #RMSE
      ave=mean(d$observed - d$predicted) #AVE
      
      Evaluations_7525[1,1] <- pear
      Evaluations_7525[1,2] <- spear
      Evaluations_7525[1,3] <- dev
      Evaluations_7525[1,4] <- rmse
      Evaluations_7525[1,5] <- ave
    }
  }
  
  return(Evaluations_7525)}


#3. LOO cross validation ####
LOO_eval_heather <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response){
  DataInput=DataInput %>% mutate(Year=year(date))
  if(family=="bernoulli"){
    Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
    colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
  }
  counter=1
  for (y in min(DataInput$Year):max(DataInput$Year)){
    print(y)
    DataInput_train <- DataInput[DataInput$Year!=y,]
    DataInput_test <- DataInput[DataInput$Year==y,]
    DataInput.loo <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                               family=family, tree.complexity=tc,
                               learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=DataInput.loo$gbm.call$best.trees, type="response")
    position=grep(response,names(DataInput))
    # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
    null <- DataInput.loo$self.statistics$null.deviance
    res <- DataInput.loo$self.statistics$resid.deviance
    dev=((null - res)/null)*100 
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    if(length(pres)>0 & length(abs)>0){
    e <- evaluate(p=pres, a=abs)
    thresh=threshold(e)$equal_sens_spec
    ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
    Evaluations_LOO[counter,1] <- y
    Evaluations_LOO[counter,2] <- dev
    Evaluations_LOO[counter,3] <- e@auc
    Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
    Evaluations_LOO[counter,5] <- e@TPR[ind]
    Evaluations_LOO[counter,6] <- e@TNR[ind]
    Evaluations_LOO[counter,7] <- test[1,2]
    Evaluations_LOO[counter,8] <- test[2,2]
    Evaluations_LOO[counter,9] <- nrow(DataInput_test)
    Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)
    counter=counter+1
  }
  }
  return(Evaluations_LOO)}

#4. SOO cross validation ####
SOO_eval_heather <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response){
  DataInput=DataInput %>% mutate(Year=year(date))
  DataInput.loo <- gbm.fixed(data=DataInput, gbm.x= gbm.x, gbm.y = gbm.y, 
                             family=family, tree.complexity=tc,
                             learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
  if(family=="bernoulli"){
    Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
    colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
  }
  counter=1
  for (y in min(DataInput$Year):max(DataInput$Year)){
    print(y)
    # DataInput_train <- DataInput[DataInput$Year!=y,]
    DataInput_test <- DataInput[DataInput$Year==y,]
    # DataInput.loo <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
    #                            family=family, tree.complexity=tc,
    #                            learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=DataInput.loo$gbm.call$best.trees, type="response")
    position=grep(response,names(DataInput))
    # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
    null <- DataInput.loo$self.statistics$null.deviance
    res <- DataInput.loo$self.statistics$resid.deviance
    dev=((null - res)/null)*100 
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    if(length(pres)>0 & length(abs)>0){
      e <- evaluate(p=pres, a=abs)
      thresh=threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- dev
      Evaluations_LOO[counter,3] <- e@auc
      Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
      Evaluations_LOO[counter,5] <- e@TPR[ind]
      Evaluations_LOO[counter,6] <- e@TNR[ind]
      Evaluations_LOO[counter,7] <- test[1,2]
      Evaluations_LOO[counter,8] <- test[2,2]
      Evaluations_LOO[counter,9] <- nrow(DataInput_test)
      Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)
      counter=counter+1
    }
  }
  return(Evaluations_LOO)}
#5.leave space out cross validation ####
LSO_eval_heather <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response,lme,longh){
  
  lme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
    filter(LME_NAME=="East Bering Sea"|LME_NAME=="Gulf of Alaska"|LME_NAME=="California Current"|LME_NAME=="Aleutian Islands"|LME_NAME=="Gulf of California"|LME_NAME=="Insular Pacific-Hawaiian")
  longh=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/Longhurst_Biogeographical_Provinces-shp/Longhurst_Biogeographical_Provinces.shp") %>% 
    filter(ProvCode=="NPPF"|ProvCode=="PSAE"|ProvCode=="NPTG")
  
  DataInput=DataInput %>% mutate(llat=lat,llon=lon) %>% mutate(lon180=case_when(lon > 180 ~ -360 + lon,
                                                                                lon<=180 ~ lon))
  Evaluations_LSO=list()
  for (i in 1:(nrow(lme))){
    lme$LME_NAME[i]
    region=lme %>% filter(LME_NAME==LME_NAME[i])
    DataInput2=DataInput
    coordinates(DataInput2)=~lon180+lat
    DataInput2=st_as_sf(DataInput2)
    st_crs(DataInput2)=st_crs(lme)
    DataInput_test=st_intersection(DataInput2,region)%>% as.data.frame()
    DataInput_train <- st_difference(DataInput2,region)%>% as.data.frame()
    if(nrow(DataInput_test)>10 & nrow(DataInput_train)>10) {
      DataInput.lso <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                 family=family, tree.complexity=tc,
                                 learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
      preds <- predict.gbm(DataInput.lso, DataInput_test,
                           n.trees=DataInput.lso$gbm.call$best.trees, type="response")
      position=grep(response,names(DataInput))
      # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
      null <- DataInput.lso$self.statistics$null.deviance
      res <- DataInput.lso$self.statistics$resid.deviance
      dev=((null - res)/null)*100 
      d <- cbind(DataInput_test[[response]], preds)
      test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
      pres <- d[d[,1]==1,2]
      abs <- d[d[,1]==0,2]
      if(length(pres)>0 & length(abs)>0){
        e <- evaluate(p=pres, a=abs)
        thresh=threshold(e)$equal_sens_spec
        ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
        
        df_lso=data.frame("Area"=as.character(lme$LME_NAME[i]),
                          "Deviance"=as.numeric(dev),
                          "AUC"=as.numeric(e@auc),
                          "TSS"=as.numeric(max(e@TPR + e@TNR-1)),
                          "TPR"=as.numeric(e@TPR[ind]),
                          "TNR"=as.numeric(e@TNR[ind]),
                          "AveragePredAbs"=as.numeric(test[1,2]),
                          "AveragePredPres"=as.numeric(test[2,2]),
                          "N.test points"=as.numeric(nrow(DataInput_test)),
                          "% left out"=as.numeric(nrow(DataInput_test)/nrow(DataInput)),
                          stringsAsFactors = F)
        
        Evaluations_LSO[[length(Evaluations_LSO)+1]]=df_lso
      }
    }
  }
  for (i in 1:(nrow(longh))){
    longh$ProvCode[i]
    region=longh %>% filter(ProvCode==ProvCode[i])
    DataInput2=DataInput
    coordinates(DataInput2)=~lon180+lat
    DataInput2=st_as_sf(DataInput2)
    st_crs(DataInput2)=st_crs(longh)
    DataInput_test=st_intersection(DataInput2,region)%>% as.data.frame()
    DataInput_train <- st_difference(DataInput2,region)%>% as.data.frame()
    if(nrow(DataInput_test)>0 & nrow(DataInput_train)>0) {
      DataInput.lso <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                 family=family, tree.complexity=tc,
                                 learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
      preds <- predict.gbm(DataInput.lso, DataInput_test,
                           n.trees=DataInput.lso$gbm.call$best.trees, type="response")
      position=grep(response,names(DataInput))
      # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
      null <- DataInput.lso$self.statistics$null.deviance
      res <- DataInput.lso$self.statistics$resid.deviance
      dev=((null - res)/null)*100 
      d <- cbind(DataInput_test[[response]], preds)
      test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
      pres <- d[d[,1]==1,2]
      abs <- d[d[,1]==0,2]
      if(length(pres)>0 & length(abs)>0){
        e <- evaluate(p=pres, a=abs)
        thresh=threshold(e)$equal_sens_spec
        ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
        
        df_lso=data.frame("Area"=as.character(longh$ProvCode[i]),
                          "Deviance"=as.numeric(dev),
                          "AUC"=as.numeric(e@auc),
                          "TSS"=as.numeric(max(e@TPR + e@TNR-1)),
                          "TPR"=as.numeric(e@TPR[ind]),
                          "TNR"=as.numeric(e@TNR[ind]),
                          "AveragePredAbs"=as.numeric(test[1,2]),
                          "AveragePredPres"=as.numeric(test[2,2]),
                          "N.test points"=as.numeric(nrow(DataInput_test)),
                          "% left out"=as.numeric(nrow(DataInput_test)/nrow(DataInput)),
                          stringsAsFactors = F)
        
        Evaluations_LSO[[length(Evaluations_LSO)+1]]=df_lso
      }
    }
  }
  if(length(Evaluations_LSO)==0){
    Evaluations_LSO=data.frame("Area"=as.character("Didn't work"),
                               "Deviance"=as.numeric(0),
                               "AUC"=as.numeric(0),
                               "TSS"=as.numeric(0),
                               "TPR"=as.numeric(0),
                               "TNR"=as.numeric(0),
                               "AveragePredAbs"=as.numeric(0),
                               "AveragePredPres"=as.numeric(0),
                               "N.test points"=as.numeric(0),
                               "% left out"=as.numeric(0),
                               stringsAsFactors = F)
  } else {Evaluations_LSO=do.call("rbind",Evaluations_LSO)}
  # Evaluations_LSO=do.call("rbind",Evaluations_LSO)
  return(Evaluations_LSO)
}
#6.leave space out cross validation ####
SBS_eval_heather <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response,lme,longh){
  
  lme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/LME66/LMEs66.shp") %>% 
    filter(LME_NAME=="East Bering Sea"|LME_NAME=="Gulf of Alaska"|LME_NAME=="California Current"|LME_NAME=="Aleutian Islands"|LME_NAME=="Gulf of California"|LME_NAME=="Insular Pacific-Hawaiian")
  longh=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/Longhurst_Biogeographical_Provinces-shp/Longhurst_Biogeographical_Provinces.shp") %>% 
    filter(ProvCode=="NPPF"|ProvCode=="PSAE"|ProvCode=="NPTG")
  
  DataInput=DataInput %>% mutate(llat=lat,llon=lon) %>% mutate(lon180=case_when(lon > 180 ~ -360 + lon,
                                                                                lon<=180 ~ lon))
  DataInput.lso <- gbm.fixed(data=DataInput, gbm.x= gbm.x, gbm.y = gbm.y, 
                             family=family, tree.complexity=tc,
                             learning.rate = lr, bag.fraction = 0.6,n.trees = 2000)
  Evaluations_LSO=list()
  for (i in 1:(nrow(lme))){
    lme$LME_NAME[i]
    region=lme %>% filter(LME_NAME==LME_NAME[i])
    DataInput2=DataInput
    coordinates(DataInput2)=~lon180+lat
    DataInput2=st_as_sf(DataInput2)
    st_crs(DataInput2)=st_crs(lme)
    DataInput_test=st_intersection(DataInput2,region)%>% as.data.frame()
    # DataInput_train <- st_difference(DataInput2,region)%>% as.data.frame()
    if(nrow(DataInput_test)>0) {
      # DataInput.lso <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
      #                            family=family, tree.complexity=tc,
      #                            learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
      preds <- predict.gbm(DataInput.lso, DataInput_test,
                           n.trees=DataInput.lso$gbm.call$best.trees, type="response")
      position=grep(response,names(DataInput))
      # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
      null <- DataInput.lso$self.statistics$null.deviance
      res <- DataInput.lso$self.statistics$resid.deviance
      dev=((null - res)/null)*100 
      d <- cbind(DataInput_test[[response]], preds)
      test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
      pres <- d[d[,1]==1,2]
      abs <- d[d[,1]==0,2]
      if(length(pres)>0 & length(abs)>0){
        e <- evaluate(p=pres, a=abs)
        thresh=threshold(e)$equal_sens_spec
        ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
        
        df_lso=data.frame("Area"=as.character(lme$LME_NAME[i]),
                          "Deviance"=as.numeric(dev),
                          "AUC"=as.numeric(e@auc),
                          "TSS"=as.numeric(max(e@TPR + e@TNR-1)),
                          "TPR"=as.numeric(e@TPR[ind]),
                          "TNR"=as.numeric(e@TNR[ind]),
                          "AveragePredAbs"=as.numeric(test[1,2]),
                          "AveragePredPres"=as.numeric(test[2,2]),
                          "N.test points"=as.numeric(nrow(DataInput_test)),
                          "% left out"=as.numeric(nrow(DataInput_test)/nrow(DataInput)),
                          stringsAsFactors = F)
        
        Evaluations_LSO[[length(Evaluations_LSO)+1]]=df_lso
      }
    }
  }
  for (i in 1:(nrow(longh))){
    longh$ProvCode[i]
    region=longh %>% filter(ProvCode==ProvCode[i])
    DataInput2=DataInput
    coordinates(DataInput2)=~lon180+lat
    DataInput2=st_as_sf(DataInput2)
    st_crs(DataInput2)=st_crs(longh)
    DataInput_test=st_intersection(DataInput2,region)%>% as.data.frame()
    # DataInput_train <- st_difference(DataInput2,region)%>% as.data.frame()
    if(nrow(DataInput_test)>0) {
      # DataInput.lso <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
      #                            family=family, tree.complexity=tc,
      #                            learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 )
      preds <- predict.gbm(DataInput.lso, DataInput_test,
                           n.trees=DataInput.lso$gbm.call$best.trees, type="response")
      position=grep(response,names(DataInput))
      # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
      null <- DataInput.lso$self.statistics$null.deviance
      res <- DataInput.lso$self.statistics$resid.deviance
      dev=((null - res)/null)*100 
      d <- cbind(DataInput_test[[response]], preds)
      test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
      pres <- d[d[,1]==1,2]
      abs <- d[d[,1]==0,2]
      if(length(pres)>0 & length(abs)>0){
        e <- evaluate(p=pres, a=abs)
        thresh=threshold(e)$equal_sens_spec
        ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
        
        df_lso=data.frame("Area"=as.character(longh$ProvCode[i]),
                          "Deviance"=as.numeric(dev),
                          "AUC"=as.numeric(e@auc),
                          "TSS"=as.numeric(max(e@TPR + e@TNR-1)),
                          "TPR"=as.numeric(e@TPR[ind]),
                          "TNR"=as.numeric(e@TNR[ind]),
                          "AveragePredAbs"=as.numeric(test[1,2]),
                          "AveragePredPres"=as.numeric(test[2,2]),
                          "N.test points"=as.numeric(nrow(DataInput_test)),
                          "% left out"=as.numeric(nrow(DataInput_test)/nrow(DataInput)),
                          stringsAsFactors = F)
        
        Evaluations_LSO[[length(Evaluations_LSO)+1]]=df_lso
      }
    }
  }
  if(length(Evaluations_LSO)==0){
    Evaluations_LSO=data.frame("Area"=as.character("Didn't work"),
                               "Deviance"=as.numeric(0),
                               "AUC"=as.numeric(0),
                               "TSS"=as.numeric(0),
                               "TPR"=as.numeric(0),
                               "TNR"=as.numeric(0),
                               "AveragePredAbs"=as.numeric(0),
                               "AveragePredPres"=as.numeric(0),
                               "N.test points"=as.numeric(0),
                               "% left out"=as.numeric(0),
                               stringsAsFactors = F)
  } else {Evaluations_LSO=do.call("rbind",Evaluations_LSO)}
  # Evaluations_LSO=do.call("rbind",Evaluations_LSO)
  return(Evaluations_LSO)
}
#7. SOO cross validation ####
SOO_eval_validation_heather <- function(DataInput,mod, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response){
  DataInput=DataInput %>% mutate(Year=year(as.Date(dt)))
  DataInput.loo <- mod
  if(family=="bernoulli"){
    Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
    colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
  }
  counter=1
  for (y in min(DataInput$Year):max(DataInput$Year)){
    print(y)
    DataInput_test <- DataInput[DataInput$Year==y,]
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=DataInput.loo$gbm.call$best.trees, type="response")
    position=grep(response,names(DataInput))
    null <- DataInput.loo$self.statistics$null.deviance
    res <- DataInput.loo$self.statistics$resid.deviance
    dev=((null - res)/null)*100 
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    if(length(pres)>0 & length(abs)>0){
      e <- dismo::evaluate(p=pres, a=abs)
      thresh=dismo::threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- dev
      Evaluations_LOO[counter,3] <- e@auc
      Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
      Evaluations_LOO[counter,5] <- e@TPR[ind]
      Evaluations_LOO[counter,6] <- e@TNR[ind]
      Evaluations_LOO[counter,7] <- test[1,2]
      Evaluations_LOO[counter,8] <- test[2,2]
      Evaluations_LOO[counter,9] <- nrow(DataInput_test)
      Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)
      counter=counter+1
    }
  }
  return(Evaluations_LOO)}

#8. Explained deviance ####
dev_eval3=function(model_object){
  null <- model_object$self.statistics$null.deviance
  res <- model_object$self.statistics$resid.deviance
  dev=((null - res)/null)*100 
  return(dev)
}

#8. Ratio of observed to predicted values ####
ratio <- function(dataInput,model_object,response){
  ## Predict on model data using the best tree for predicting
  BRTpred <- predict.gbm(model_object, dataInput, n.trees = model_object$gbm.call$best.trees, "response")
  # calculate ratio of observed to predicted values for study area
  position=grep(response,names(dataInput))
  ratio.BRTpred <- sum(dataInput[position])/sum(BRTpred)
  return(ratio.BRTpred)
}



