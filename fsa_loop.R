# LOO-RMSE by product by year
rm(list=ls())
#library(penalized)

max_engagements <- 20
products <- c("SPIRIVA","JARDIANCE","PRADAXA")
years <- as.character(2013:2016)
rmse.lm <- matrix(NA,nrow=length(products),ncol=length(years),dimnames=list(products,years))
#rmse.pen <- matrix(NA,nrow=length(products),ncol=length(years),dimnames=list(products,years))
meansales <- matrix(0,nrow=length(products),ncol=length(years),dimnames=list(products,years))
n <- matrix(0,nrow=length(products),ncol=length(years),dimnames=list(products,years))

# CVM_Info
cvm_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_CVM_Info.txt",
                       header=TRUE, sep="|",colClasses=c("factor","numeric","factor","factor","factor",
                                                         "factor"))
# Demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                             "factor","factor","factor","character",
                                                             "character","character","factor",
                                                             "factor","factor","factor","numeric",
                                                             "factor","factor","factor","factor",
                                                             "factor"))

demographics <- demographics[demographics$HCP_TYPE=="Physician"&!is.na(demographics$ONEKEY_ID)&
                               demographics$HCP_CLASSIFICATION=="Physician"&
                               demographics$SCI_EXPERT=="No"&demographics$GENDER!='U',]
demographics$ACCOUNT_TYPE <- NULL
demographics$NAME <- NULL
demographics$HCP_TYPE <- NULL
demographics$HCP_CLASSIFICATION <- NULL
demographics$ACCOUNT_STATUS <- NULL
demographics$LASTNAME <- NULL
demographics$FIRSTNAME <- NULL
demographics$MIDNAME <- NULL
demographics$SCI_EXPERT <- NULL

# Engagement_Info
engagement_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Info.txt",
                              header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor",
                                                                "factor","Date","factor","factor",
                                                                "factor","factor","factor",
                                                                "factor","factor","factor","factor",
                                                                "factor"))
# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
discrete_vars <- c("CVM","STSEG","PLANNED_DETAILS","PRIMARY_SPECIALTY","PROVINCE","GENDER",
                   "PRIMARY_LANGUAGE")

for (product in products) {
  for (year in years) {
    cvm_info1 <- cvm_info[cvm_info$CVM_YEAR==as.numeric(year)&cvm_info$PRODUCT==product,]
    if (prod(dim(cvm_info1))==0) 
      next
    cvm_info1$CVM_YEAR <- NULL
    cvm_info1$PRODUCT <- NULL
    demographics1 <- demographics[demographics$GRADUATION_DATE<=as.numeric(year),]    
    # merge CVM_Info & Demographics
    dat <- merge(cvm_info1,demographics1,by="ONEKEY_ID")
    # FSA2
    dat$FSA2 <- factor(substr(dat$FSA,1,2))
    # discrete features for prediction
    dat <- dat[,c("ONEKEY_ID","CVM","STSEG","PLANNED_DETAILS","PRIMARY_SPECIALTY","PROVINCE",
                  "GRADUATION_DATE","GP_SPECIALISTS","GENDER","SECONDARY_SPECIALTY",
                  "PRIMARY_LANGUAGE","FSA2")]
    dat <- droplevels(dat)
    engagement_info1 <- engagement_info[format(engagement_info$CALL_DATE,"%Y")==year&
                                         engagement_info$PRODUCT_DETAILED==product,
                                       c("ONEKEY_ID","DETAIL_PRIORITY")]
    if (prod(dim(engagement_info1))==0) 
      next
    engagements <- aggregate(engagement_info1$ONEKEY_ID,list(engagement_info1$ONEKEY_ID),length)
    names(engagements) <- c("ONEKEY_ID","N")
    engagements <- engagements[engagements$N<max_engagements,]
    
    productcc <- paste(substr(product,1,1),tolower(substring(product,2)),sep="")
    TRx_1Mo1 <- TRx_1Mo[substr(TRx_1Mo$PRODUCT,1,6)==substr(productcc,1,6),
                       c("ONEKEY_ID","PERIOD","TRX_UNITS")]
    TRx_1Mo1$quarter <- as.numeric(paste(substr(as.character(TRx_1Mo1$PERIOD),1,4),
          ifelse(ceiling(as.numeric(substr(as.character(TRx_1Mo1$PERIOD),5,6))/3.0)*3<10,"0",""),
                    ceiling(as.numeric(substr(as.character(TRx_1Mo1$PERIOD),5,6))/3.0)*3,sep=""))
    TRx_3Mo <- aggregate(TRx_1Mo1$TRX_UNITS,list(TRx_1Mo1$ONEKEY_ID,TRx_1Mo1$quarter),sum)
    names(TRx_3Mo) <- c("ONEKEY_ID","quarter","TRX_UNITS")
    nextperiod <- format(tail(seq(as.Date(paste(year,12,31,sep="-"),format="%Y-%m-%d"),
                                  by="month",length=4),1),"%Y%m")
    TRx_3Mo <- TRx_3Mo[TRx_3Mo$quarter==nextperiod,c("ONEKEY_ID","TRX_UNITS")]
    
    # merge everything
    dat <- merge(dat, engagements, by="ONEKEY_ID")
    dat <- merge(dat, TRx_3Mo, by="ONEKEY_ID")
    
    dat <- dat[dat$PRIMARY_SPECIALTY %in% c("Family Medicine","General Practice") &
                 dat$GP_SPECIALISTS %in% "GP/FM" & trimws(dat$SECONDARY_SPECIALTY)=="",]
    dat$GP_SPECIALISTS <- NULL
    dat$SECONDARY_SPECIALTY <- NULL
    dat <- droplevels(dat)

    dat$years <- as.numeric(year)-dat$GRADUATION_DATE
    dat$GRADUATION_DATE <- NULL
    
    # OHE
    for (var in discrete_vars) 
      if (length(levels(dat[,var]))>1)
        dat <- cbind(dat,model.matrix(as.formula(paste("~",var,"-1")),dat))
    dat$CVM <- NULL
    dat$STSEG <- NULL
    dat$PLANNED_DETAILS <- NULL
    dat$PLANNED_DETAILS <- NULL
    dat$PRIMARY_SPECIALTY <- NULL
    dat$PROVINCE <- NULL
    dat$PROVINCE <- NULL
    dat$GENDER <- NULL
    dat$PRIMARY_LANGUAGE <- NULL
    
    d <- dim(dat)[2]
    form <- as.formula(paste(paste("TRX_UNITS ~",
                                   paste("N","years",
                            paste("`",names(dat)[6:d],"`",sep="",collapse="+"),sep="+")),"-1"))
    # Aggregate to FSA2
    dat_agg <- aggregate(dat[,c("TRX_UNITS","N","years",names(dat)[6:d])],list(dat$FSA2),mean)

    # LOO-RMSE 
    ld <- dim(dat_agg); l <- ld[1]; d <- ld[2]
    predicted.lm <- numeric(l)
    #predicted.pen <- numeric(l)
    for (i in 1:l) { 
      predicted.lm[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
      #predicted.pen[i] <- predict(penalized(form,lambda1=10,data=dat_agg[-i,],standardize=TRUE),
      #                        dat_agg[i,3:d])
    }
    rmse.lm[product,year] <- sqrt(mean((predicted.lm-dat_agg$TRX_UNITS)^2))
    #rmse.pen[product,year] <- sqrt(mean((predicted.pen-dat_agg$TRX_UNITS)^2))
    meansales[product,year] <- mean(dat_agg$TRX_UNITS)
    n[product,year] <- l
  }
}
print(rmse.lm/meansales)
#print(rmse.pen/meansales)
print(n)