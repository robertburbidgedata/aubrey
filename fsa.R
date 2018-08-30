### merge CVM_Info and Demographics and form FSA bricks
rm(list=ls())
# SPIRIVA, 2013
product <- "SPIRIVA"
year <- 2013

# CVM_Info
cvm_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_CVM_Info.txt",
                    header=TRUE, sep="|",colClasses=c("factor","numeric","factor","factor","factor",
                                                      "factor"))
print(dim(cvm_info))
names(cvm_info)
summary(cvm_info)

cvm_info <- cvm_info[cvm_info$CVM_YEAR==year&cvm_info$PRODUCT==product,]
cvm_info$CVM_YEAR <- NULL
cvm_info$PRODUCT <- NULL
print(dim(cvm_info))
summary(cvm_info)

# Demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                       header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                         "factor","factor","factor","character",
                                                         "character","character","factor",
                                                         "factor","factor","factor","numeric",
                                                         "factor","factor","factor","factor",
                                                         "factor"))
print(dim(demographics))
names(demographics)
summary(demographics)

demographics <- demographics[demographics$GRADUATION_DATE<=year&
                               demographics$HCP_TYPE=="Physician"&!is.na(demographics$ONEKEY_ID)&
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
print(dim(demographics))
summary(demographics)

# merge CVM_Info & Demographics
dat <- merge(cvm_info,demographics,by="ONEKEY_ID")
print(dim(dat))
summary(dat)

# FSA sizes
fsa_sizes <- aggregate(dat$ONEKEY_ID,list(dat$FSA),length)
summary(fsa_sizes$x)
hist(fsa_sizes$x,breaks=35)

# FSA2
dat$FSA2 <- factor(substr(dat$FSA,1,2))
fsa2_sizes <- aggregate(dat$ONEKEY_ID,list(dat$FSA2),length)
hist(fsa2_sizes$x,breaks=30)
summary(fsa2_sizes$x)

# discrete features for prediction
dat <- dat[,c("ONEKEY_ID","CVM","STSEG","PLANNED_DETAILS","PRIMARY_SPECIALTY","PROVINCE",
             "GRADUATION_DATE","GP_SPECIALISTS","GENDER","SECONDARY_SPECIALTY",
             "PRIMARY_LANGUAGE","FSA2")]
dat <- droplevels(dat)
summary(dat)

# Engagement_Info
engagement_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Info.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor",
                                                             "factor","Date","factor","factor",
                                                             "factor","factor","factor",
                                                             "factor","factor","factor","factor",
                                                             "factor"))
names(engagement_info)
print(dim(engagement_info))
summary(engagement_info)
engagement_info <- engagement_info[format(engagement_info$CALL_DATE,"%Y")==year&
                                     engagement_info$PRODUCT_DETAILED==product,
                                   c("ONEKEY_ID","DETAIL_PRIORITY")]
engagements <- aggregate(engagement_info$ONEKEY_ID,list(engagement_info$ONEKEY_ID),length)
names(engagements) <- c("ONEKEY_ID","N")
hist(engagements$N,breaks=20,xlim=c(0,20))
engagements <- engagements[engagements$N<20,]
summary(engagements$N)

# Engagement_Samples
engagement_samples <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Samples.txt",
                              header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor",
                                                                "Date","factor","factor",
                                                                "factor","factor","factor",
                                                                "factor","factor","factor","factor",
                                                                "numeric"))
names(engagement_samples)
print(dim(engagement_samples))
summary(engagement_samples)
engagement_samples <- engagement_samples[format(engagement_samples$CALL_DATE,"%Y")==year&
                                     substr(engagement_samples$PRODUCT_SAMPLED,1,nchar(product))
                                            ==product,
                                   c("ONEKEY_ID","SAMPLE_QUANTITY")]
samples <- aggregate(engagement_samples$SAMPLE_QUANTITY,list(engagement_samples$ONEKEY_ID),sum)
names(samples) <- c("ONEKEY_ID","SAMPLE_QUANTITY")
hist(samples$SAMPLE_QUANTITY,breaks=20,xlim=c(0,200))
samples <- samples[samples$SAMPLE_QUANTITY<50,]
summary(samples$SAMPLE_QUANTITY)

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                              header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                                 "numeric","numeric","numeric",
                                                                 "factor","factor"))
print(dim(TRx_1Mo))
names(TRx_1Mo)
summary(TRx_1Mo)
productcc <- paste(substr(product,1,1),tolower(substring(product,2)),sep="")
TRx_1Mo <- TRx_1Mo[TRx_1Mo$PRODUCT==productcc,
                   c("ONEKEY_ID","PERIOD","TRX_UNITS")]
TRx_1Mo$quarter <- as.numeric(paste(substr(as.character(TRx_1Mo$PERIOD),1,4),
      ifelse(ceiling(as.numeric(substr(as.character(TRx_1Mo$PERIOD),5,6))/3.0)*3<10,"0",""),
      ceiling(as.numeric(substr(as.character(TRx_1Mo$PERIOD),5,6))/3.0)*3,sep=""))
TRx_3Mo <- aggregate(TRx_1Mo$TRX_UNITS,list(TRx_1Mo$ONEKEY_ID,TRx_1Mo$quarter),sum)
names(TRx_3Mo) <- c("ONEKEY_ID","quarter","TRX_UNITS")
nextperiod <- format(tail(seq(as.Date(paste(year,12,31,sep="-"),format="%Y-%m-%d"),
                              by="month",length=4),1),"%Y%m")
TRx_3Mo <- TRx_3Mo[TRx_3Mo$quarter==nextperiod,c("ONEKEY_ID","TRX_UNITS")]

# merge everything
dat <- merge(dat, engagements, by="ONEKEY_ID")
dat <- merge(dat, samples, by="ONEKEY_ID")
dat <- merge(dat, TRx_3Mo, by="ONEKEY_ID")

# pre-OHE encoding
summary(dat)
print(length(levels(dat$FSA2)))
dat <- dat[dat$PRIMARY_SPECIALTY %in% c("Family Medicine","General Practice") &
           dat$GP_SPECIALISTS %in% "GP/FM" & trimws(dat$SECONDARY_SPECIALTY)=="",]

dat <- droplevels(dat)
summary(dat)
print(length(levels(dat$FSA2)))

dat$GP_SPECIALISTS <- NULL
dat$SECONDARY_SPECIALTY <- NULL

# barcharts for discrete variables
names(dat)
discrete_vars <- c("CVM","STSEG","PLANNED_DETAILS","PRIMARY_SPECIALTY","PROVINCE","GENDER",
                   "PRIMARY_LANGUAGE")
for (var in discrete_vars) {
  barplot(table(dat[,var]),xlab="",ylab="Count",main=paste("No. HCPs by",var),
        sub=paste("CA/Aubrey", year, product))
  barplot(xtabs(as.formula(paste("TRX_UNITS ~",var)), dat),xlab="",ylab="TRX_UNITS",
          main=paste("Sales by",var),
          sub=paste("CA/Aubrey", year, product))
  barplot(xtabs(as.formula(paste("TRX_UNITS ~",var)), dat)/table(dat[,var]),
          xlab="",ylab="TRX_UNITS per HCP",
          main=paste("Sales per HCP by",var),
          sub=paste("CA/Aubrey", year, product))
} 

# scatter plots for continuous variables
dat$years <- year-dat$GRADUATION_DATE
plot(dat$years,dat$TRX_UNITS,type="p",xlab="Years since graduation",
     ylab="TRX_UNITS",main="Sales vs Years since Graduation",sub=paste("CA/Aubrey", year, product))
abline(lm(TRX_UNITS ~ years,dat),col="red")
plot(dat$N,dat$TRX_UNITS,type="p",xlab="Number of engagements",
     ylab="TRX_UNITS",main="Sales vs No. engagements",sub=paste("CA/Aubrey", year, product))
abline(lm(TRX_UNITS ~ N,dat),col="red")
plot(dat$SAMPLE_QUANTITY,dat$TRX_UNITS,type="p",xlab="Number of samples",
     ylab="TRX_UNITS",main="Sales vs No. samples",sub=paste("CA/Aubrey", year, product))
abline(lm(TRX_UNITS ~ SAMPLE_QUANTITY,dat),col="red")

# COPD prevalence 2013
# http://www12.statcan.gc.ca/health-sante/82-213/Op2.cfm?Lang=ENG&TABID=0&LINE_ID=2675&IND=ASR&SX=TOTAL&change=no&S=9&O=A
copd1 <- data.frame(PROVINCE=c("Alberta","British Columbia","Manitoba","New Brunswick",
                              "Newfoundland","Nova Scotia","Nunavut","NWT","Ontario",
                              "Prince Edward Island","Quebec","Saskatchewan","Yukon"),
                   copd1=c(4.2,4.0,2.9,4.2,4.3,3.9,NA,3.6,4.0,4.5,4.1,2.8,3.5))
dat <- merge(dat,copd1,by="PROVINCE")
dat$copd1[is.na(dat$copd1)] <- mean(dat$copd1,na.rm=TRUE)
# NAHVSMDB57:copd.COPD_BOTH_OVER12_PRC
copd2 <- data.frame(PROVINCE=c("Alberta","British Columbia","Manitoba","New Brunswick",
                              "Newfoundland","Nova Scotia","Nunavut","NWT","Ontario",
                              "Prince Edward Island","Quebec","Saskatchewan","Yukon"),
                   copd2=c(3.8,3.6,3.6,5.2,NA,5.9,NA,NA,3.8,5.7,4.5,4.8,5.6))
dat <- merge(dat,copd2,by="PROVINCE")
dat$copd2[is.na(dat$copd2)] <- mean(dat$copd2,na.rm=TRUE)

# OHE
for (var in discrete_vars)
  dat <- cbind(dat,model.matrix(as.formula(paste("~",var,"-1")),dat))
dat$CVM <- NULL
dat$STSEG <- NULL
dat$PLANNED_DETAILS <- NULL
dat$PLANNED_DETAILS <- NULL
dat$PRIMARY_SPECIALTY <- NULL
dat$PROVINCE <- NULL
dat$PROVINCE <- NULL
dat$GRADUATION_DATE <- NULL
dat$GENDER <- NULL
dat$PRIMARY_LANGUAGE <- NULL

# exc. prevalence
form <- as.formula(paste(paste("TRX_UNITS ~",
                         paste("N","years","SAMPLE_QUANTITY",
                        paste("`",names(dat)[9:55],"`",sep="",collapse="+"),sep="+")),"-1"))
mod1 <- lm(form,dat)
summary(mod1)

# Aggregate to FSA2
dat_agg <- aggregate(dat[,3:55],list(dat$FSA2),mean)
names(dat_agg)[1] <- "FSA2"

# lm
mod2 <- lm(form,dat_agg)
summary(mod2)
plot(dat_agg$TRX_UNITS,predict(mod2,dat_agg),type="p",xlab="Actual",ylab="Predicted",
     xlim=c(0,1700),ylim=c(0,1700))
abline(0,1,col="red")
print(sqrt(mean((predict(mod2,dat_agg)-dat_agg$TRX_UNITS)^2)))

# LOO-RMSE
l <- dim(dat_agg)[1]
sse <- 0
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))
plot(dat_agg$TRX_UNITS,predicted,type="p",xlab="Actual",ylab="Predicted (LOO)",
     xlim=c(0,1700),ylim=c(0,1700))
abline(0,1,col="red")
print(mean(dat_agg$TRX_UNITS))

# ridge regression
library(penalized)
mod3 <- penalized(form,lambda1=1,data=dat_agg,standardize=TRUE)
print(sqrt(mean(residuals(mod3)^2)))

predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(penalized(form,lambda1=1,data=dat_agg[-i,],standardize=TRUE),
                          dat_agg[i,c("N","years","SAMPLE_QUANTITY",names(dat_agg)[8:54])])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))
plot(dat_agg$TRX_UNITS,predicted,type="p",xlab="Actual",ylab="Predicted (LOO)",
     xlim=c(0,1700),ylim=c(0,1700))
abline(0,1,col="red")

# inc. prevalence
for (var in names(dat)) {
  if(substring(var,1,8)=="PROVINCE"){
    dat[,var] <- NULL
    dat_agg[,var] <- NULL
  }
}
form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("N","SAMPLE_QUANTITY","years","copd1",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
mod3a <- lm(form,dat_agg)
summary(mod3a)

l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("N","SAMPLE_QUANTITY","years","copd2",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
mod3b <- lm(form,dat_agg)
summary(mod3b)

l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# census 2016
# http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/page_dl-tc.cfm?Lang=E
census <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/98-401-X2016026_English_CSV_data.csv",
                       header=TRUE, sep=",",colClasses=c("numeric","factor","factor","factor",
                                                         "numeric","factor","factor","factor",
                                                         "character","character","character",
                                                         "character","character"))
names(census) <- c("YEAR","FSA","GEO_LEVEL","GEO_NAME",
                   "GNR","DATA_QUALITY_FLAG","ALT_GEO_CODE",
                   "Attribute","Member.ID","Notes","Total","Male","Female")
census$YEAR <- NULL
census$GEO_LEVEL <- NULL
census$GEO_NAME <- NULL
census$GNR <- NULL
census$DATA_QUALITY_FLAG <- NULL
census$ALT_GEO_CODE <- NULL
census$Member.ID <- NULL
census$Notes <- NULL
census$Male <- NULL
census$Female <- NULL

census$Total <- as.numeric(ifelse(census$Total=="...","",census$Total))

# income
income <- census[census$Attribute=="Median after-tax income of households in 2015 ($)",]
income$Attribute <- NULL
hist(income$Total)

income$FSA2 <- substr(income$FSA,1,2)
income1 <- aggregate(income$Total,list(income$FSA2),mean)
names(income1) <- c("FSA2","Income")
dat_agg <- merge(dat_agg,income1,by="FSA2")
dat_agg$Income[is.na(dat_agg$Income)] <- mean(dat_agg$Income[!is.na(dat_agg$Income)])
dat_agg$Income <- dat_agg$Income/max(dat_agg$Income) 

form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("N","SAMPLE_QUANTITY","years","copd2","Income",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
mod4 <- lm(form,dat_agg)
summary(mod4)

l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# age
age <- census[census$Attribute=="Average age of the population",]
age$Attribute <- NULL
hist(age$Total)

age$FSA2 <- substr(age$FSA,1,2)
age1 <- aggregate(age$Total,list(age$FSA2),mean)
names(age1) <- c("FSA2","Age")
dat_agg <- merge(dat_agg,age1,by="FSA2")
dat_agg$Age[is.na(dat_agg$Age)] <- mean(dat_agg$Age[!is.na(dat_agg$Age)])
dat_agg$Age <- dat_agg$Age/max(dat_agg$Age) 

form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("N","SAMPLE_QUANTITY","years","copd2","Income","Age",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
mod5 <- lm(form,dat_agg)
summary(mod5)

l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# Population, 2016
pop <- census[census$Attribute=="Population, 2016",]
pop$Attribute <- NULL
hist(pop$Total)

pop$FSA2 <- substr(pop$FSA,1,2)
pop1 <- aggregate(pop$Total,list(pop$FSA2),mean)
names(pop1) <- c("FSA2","Pop")
dat_agg <- merge(dat_agg,pop1,by="FSA2")
dat_agg$Pop[is.na(dat_agg$Pop)] <- mean(dat_agg$Pop[!is.na(dat_agg$Pop)])
dat_agg$Pop <- dat_agg$Pop/max(dat_agg$Pop) 

form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("N","SAMPLE_QUANTITY","years","copd2","Income","Age","Pop",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
mod6 <- lm(form,dat_agg)
summary(mod6)

l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(lm(form,dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# glm poisson
l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(glm(form,family=poisson(link="identity"),dat_agg[-i,]),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# svm rbf
library("e1071")
l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(svm(form,dat_agg[-i,],kernel="radial"),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# svm rbf \ N, SAMPLE_QUANTITY
form <- as.formula(paste(paste("TRX_UNITS ~",
                               paste("years","copd2","Income","Age","Pop",
                                     paste("`",names(dat)[9:48],"`",sep="",collapse="+"),sep="+")),"-1"))
l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) 
  predicted[i] <- predict(svm(form,dat_agg[-i,],kernel="radial"),dat_agg[i,])
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# svm rbf \ N, SAMPLE_QUANTITY CV over C
l <- dim(dat_agg)[1]
predicted <- numeric(l)
for (i in 1:l) {
  dat_agg1 <- dat_agg[-i,]
  ncost <- 6
  rmse <- numeric(ncost)
  for (costi in 1:ncost) {
    predicted1 <- numeric(l-1)
    cost <- 10^costi
    for (j in 1:(l-1))
      predicted1[j] <- predict(svm(form,dat_agg1[-j,],kernel="radial",cost=cost),dat_agg1[j,])
    rmse[costi] <- sqrt(mean((predicted1-dat_agg1$TRX_UNITS)^2))
  }
  cost <- 10^which.min(rmse)
  print(cost)
  predicted[i] <- predict(svm(form,dat_agg[-i,],kernel="radial",cost=cost),dat_agg[i,])
}
print(sqrt(mean((predicted-dat_agg$TRX_UNITS)^2)))

# clustering
names(dat_agg)
ncenters <- 10
nstart <- 25
tot.withinss <- numeric(ncenters)
for (centers in 1:ncenters) {
  tot.withinss[centers] <- kmeans(dat_agg[,4:50],centers,nstart=nstart)$tot.withinss
}
plot(1:ncenters,tot.withinss,type="l",xlab="No. centers",ylab="Total within cluster sum of squares")

# elbow
theta <- 180-(atan(-(tot.withinss[2:ncenters]/1000-tot.withinss[1:(ncenters-1)]/1000)/
  (2:ncenters-1:(ncenters-1)))/pi*180)
kopt <- which.max(theta[2:(ncenters-1)]-theta[1:(ncenters-2)])+1
km <- kmeans(dat_agg[,4:50],kopt,nstart=nstart)
print(km$cluster)

# increase k to split cluster 1
km <- kmeans(dat_agg[,4:50],kopt+1,nstart=nstart)
print(km$cluster)
dat_agg <- cbind(dat_agg,km$cluster)

# plot in PCA space
pca <- eigen(cov(dat_agg[,4:50]))
dat_agg$pc1 <- as.matrix(dat_agg[,4:50])%*%as.matrix(pca$vectors[,1])
dat_agg$pc2 <- as.matrix(dat_agg[,4:50])%*%as.matrix(pca$vectors[,2])
sales.med <- median(dat_agg$TRX_UNITS)
xrange <- range(dat_agg$pc1)
yrange <- range(dat_agg$pc2)
i <- dat_agg$TRX_UNITS>=sales.med & dat_agg$`km$cluster`==1
plot(dat_agg$pc1[i],dat_agg$pc2[i],type="p",col="green",pch=0,xlab="PC1",ylab="PC2",
     main="k-means clustering in PC-space",xlim=xrange,ylim=yrange*1.2)
i <- dat_agg$TRX_UNITS>=sales.med & dat_agg$`km$cluster`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="green",pch=1)
i <- dat_agg$TRX_UNITS>=sales.med & dat_agg$`km$cluster`==3
points(dat_agg$pc1[i],dat_agg$pc2[i],col="green",pch=4)
i <- dat_agg$TRX_UNITS<sales.med & dat_agg$`km$cluster`==1
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=0)
i <- dat_agg$TRX_UNITS<sales.med & dat_agg$`km$cluster`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=1)
i <- dat_agg$TRX_UNITS<sales.med & dat_agg$`km$cluster`==3
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=4)
legend("topleft",c("Cluster 1","Cluster 2","Cluster 3","High sales","Low sales"),
       col=c("black","black","black","green","red"),pch=c(0,1,4,13,13),ncol=2)
