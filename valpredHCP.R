rm(list=ls())

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
names(TRx_1Mo)

# drop unneeded products
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Anticoagulants Orals","Bronchial","Diabetes",
                                            "Komboglyze",
                                            "Jardiance excl combos","Trajenta excl combos",
                                            "Spiriva Handihaler","Spiriva Respimat")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# look-up table for Comp group
lookup<-data.frame(PRODUCT=levels(TRx_1Mo$PRODUCT),Group=c("RESP_1","CV_1","DIA_2","RESP_1",
                                                           "DIA_2","DIA_1","DIA_1","DIA_2",
                                                           "DIA_1","DIA_1","CV_1","RESP_2",
                                                           "RESP_2","DIA_1","RESP_2","RESP_1",
                                                           "CV_1"))
TRx_1Mo<-merge(TRx_1Mo,lookup,by="PRODUCT")
groups<-levels(TRx_1Mo$Group)

# drop fields
TRx_1Mo$PRODUCT<-NULL
TRx_1Mo$NRX_SCRIPTS<-NULL
TRx_1Mo$TRX_UNITS<-NULL
TRx_1Mo$PRODUCT_CODE<-NULL
TRx_1Mo$FINDER_ID<-NULL

# average scripts per month by year
TRx_1Mo$PERIOD <- as.Date(paste(TRx_1Mo$PERIOD,"01",sep=""),format="%Y%m%d")
TRx_1Mo$Year<-format(TRx_1Mo$PERIOD,format="%Y")
sales<-aggregate(TRx_1Mo$TRX_SCRIPTS,
                 by=list(TRx_1Mo$ONEKEY_ID,TRx_1Mo$Group,TRx_1Mo$Year),mean,na.rm=TRUE)
names(sales)<-c("ONEKEY_ID","Group","Year","TRX_SCRIPTS")

# value category by group by year
sales$cat<-numeric(length(sales$TRX_SCRIPTS))
sales$cat[sales$TRX_SCRIPTS<5]<-1 
sales$cat[sales$TRX_SCRIPTS>=5&sales$TRX_SCRIPTS<15]<-2 
sales$cat[sales$TRX_SCRIPTS>=15&sales$TRX_SCRIPTS<25]<-3 
sales$cat[sales$TRX_SCRIPTS>=25&sales$TRX_SCRIPTS<50]<-4 
sales$cat[sales$TRX_SCRIPTS>=50]<-5 
sales$cat<-factor(sales$cat,levels=0:5,ordered=TRUE)

# transition matrix
# Function to calculate first-order Markov transition matrix.
# Each *row* corresponds to a single run of the Markov chain
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

# for each group, construct a matrix where each row is a run of a Markov chain over the years
ng<-length(groups)
for(groupi in groups){
  salesi<-sales[sales$Group==groupi,c("ONEKEY_ID","Year","cat")]
  years<-unique(salesi$Year)
  ny<-length(years)
  dat <- salesi[salesi$Year==years[1],c("ONEKEY_ID","cat")]
  names(dat) <- c("ONEKEY_ID",years[1])
  for (i in 2:ny) {
    newdat <- salesi[salesi$Year==years[i],c("ONEKEY_ID","cat")]
    names(newdat) <- c("ONEKEY_ID",years[i])
    dat <- merge(dat,newdat,by="ONEKEY_ID",all.x=TRUE)
  }
  dat[is.na(dat)]<-0
  print(round(trans.matrix(as.matrix(dat[,-1])),digits=2),digits=2)
}  

# use 2016 value category for prediction task
sales2016<-sales[sales$Year=="2016",c("ONEKEY_ID","Group","cat")]

# variables for prediction
discrete_vars <- c("PRIMARY_SPECIALTY","HCP_TYPE","HCP_CLASSIFICATION","SCI_EXPERT",
                   "GP_SPECIALISTS","GENDER","SECONDARY_SPECIALTY",
                   "PRIMARY_LANGUAGE")

# Demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                             "factor","factor","factor","character",
                                                             "character","character","factor",
                                                             "factor","factor","factor","numeric",
                                                             "factor","factor","factor","factor",
                                                             "factor"))
# subset
demographics <- demographics[demographics$GRADUATION_DATE<=2016&
                               !is.na(demographics$ONEKEY_ID)&
                               demographics$GENDER!='U',]
demographics <- droplevels(demographics)

# drop redundant fields
demographics$ACCOUNT_TYPE <- NULL
demographics$NAME <- NULL
demographics$ACCOUNT_STATUS <- NULL
demographics$LASTNAME <- NULL
demographics$FIRSTNAME <- NULL
demographics$MIDNAME <- NULL
demographics$FINDER_ID <- NULL
demographics$PRIMARY_LANGUAGE[demographics$PRIMARY_LANGUAGE=="English"] <- "en"
demographics <- droplevels(demographics)
demographics$MD_CONNECT <- NULL

# merge everything
dat <- merge(demographics, sales2016, by="ONEKEY_ID")

dat$years <- 2016-dat$GRADUATION_DATE
dat$GRADUATION_DATE <- NULL

# OHE
for (var in discrete_vars) {
  dat <- cbind(dat,model.matrix(as.formula(paste("~",var,"-1")),dat))
  dat[,var] <- NULL
}
dat$SECONDARY_SPECIALTY <- NULL

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
names(income) <- c("FSA","Income")
dat <- merge(dat,income,by="FSA")
dat$Income[is.na(dat$Income)] <- mean(dat$Income[!is.na(dat$Income)])
dat$Income <- dat$Income/max(dat$Income) 

# age
age <- census[census$Attribute=="Average age of the population",]
age$Attribute <- NULL
names(age) <- c("FSA","Age")
dat <- merge(dat,age,by="FSA")
dat$Age[is.na(dat$Age)] <- mean(dat$Age[!is.na(dat$Age)])
dat$Age <- dat$Age/max(dat$Age) 

# Population, 2016
pop <- census[census$Attribute=="Population, 2016",]
pop$Attribute <- NULL
names(pop) <- c("FSA","Pop")
dat <- merge(dat,pop,by="FSA")
dat$Pop[is.na(dat$Pop)] <- mean(dat$Pop[!is.na(dat$Pop)])
dat$Pop <- dat$Pop/max(dat$Pop) 

# prediction
dat$FSA<-NULL
dat$PROVINCE<-NULL
predvars <- setdiff(names(dat),c("ONEKEY_ID","Group","cat"))
(cnts<-aggregate(dat$ONEKEY_ID,by=list(dat$Group),length))
names(cnts)<-c("Group","N")
cnts$ntrain<-floor(cnts$N*0.9)
cnts$ntest<-cnts$N-cnts$ntrain
cnts

library(ordinal)
library(caret)
library(bmrm)

lambdas<-exp(-2:2)
nlambda<-length(lambdas)
accuracy<-list(CLM=matrix(NA,nrow=1,ncol=ng,dimnames=list("CLM",groups)),
               NRBM=matrix(NA,nrow=nlambda,ncol=ng,dimnames=list(lambdas,groups)))

for(groupi in groups){
  dat1<-dat[dat$Group==groupi,]
  
  # training/test sets
  ntrain<-cnts[cnts$Group==groupi,"ntrain"]
  ntest<-cnts[cnts$Group==groupi,"ntest"]
  idx<-sample(ntrain+ntest,ntrain+ntest)
  train<-dat1[idx[1:ntrain],]
  test<-dat1[idx[(ntrain+1):(ntrain+ntest)],]
  
  # training/val sets
  ntrain1<-floor(ntrain*0.9)
  nval1<-ntrain-ntrain1
  idx1<-sample(ntrain1+nval1,ntrain1+nval1)
  train1<-train[idx1[1:ntrain1],]
  val1<-train[idx1[(ntrain1+1):(ntrain1+nval1)],]
  
  # ordinal regression
  form1<-as.formula(paste("cat",paste("`",predvars,"`",sep="",collapse="+"),sep="~"))
  wgts<-numeric(ntrain1)
  for(i in unique(train1$cat)){
    wgts[train1$cat==i]<-ntrain1/sum(train1$cat==i)
  }
  clm1<-clm(form1,data=train1,weights=wgts)
  summary(clm1)
  
  # prediction
  pred<-factor(apply(predict(clm1,val1[,predvars])$fit,1,which.max),levels=0:5)
  cM<-confusionMatrix(val1$cat,pred)
  accuracy$CLM["CLM",groupi]<-cM$overall["Accuracy"]

  # cost matrix for SVM
  cats<-unique(train1$cat)
  nc<-length(cats)
  cost<-matrix(NA,nc,nc)
  for(i in 1:nc){
    for(j in 1:nc){
      cost[i,j]<-sum(train1$cat==cats[i])/sum(train1$cat==cats[j])
    }
  }

  # SVM
  for(lambdai in 1:nlambda){
    x<-cbind(1,as.matrix(train1[,predvars]))
    y<-as.numeric(as.character(train1$cat))
    nrbm1<-nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),LAMBDA=lambdas[lambdai])
    xv<-cbind(1,as.matrix(val1[,predvars]))
    pred<-factor(round(xv %*% nrbm1),levels=0:5)
    cM<-confusionMatrix(val1$cat,pred)
    accuracy$NRBM[lambdai,groupi]<-cM$overall["Accuracy"]
  }
}
round(accuracy$CLM,2)
round(accuracy$NRBM,2)
