# add population per HCP
# building density data not found
# investigate significant variables and transforms
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
discrete_vars <- c("PRIMARY_SPECIALTY","SCI_EXPERT",
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
demographics$HCP_CLASSIFICATION<-NULL
demographics$HCP_TYPE<-NULL

# merge everything
dat <- merge(demographics, sales2016, by="ONEKEY_ID")

# pmf cat
barplot(table(as.numeric(as.character(dat$cat))),
        main="Histogram of Value Category (cat): 2016")
        
# years
dat$years <- 2016-dat$GRADUATION_DATE
dat$years<-dat$years/max(dat$years)
dat$GRADUATION_DATE <- NULL

# OHE (change "" to "NA" to avoid name duplication)
for (discrete_var in discrete_vars) {
  if(any(dat[,discrete_var]=="")){
    levels(dat[,discrete_var])<-c(levels(dat[,discrete_var]),"NA")
    dat[dat[,discrete_var]=="",discrete_var]<-"NA"
    dat<-droplevels(dat)
  }
  dat <- cbind(dat,model.matrix(as.formula(paste("~",discrete_var,"-1")),dat))
}

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

# Population per ONEKEY_ID
dat_agg<-aggregate(dat$ONEKEY_ID,by=list(dat$FSA),length)
names(dat_agg)<-c("FSA","nHCP")
dat<-merge(dat,dat_agg,"FSA")
dat$popperHCP<-dat$Pop/dat$nHCP
dat$popperHCP <- dat$popperHCP/max(dat$popperHCP) 

# scale population
dat$Pop <- dat$Pop/max(dat$Pop) 

# plots: continuous vars
ctsvars<-c("years","Income","Age","Pop","popperHCP")

# scatter matrix
df<-cbind(dat[,ctsvars],as.numeric(as.character(dat$cat)))
names(df)<-c(ctsvars,"cat")
pairs(df)

# pdfs
for(ctsvar in ctsvars){
  hist(df[,ctsvar],main=paste("Histogram of",ctsvar),xlab="")
}

# plots: discrete vars
for(discrete_var in discrete_vars){
  dat1<-aggregate(dat$ONEKEY_ID,list(dat[,discrete_var]),length)
  names(dat1)<-c(discrete_var,"cnt")
  dat1$cnt<-dat1$cnt/length(dat$ONEKEY_ID)
  par(mar=c(14,4,1,0)+0.1)
  barplot(dat1$cnt,names.arg=levels(dat1[,discrete_var]),las=2,log="y")
  dat[,discrete_var]<-NULL
}
par(mar=c(5,4,4,2)+0.1)

# prediction
dat$FSA<-NULL
dat$PROVINCE<-NULL
predvars <- setdiff(names(dat),c("ONEKEY_ID","Group","cat","nHCP"))
(cnts<-aggregate(dat$ONEKEY_ID,by=list(dat$Group),length))
names(cnts)<-c("Group","N")
cnts$ntrain<-floor(cnts$N*0.9)
cnts$ntest<-cnts$N-cnts$ntrain
cnts

library(ordinal)
library(caret)
library(bmrm)

lambdas<-exp(seq(-5,5,length.out=41))
nlambda<-length(lambdas)
accuracy<-list(CLM=matrix(NA,nrow=1,ncol=ng,dimnames=list("CLM",groups)),
               NRBM=matrix(NA,nrow=nlambda,ncol=ng,dimnames=list(lambdas,groups)))
ci95<-list(CLM=matrix(NA,nrow=1,ncol=ng,dimnames=list("CLM",groups)),
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
  for(i in levels(train1$cat)){
    wgts[train1$cat==i]<-ntrain1/sum(train1$cat==i)
  }
  clm1<-clm(form1,data=train1,weights=wgts)

  # prediction
  pred<-factor(apply(predict(clm1,val1[,predvars])$fit,1,which.max),levels=1:5)
  cM<-confusionMatrix(val1$cat,pred)
  accuracy$CLM["CLM",groupi]<-cM$overall["Accuracy"]
  ci95$CLM["CLM",groupi]<-(cM$overall["AccuracyUpper"]-cM$overall["AccuracyLower"])/2
  
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
    # suppress output from nrbm
    { sink("/dev/null"); nrbm1<-nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                          LAMBDA=lambdas[lambdai]); sink() }
    xv<-cbind(1,as.matrix(val1[,predvars]))
    pred<-factor(round(xv %*% nrbm1),levels=1:5)
    cM<-confusionMatrix(val1$cat,pred)
    acc<-cM$overall["Accuracy"]
    if(!is.nan(acc)){
      accuracy$NRBM[lambdai,groupi]<-acc
      ci95$NRBM[lambdai,groupi]<-(cM$overall["AccuracyUpper"]-cM$overall["AccuracyLower"])/2
    }
  }
}

# print results
data.frame(rbind(round(accuracy$CLM,2),round(apply(accuracy$NRBM,2,max,na.rm=T),2)),
           row.names=c("CLM","NRBM"))
data.frame(rbind(round(ci95$CLM,2),round(apply(ci95$NRBM,2,min,na.rm=T),2)),
           row.names=c("CLM","NRBM"))

# optimal lambdas
lambda.opt<-lambdas[apply(accuracy$NRBM,2,which.max)]

# examine weight vectors
for(i in 1:ng){
  groupi<-groups[i]
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
  
  # cost matrix for SVM
  cats<-unique(train1$cat)
  nc<-length(cats)
  cost<-matrix(NA,nc,nc)
  for(j1 in 1:nc){
    for(j2 in 1:nc){
      cost[j1,j2]<-sum(train1$cat==cats[j1])/sum(train1$cat==cats[j2])
    }
  }
  
  # SVM
  x<-cbind(1,as.matrix(train1[,predvars]))
  y<-as.numeric(as.character(train1$cat))
  # suppress output from nrbm
  { sink("/dev/null"); nrbm1<-nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                                   LAMBDA=lambda.opt[i]); sink() }
  
  # feature selection and retrain
  q3<-quantile(abs(nrbm1),0.80)
  idx<-abs(as.vector(nrbm1))>q3
  x<-x[,idx]
  # suppress output from nrbm
  { sink("/dev/null"); nrbm1<-nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                                   LAMBDA=lambda.opt[i]); sink() }
  
  # plot weights
  par(mar=c(3,20,1,0)+0.1)
  ord<-order(nrbm1,decreasing=T)
  barplot(as.vector(nrbm1)[ord],names.arg=c("Intercept",predvars)[ord],las=2,horiz=T,
          main="")
  loc=par("usr")
  text(loc[1],loc[4],
       paste("NRBM Weights, lambda= ",round(lambda.opt[i],2),": ",groups[i],sep=""),
       pos=3,xpd=T)
  
  # val acc
  xv<-cbind(1,as.matrix(val1[,predvars]))[,idx]
  pred<-factor(round(xv %*% nrbm1),levels=1:5)
  cM<-confusionMatrix(val1$cat,pred)
  print(cM$overall["Accuracy"])
  print((cM$overall["AccuracyUpper"]-cM$overall["AccuracyLower"])/2)
}

# kernel PCA
library(kernlab)
accuracy<-numeric(ng)
ci95<-numeric(ng)
for(i in 1:ng){
  groupi<-groups[i]
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
  
  kpca1 <- kpca(~.,data=train1[,predvars],kernel="rbfdot",kpar=list(sigma=1/length(predvars)))

  rot <- rotated(kpca1)
  
  # cost matrix for SVM
  cats<-unique(train1$cat)
  nc<-length(cats)
  cost<-matrix(NA,nc,nc)
  for(j1 in 1:nc){
    for(j2 in 1:nc){
      cost[j1,j2]<-sum(train1$cat==cats[j1])/sum(train1$cat==cats[j2])
    }
  }
  
  # NRBM
  x<-cbind(1,rot)
  y<-as.numeric(as.character(train1$cat))
  # suppress output from nrbm
  { sink("/dev/null"); nrbm1<-nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                                   LAMBDA=lambda.opt[i]); sink() }
  
  xv<-cbind(1,as.matrix(predict(kpca1,val1[,predvars])))
  pred<-factor(round(xv %*% nrbm1),levels=1:5)
  cM<-confusionMatrix(val1$cat,pred)
  acc<-cM$overall["Accuracy"]
  if(!is.nan(acc)){
    accuracy[i]<-acc
    ci95[i]<-(cM$overall["AccuracyUpper"]-cM$overall["AccuracyLower"])/2
  }
}