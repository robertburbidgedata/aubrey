# NRBM prediction of average value in last 12 months (2016-09 to 2017-08)
# by competitive group
# based on demographics, census, and HCP_JOIN_15
# value is binned monthly TRX_SCRIPTS 
#   (as advised by Gabriell & same as Florian,
#    but here we look at total sales in competitive group,
#    not just BI products)
# decision tree is used to filter out zero-sales cases
# performance estimated on DLD by test set with lambda optimized on valn set
#   NRBM used for regularized minimization of ordinal regression loss and
#   lambda optimized on valn set using same loss function
# performance reported as confusion matrix, mean balanced accuracy prob most useful
# retrain on train+val
# predict on all in demographics
# (no kernel transform - but could be useful to use kernel PCA with 
#  sigmoid kernel and choose 2/3 of PCs as features to simulate
#  MLP, which had the best results previously ...
#  but kpca fails with tanh kernel and experiments with rbf kernel gave poor results)
# Q. Can we calibrate by aggregating predictions for non-DLD 
#    and comparing to brick-level sales? (Gabriell to ask IMS about this)
# N1. Check ACCOUNT_STATUS - DONE (all Active on demographics)
# N2. DLD sales to 2017-08
# N3. Non-DLD sales to 2017-10
# N4. Possibly adjust by time trends by PROVINCE

# possible calibration check:
#   calculate mean TRX_SCRIPTS by cat for DLD and use this to derive
#   total predicted sales for non-DLD at brick level from cat preds
#   compare to actual sales at brick level for non-DLD
#   NB1. need to group minor bricks
#   NB2. some FSAs split, exclude these
#   NB3. have no idea whether the ONEKEY_IDs being predicted on here are the 
#        same as those generating the sales in GPM file ... (ask IMS?)

# parametrize bounded monotonic function of yhat -> ystar

rm(list=ls())

# parameters
startmth<-as.Date("2016-09-01")
endmth<-as.Date("2017-08-01")
ptest<-0.2
ptrain<-1-ptest


### predictive features for all ONEKEY_IDs on demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                             "factor","factor","factor","character",
                                                             "character","character","factor",
                                                             "factor","factor","factor","numeric",
                                                             "factor","factor","factor","factor",
                                                             "factor"))

# NB. Need to deal with missing values where FSA missing
# years can be zero

# ACCOUNT_STATUS
levels(demographics$ACCOUNT_STATUS)

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
demographics$PROVINCE<-NULL # could use to merge in prevalence

# years (see yearsNA.R)
demographics$years <- 2017-demographics$GRADUATION_DATE
demographics$years[is.na(demographics$years)]<-0
demographics$GRADUATION_DATE <- NULL

# variables for prediction
discrete_vars <- c("PRIMARY_SPECIALTY","SCI_EXPERT",
                   "GP_SPECIALISTS","GENDER","SECONDARY_SPECIALTY",
                   "PRIMARY_LANGUAGE")

# OHE (change "" to "NA" to avoid name duplication)
oldnames<-names(demographics)
for (discrete_var in discrete_vars) {
  levels(demographics[,discrete_var])<-c(levels(demographics[,discrete_var]),"NA1","NA2")
  demographics[is.na(demographics[,discrete_var]),discrete_var]<-"NA1"
  demographics[demographics[,discrete_var]=="",discrete_var]<-"NA2"
  demographics<-droplevels(demographics)
  demographics <- cbind(demographics,
                        model.matrix(as.formula(paste("~",discrete_var,"-1")),demographics))
  demographics[,discrete_var]<-NULL
}
newnames<-names(demographics)
ohevars<-setdiff(newnames,oldnames)

# addl features from HCP_JOIN_15
# see valpredHCP3.R
HCP_JOIN_15 <- read.csv("/wps_scratch/usr_scratch/burbidge/Aubrey/HCP_JOIN_15.csv",
                        header=TRUE,colClasses=c("character","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","numeric","numeric"
                                                 ,"numeric","factor","factor"
                                                 ,"factor","factor"))
discrete_vars_HCP15<-c("REVIEWS_CAT_HELP_KNOW","REVIEWS_CAT_STAFF_PUNCT","REVIEWS_CATEGORY"
                       ,"REVIEWS_KNOWLEDGEABLE")

# drop correlated features
HCP_JOIN_15$REVIEWS_VAR_RATING<-NULL
HCP_JOIN_15$REVIEWS_MEDIAN_RATING<-NULL
HCP_JOIN_15$REVIEWS_F1_SD_RATING<-NULL
HCP_JOIN_15$REVIEWS_AVR_HELP_KNOW<-NULL
HCP_JOIN_15$REVIEWS_AVR_STAFF_PUNCT<-NULL

# OHE (change "" to "NA" to avoid name duplication)
oldnames<-names(HCP_JOIN_15)
for (discrete_var in discrete_vars_HCP15) {
  if(any(HCP_JOIN_15[,discrete_var]=="")){
    levels(HCP_JOIN_15[,discrete_var])<-c(levels(HCP_JOIN_15[,discrete_var]),"NA")
    HCP_JOIN_15[HCP_JOIN_15[,discrete_var]=="",discrete_var]<-"NA"
    HCP_JOIN_15<-droplevels(HCP_JOIN_15)
  }
  HCP_JOIN_15 <- cbind(HCP_JOIN_15,model.matrix(as.formula(paste("~",discrete_var,"-1"))
                                                ,HCP_JOIN_15))
  HCP_JOIN_15[,discrete_var]<-NULL
}
newnames<-names(HCP_JOIN_15)
ohevars_HCP15<-setdiff(newnames,oldnames)

# ctsvars on HCP_JOIN_15
ctsvars_HCP15<-setdiff(names(HCP_JOIN_15),c(ohevars_HCP15,"ACCOUNT_ONEKEY_ID"))
ctsvars_HCP15<-setdiff(ctsvars_HCP15,
                       c("REVIEWS_VAR_RATING","REVIEWS_MEDIAN_RATING","REVIEWS_F1_SD_RATING"
                         ,"REVIEWS_AVR_HELP_KNOW","REVIEWS_AVR_STAFF_PUNCT"))

# missing values on HCP_JOIN_15 - take average over FSA
HCP_JOIN_15<-merge(demographics[,c("ONEKEY_ID","FSA")]
                   ,HCP_JOIN_15,by.x="ONEKEY_ID",by.y="ACCOUNT_ONEKEY_ID",all.x=T)
HCP_JOIN_15_fsa<-aggregate(HCP_JOIN_15[,setdiff(names(HCP_JOIN_15),c("FSA","ONEKEY_ID"))],
                           by=list(HCP_JOIN_15$FSA),mean,na.rm=T)
names(HCP_JOIN_15_fsa)<-c("FSA",setdiff(names(HCP_JOIN_15),c("FSA","ONEKEY_ID")))

# missing values FSA
for (HCP15var in c(ctsvars_HCP15,ohevars_HCP15)){
  HCP_JOIN_15_fsa[is.na(HCP_JOIN_15_fsa[,HCP15var]),HCP15var]<-
    mean(HCP_JOIN_15_fsa[!is.na(HCP_JOIN_15_fsa[,HCP15var]),HCP15var])
}
HCP_JOIN_15_fsa<-droplevels(HCP_JOIN_15_fsa)

# missing values ONEKEY_ID
for (HCP15var in c(ctsvars_HCP15,ohevars_HCP15)){
  HCP_JOIN_15<-merge(HCP_JOIN_15,HCP_JOIN_15_fsa[,c("FSA",HCP15var)],"FSA",all.x=T)
  HCP_JOIN_15[is.na(HCP_JOIN_15[,paste(HCP15var,"x",sep=".")]),paste(HCP15var,"x",sep=".")]<-
    HCP_JOIN_15[is.na(HCP_JOIN_15[,paste(HCP15var,"x",sep=".")]),paste(HCP15var,"y",sep=".")]
  HCP_JOIN_15[,paste(HCP15var,"y",sep=".")]<-NULL
}
names(HCP_JOIN_15)<-c("FSA","ONEKEY_ID",ctsvars_HCP15,ohevars_HCP15)
HCP_JOIN_15$FSA<-NULL

# merge (one duplicate ONEKEY_ID with inconsistent features)
x<-merge(demographics,HCP_JOIN_15,"ONEKEY_ID",all.x=T,all.y=F)
x<-unique(x)

# add in census 2016 data at FSA
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

# this gives a warning, but isn't needed here in any case
#census$Total <- as.numeric(ifelse(census$Total=="...","NA",census$Total))

# some FSAs on demographics are split but not split on census
x$FSA1<-substr(x$FSA,1,3)

# income
income <- census[census$Attribute=="Median after-tax income of households in 2015 ($)",]
income$Attribute <- NULL
names(income) <- c("FSA","Income")
x <- merge(x,income,by.x="FSA1",by.y="FSA",all.x=T)
x$Income[is.na(x$Income)] <- mean(x$Income[!is.na(x$Income)])

# age
age <- census[census$Attribute=="Average age of the population",]
age$Attribute <- NULL
names(age) <- c("FSA","Age")
x <- merge(x,age,by.x="FSA1",by.y="FSA",all.x=T)
x$Age[is.na(x$Age)] <- mean(x$Age[!is.na(x$Age)])

# Population, 2016
pop <- census[census$Attribute=="Population, 2016",]
pop$Attribute <- NULL
names(pop) <- c("FSA","Pop")
x <- merge(x,pop,by.x="FSA1",by.y="FSA",all.x=T)
x$Pop[is.na(x$Pop)] <- mean(x$Pop[!is.na(x$Pop)])

# Population per ONEKEY_ID
nhcp_fsa<-aggregate(x$ONEKEY_ID,by=list(x$FSA),length)
names(nhcp_fsa)<-c("FSA","nHCP")
x<-merge(x,nhcp_fsa,by.x="FSA1",by.y="FSA",all.x=T)
x$popperHCP<-x$Pop/x$nHCP
x$popperHCP[is.na(x$popperHCP)] <- mean(x$popperHCP[!is.na(x$popperHCP)])
x$nHCP<-NULL

# scale to [0,1]
ctsvars<-c("Income","Age","Pop","popperHCP","years")
for (ctsvar in c(ctsvars,ctsvars_HCP15)){
  x[,ctsvar]<-(x[,ctsvar]-min(x[,ctsvar]))/
    (max(x[,ctsvar])-min(x[,ctsvar]))
}

x$FSA1<-NULL

### TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))

# drop unneeded fields
TRx_1Mo$NRX_SCRIPTS<-NULL
TRx_1Mo$TRX_UNITS<-NULL
TRx_1Mo$PRODUCT_CODE<-NULL
TRx_1Mo$FINDER_ID<-NULL

# drop unneeded products
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Anticoagulants Orals","Bronchial","Diabetes",
                                            "Komboglyze",
                                            "Jardiance excl combos","Trajenta excl combos",
                                            "Spiriva Handihaler","Spiriva Respimat")),]

# drop products not on non-DLD data (for calibration)
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Janumet")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# look-up table for Comp group
lookup<-data.frame(PRODUCT=levels(TRx_1Mo$PRODUCT),Group=c("RESP_1","CV_1","DIA_2","RESP_1",
                                                           "DIA_2","DIA_1","DIA_2",
                                                           "DIA_1","DIA_1","CV_1","RESP_2",
                                                           "RESP_2","DIA_1","RESP_2","RESP_1",
                                                           "CV_1"))
TRx_1Mo<-merge(TRx_1Mo,lookup,by="PRODUCT")
TRx_1Mo$PRODUCT<-NULL
groups<-levels(TRx_1Mo$Group)
ng<-length(groups)

# mean sales last twelve months
TRx_1Mo$MONTH<-as.Date(paste(TRx_1Mo$PERIOD,"01",sep=""),format="%Y%m%d")
TRx_1Mo$PERIOD<-NULL
idx<-TRx_1Mo$MONTH>=startmth&TRx_1Mo$MONTH<=endmth
sales<-aggregate(TRx_1Mo$TRX_SCRIPTS[idx],TRx_1Mo[idx,c("ONEKEY_ID","Group")],mean,na.rm=T)
names(sales)<-c("ONEKEY_ID","Group","TRX_SCRIPTS")

# fill in zeros for missing products
skel<-merge(data.frame(ONEKEY_ID=unique(sales$ONEKEY_ID))
            ,data.frame(Group=levels(sales$Group)),by=NULL)
sales<-merge(skel,sales,c("ONEKEY_ID","Group"),all.x=T)
sales$TRX_SCRIPTS[is.na(sales$TRX_SCRIPTS)]<-0

# value category by group by year
sales$cat<-numeric(length(sales$TRX_SCRIPTS))
sales$cat[sales$TRX_SCRIPTS==0]<-0
sales$cat[sales$TRX_SCRIPTS>0&sales$TRX_SCRIPTS<5]<-1 
#sales$cat[sales$TRX_SCRIPTS<5]<-1 
sales$cat[sales$TRX_SCRIPTS>=5&sales$TRX_SCRIPTS<15]<-2 
sales$cat[sales$TRX_SCRIPTS>=15&sales$TRX_SCRIPTS<25]<-3 
sales$cat[sales$TRX_SCRIPTS>=25&sales$TRX_SCRIPTS<50]<-4 
sales$cat[sales$TRX_SCRIPTS>=50]<-5 
sales$cat<-factor(sales$cat,levels=0:5,ordered=TRUE)

# non-zero flag
sales$nz<-as.factor(ifelse(sales$TRX_SCRIPTS!=0,"non-zero","zero"))

# merge (1802 accounts on sales not on demographics)
dat<-merge(x,sales,"ONEKEY_ID",all.x=T,all.F=F)
dat<-dat[dat$ONEKEY_ID!="",]

### modelling
predvars<-c(ctsvars,ctsvars_HCP15,ohevars,ohevars_HCP15)
idx_dld<-!is.na(dat$cat)
ndld<-sum(idx_dld)/ng
ntrain<-floor(ndld*ptrain)
ntest<-ndld-ntrain

library(C50)
library(bmrm)
library(caret)
library(ordinal)

# minimize loss over lambda on valn and estimate confusion matrix on test
# (NB. low lambda is equiv. to unbounded coefs and can take a lot of iters)
# retrain on everything and predict
cMs<-list()
loss_lambda<-list()
lambdas<-exp(seq(-10,10,length.out=41))
nlambda<-length(lambdas)
outpreddata<-data.frame(ONEKEY_ID=dat$ONEKEY_ID[!idx_dld],FSA=dat$FSA[!idx_dld])
#outpreddata<-unique(outpreddata)

for(groupi in groups){
  print(groupi)
  
  dat1<-dat[idx_dld&dat$Group==groupi,]
  # (assume we know a priori class propns)
  # (and expected sales in each bin)
  expsales<-aggregate(dat1$TRX_SCRIPTS,list(dat1$cat),mean)$x[2:length(levels(dat1$cat))]
  
  prevalencez<-table(dat1$nz)/length(dat1$nz)
  # cost matrix for C5.0
  costz<-matrix(0,nrow=2,ncol=2,dimnames=list(c("non-zero","zero"),c("non-zero","zero")))
  costz[1,2]<-prevalencez[1]/prevalencez[2]
  costz[2,1]<-prevalencez[2]/prevalencez[1]
  
  ally<-as.numeric(as.character(dat1$cat))
  nbin<-table(ally)
  prevalence<-nbin/length(ally)
  prevalence1<-numeric(length(prevalence)-1)
  prevalence1[1]<-prevalence[1]+prevalence[2]
  prevalence1[2:(length(prevalence)-1)]<-prevalence[3:length(prevalence)]
  names(prevalence1)<-names(prevalence)[2:length(prevalence)]
  
  # cost matrix for NRBM (same as for SVM) now upweighted by expected sales per bin
  # could recode y here to be safe but we know values are {1,2,3,4,5} or {1,2,3,4}
  # it is possible but unlikely that subsets (train, val, test) could lack a cat
  # which would cause an error
  nc<-length(prevalence1)
  cost<-matrix(0,nc,nc)
  for(i in 1:nc){
    for(j in 1:nc){
      if (i!=j)
        #cost[i,j]<-sum(ally==i)/sum(ally==j)*expsales[j]
        cost[i,j]<-prevalence1[i]/prevalence1[j]*expsales[j]
    }
  }
  
  # [18/01/18] drop constant columns
  newpredvars<-names(dat1[,predvars][, sapply(dat1[,predvars], function(v) var(v, na.rm=TRUE)!=0)])
  
  # training/test sets (stratified over bins)
  # round down for bins 1-4 and put rest in bin 5 as it is smallest
  nbins<-length(unique(dat1$cat))
  ntrainbin<-numeric(nbins)
  ntrainbin[2:nbins]<-floor(ntrain*prevalence[2:nbins])
  ntrainbin[1]<-ntrain-sum(ntrainbin)
  # if any bin in test is size 1 then borrow one from train
  ntestbin<-table(ally)-ntrainbin
  ntestbin<-pmax(ntestbin,rep(2,length.out=nbins))
  ntrainbin<-table(ally)-ntestbin
  train<-dat1[F,]
  test<-dat1[F,]
  for (bin in 1:nbins){
    idx<-sample(nbin[bin],nbin[bin])
    trainbin<-dat1[ally+1==bin,][idx[1:ntrainbin[bin]],]
    testbin<-dat1[ally+1==bin,][idx[(ntrainbin[bin]+1):nbin[bin]],]
    train<-rbind(train,trainbin)
    test<-rbind(test,testbin)
  }
  
  # training/val sets (stratified over bins)
  trainy<-as.numeric(as.character(train$cat))
  nbin<-table(trainy)
  ntrain1<-floor(ntrain*ptrain)
  ntrain1bin<-numeric(nbins)
  ntrain1bin[1:(nbins-1)]<-floor(ntrain1*prevalence[1:(nbins-1)])
  ntrain1bin[nbins]<-ntrain1-sum(ntrain1bin)
  # if any bin in val1 is size 1 then borrow one from train1
  nval1bin<-table(trainy)-ntrain1bin
  nval1bin<-pmax(nval1bin,rep(2,length.out=nbins))
  ntrain1bin<-table(trainy)-nval1bin
  train1<-dat1[F,]
  val1<-dat1[F,]
  for (bin in 1:nbins){
    idx<-sample(nbin[bin],nbin[bin])
    train1bin<-train[trainy+1==bin,][idx[1:ntrain1bin[bin]],]
    val1bin<-train[trainy+1==bin,][idx[(ntrain1bin[bin]+1):nbin[bin]],]
    train1<-rbind(train1,train1bin)
    val1<-rbind(val1,val1bin)
  }
  
  # filter with C5.0 (use sample=ptrain for accuracy estimate)
  # minCases=2 since the rare high-value HCPs need to be separated
  x<-train1[,newpredvars]
  y<-train1$nz
  control=C5.0Control(subset=TRUE,winnow=TRUE,minCases=2)
  c5.mod<-C5.0(x,y,rules=TRUE,control=control,cost=costz)
  t1idx<-predict.C5.0(c5.mod,x)=="non-zero"
  xv<-val1[,newpredvars]
  v1idx<-predict.C5.0(c5.mod,xv)=="non-zero"
  
  # filtered data for NRBM (which doesn't care about class zero)
  x<-cbind(1,as.matrix(train1[t1idx,newpredvars]))
  y<-as.numeric(as.character(train1[t1idx,]$cat))
  y[y==0]<-1
  xv<-cbind(1,as.matrix(val1[v1idx,newpredvars]))
  yv<-as.numeric(as.character(val1[v1idx,]$cat))
  yv[yv==0]<-1

  # NRBM optimize lambda
  loss_lambda[[groupi]]<-numeric(nlambda)
  for(lambdai in 1:nlambda){
    # suppress output from nrbm
    { sink("/dev/null")
      
      val<-try(nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                                     LAMBDA=lambdas[lambdai]),silent=TRUE)
      if(class(val)!="try-error") nrbm.w<-val
      else nrbm.w<-rep(0,length(newpredvars)+1)
      sink() 
    }
    #pred<-factor(as.character(round(xv %*% nrbm.w)),levels=as.character(sort(unique(yv))))
    #baccuracy_lambda[lambdai]<-mean(confusionMatrix(pred,factor(yv),prevalence=table(ally)/length(ally)
    #                    ,mode="everything")$byClass[,"Balanced Accuracy"],na.rm=T)
    #accuracy_lambda[lambdai]<-confusionMatrix(pred,factor(yv),prevalence=table(ally)/length(ally)
    #                                          ,mode="everything")$overall["Accuracy"]
    loss_lambda[[groupi]][lambdai]<-attr(ordinalRegressionLoss(xv,yv,C=cost[unique(yv),unique(yv)]
                                      ,impl="loglin")(nrbm.w),"lvalue")
  }
  #plot(log(lambdas),loss_lambda[[groupi]])
  
  # retrain on train1+val1 with opt lambda and eval on test
  lambda.opt<-lambdas[which.min(loss_lambda[[groupi]])]
  
  # (re)filter with C5.0 
  x<-train[,newpredvars]
  y<-train$nz
  control=C5.0Control(subset=TRUE,winnow=TRUE,minCases=2)
  c5.mod<-C5.0(x,y,rules=TRUE,control=control,cost=costz)
  tidx<-predict.C5.0(c5.mod,x)=="non-zero"
  xt<-test[,newpredvars]
  tstidx<-predict.C5.0(c5.mod,xt)=="non-zero"
  
  # (re)filtered data for NRBM
  x<-cbind(1,as.matrix(train[tidx,newpredvars]))
  y<-as.numeric(as.character(train[tidx,]$cat))
  y[y==0]<-1
  xt<-cbind(1,as.matrix(test[tstidx,newpredvars]))
  yt<-as.numeric(as.character(test$cat))
  yt[yt==0]<-1
  { sink("/dev/null")
    
    val<-try(nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
                  LAMBDA=lambda.opt),silent=TRUE)
    if(class(val)!="try-error") nrbm.w<-val
    else nrbm.w<-rep(0,length(newpredvars)+1)
    sink() 
  }
  
  # calibrate output using clm
  yhat<-as.numeric(x %*% nrbm.w)
  exp1<-exp(yhat/1e1)
  exp2<-exp(yhat^2/1e2)
  exp3<-exp(yhat^3/1e3)
  wgts<-merge(data.frame(cat=y),data.frame(cat=names(prevalence1)
                                           ,wgt=1/as.numeric(prevalence1)))$wgt
  ystar<-factor(y,ordered=T)
  df<-data.frame(exp1=exp1,exp2=exp2,exp3=exp3,ystar=ystar)
  clm1<-clm(ystar~exp1+exp2+exp3+1,data=df,weights=wgts)

  # test preds (non-tstidx preds are necessarily zero)
  ythat<-as.numeric(xt %*% nrbm.w)
  exp1<-exp(ythat/1e1)
  exp2<-exp(ythat^2/1e2)
  exp3<-exp(ythat^3/1e3)
  df<-data.frame(exp1=exp1,exp2=exp2,exp3=exp3,yhat=ythat)
  pred<-numeric(ntest)
  pred[tstidx]<-apply(predict(clm1,df[,c("yhat","exp1","exp2","exp3")])$fit,1,which.max)
  
  # regroup zeros (default, hence label for !tstidx) into bin 1
  pred[pred==0]<-1
  pred<-factor(pred,levels=unique(yt))

  # confusion matrix
  cMs[[groupi]]<-confusionMatrix(pred,factor(yt),prevalence=prevalence1,mode="everything")
  
  # retrain on train+test and make predictions
  # distribution of preds goes awry here
  # safer, and more correct, to keep model above
  # x<-cbind(1,as.matrix(dat1[,predvars]))
  # y<-as.numeric(as.character(dat1$cat))
  # { sink("/dev/null")
  #   
  #   val<-try(nrbm(ordinalRegressionLoss(x,y,C=cost,impl="loglin"),
  #                 LAMBDA=lambda.opt),silent=TRUE)
  #   if(class(val)!="try-error") nrbm.w<-val
  #   else nrbm.w<-rep(0,length(predvars)+1)
  #   sink() 
  # }
  # 
  # # calibrate output using clm
  # yhat<-as.numeric(x %*% nrbm.w)
  # wgts<-merge(data.frame(cat=y),data.frame(cat=names(prevalence),wgt=1/as.numeric(prevalence)))$wgt
  # ystar<-factor(y,ordered=T)
  # r<-rnorm(ndld,0,0.001)
  # df<-data.frame(yhat=yhat,r=r,ystar=ystar)
  # clm1<-clm(ystar~yhat+r+1,data=df,weights=wgts)
  # 
  # # check preds on dld
  # pred<-factor(apply(predict(clm1,df[,c("yhat","r")])$fit,1,which.max),levels=levels(ystar))
  # confusionMatrix(pred,ystar,prevalence=prevalence,mode="everything")
  
  # non-dld preds
  dat2<-dat[!idx_dld,]

  # filter with C5.0 
  x2<-dat2[,newpredvars]
  x2idx<-predict.C5.0(c5.mod,x2)=="non-zero"
  x2<-cbind(1,as.matrix(dat2[x2idx,newpredvars]))
  
  yhat<-as.numeric(x2 %*% nrbm.w)
  exp1<-exp(yhat/1e1)
  exp2<-exp(yhat^2/1e2)
  exp3<-exp(yhat^3/1e3)
  df<-data.frame(exp1=exp1,exp2=exp2,exp3=exp3,yhat=yhat)

  pred<-numeric(nrow(dat2))
  pred[x2idx]<-apply(predict(clm1,df[,c("yhat","exp1","exp2","exp3")])$fit,1,which.max)

  # regroup zeros (default for !x2idx) into bin 1
  pred[pred==0]<-1

  # collate
  preddf<-data.frame(ONEKEY_ID=dat2$ONEKEY_ID,pred=pred)
  names(preddf)<-c("ONEKEY_ID",groupi) # there is a cleverer way of doing this naming ...
  outpreddata<-merge(outpreddata,preddf,"ONEKEY_ID",all.x=T)
}

save(list=ls(all.names=TRUE),file="/wps_scratch/usr_scratch/burbidge/Aubrey/finalworkspace1b.RData"
     ,envir=.GlobalEnv)

# performance
round(sapply(cMs,function(x) mean(x$byClass[,"Balanced Accuracy"],na.rm=T)),2)
round(sapply(cMs,function(x) x$overall["Accuracy"]),2)
round(sapply(cMs,function(x) x$overall["AccuracyUpper"]-x$overall["Accuracy"]),2)

# lift
(mylift<-round(
sapply(cMs,function(x) 
  sum(x$table[3:nrow(x$table),3:ncol(x$table)][1:((nrow(x$table)-2)^2)])/
    sum(x$table[3:nrow(x$table),][1:((nrow(x$table)-2)^2)]))/
sapply(cMs,function(x) 
  sum(x$table[,3:ncol(x$table)][1:(nrow(x$table)*(ncol(x$table)-2))])/
    sum(x$table[1:(nrow(x$table)^2)]))
,2))
prod(mylift)^(1/ng)

# distribution of preds
lapply(outpreddata[,groups],table)

# confusion matrices
sapply(cMs, function(x) x$table)

# output
write.csv(outpreddata,row.names=F
          ,file="/wps_scratch/usr_scratch/burbidge/Aubrey/finalpreds1b.csv")

