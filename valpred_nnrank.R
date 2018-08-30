# valpred_nnrank.R
# http://sysbio.rnet.missouri.edu/multicom_toolbox/nnrank%201.1.html
rm(list=ls())

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
names(TRx_1Mo)

idx<-TRx_1Mo$PRODUCT=="Spiriva"
y1<-aggregate(TRx_1Mo$TRX_SCRIPTS[idx],list(TRx_1Mo$PERIOD[idx]),sum)
idx<-TRx_1Mo$PRODUCT=="Spiriva Handihaler"
y2a<-aggregate(TRx_1Mo$TRX_SCRIPTS[idx],list(TRx_1Mo$PERIOD[idx]),sum)
idx<-TRx_1Mo$PRODUCT=="Spiriva Respimat"
y2b<-aggregate(TRx_1Mo$TRX_SCRIPTS[idx],list(TRx_1Mo$PERIOD[idx]),sum)
plot(y1$Group.1,y1$x,type="l",col=1)
y2<-merge(y2a,y2b,"Group.1")
lines(y2$Group.1,y2$x.x+y2$x.y,col=2)
# Spiriva == Spiriva Handihaler + Spiriva Respimat

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

# value category by group by year (note: 0-4, not 1-5)
sales$cat<-numeric(length(sales$TRX_SCRIPTS))
sales$cat[sales$TRX_SCRIPTS<5]<-0 
sales$cat[sales$TRX_SCRIPTS>=5&sales$TRX_SCRIPTS<15]<-1 
sales$cat[sales$TRX_SCRIPTS>=15&sales$TRX_SCRIPTS<25]<-2 
sales$cat[sales$TRX_SCRIPTS>=25&sales$TRX_SCRIPTS<50]<-3 
sales$cat[sales$TRX_SCRIPTS>=50]<-4 
sales$cat<-factor(sales$cat,levels=0:4,ordered=TRUE)

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
  dat[,discrete_var]<-NULL
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

# prediction
dat$FSA<-NULL
dat$PROVINCE<-NULL
predvars <- setdiff(names(dat),c("ONEKEY_ID","Group","cat","nHCP"))
(cnts<-aggregate(dat$ONEKEY_ID,by=list(dat$Group),length))
names(cnts)<-c("Group","N")
cnts$ntrain<-floor(cnts$N*0.9)
cnts$ntest<-cnts$N-cnts$ntrain
cnts

# export train data
for(groupi in groups){
  #groupi<-groups[1]
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

  # export data
  ofile<-paste("/icebox_data/data/tmp/burbidge/aubrey/nnrank/train1",groupi,sep="_")
  write(length(predvars),ofile,ncolumns=1,append=FALSE,sep=" ")
  write(c(length(levels(train1$cat)),levels(train1$cat)),ofile,
        ncolumns=length(levels(train1$cat))+1,append=TRUE,sep=" ")
  write(dim(train1)[1],ofile,ncolumns=1,append=TRUE,sep=" ")
  write(t(as.matrix(train1[,c("cat",predvars)])),
        ofile,ncolumns=length(predvars)+1,append=TRUE,sep=" ")
  ofile<-paste("/icebox_data/data/tmp/burbidge/aubrey/nnrank/val1",groupi,sep="_")
  write(length(predvars),ofile,ncolumns=1,append=FALSE,sep=" ")
  write(c(length(levels(val1$cat)),levels(val1$cat)),ofile,
        ncolumns=length(levels(val1$cat))+1,append=TRUE,sep=" ")
  write(dim(val1)[1],ofile,ncolumns=1,append=TRUE,sep=" ")
  write(t(as.matrix(val1[,c("cat",predvars)])),
        ofile,ncolumns=length(predvars)+1,append=TRUE,sep=" ")
}
