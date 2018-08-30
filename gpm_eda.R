# gpm_eda.R
rm(list=ls())

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","character",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
names(TRx_1Mo)
TRx_1Mo$PRODUCT_CODE<-NULL
TRx_1Mo$FINDER_ID<-NULL
TRx_1Mo$MONTH<-as.Date(paste(TRx_1Mo$PERIOD,"01",sep=""),format="%Y%m%d")
range(TRx_1Mo$MONTH)
TRx_1Mo$PERIOD<-NULL

# drop unneeded products
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Anticoagulants Orals","Bronchial","Diabetes",
                                            "Komboglyze",
                                            "Jardiance excl combos","Trajenta excl combos",
                                            "Spiriva Handihaler","Spiriva Respimat")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# GPM_Retail_Sales.txt
TRx_1Mo_GPM <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/GPM_Retail_Sales.txt",
              header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                 "factor","Date","numeric","numeric","numeric"
                                                 ,"numeric","numeric"))
names(TRx_1Mo_GPM)
TRx_1Mo_GPM$GPM_PRODUCT<-NULL

# aggregate Spiriva
levels(TRx_1Mo_GPM$PRODUCT)
tmp1<-TRx_1Mo_GPM[TRx_1Mo_GPM$PRODUCT=="Spiriva Handihaler",-1]
tmp2<-TRx_1Mo_GPM[TRx_1Mo_GPM$PRODUCT=="Spiriva Respimat",-1]
tmp<-merge(tmp1,tmp2,by=c("GPM_MAJOR_BRICK","GPM_GROUPED_BRICK","MONTH"),all=T)
tmp$PRODUCT<-"Spiriva"
tmp$TRX<-apply(cbind(tmp$TRX.x,tmp$TRX.y),1,sum,na.rm=T)
tmp$TRX.x<-NULL
tmp$TRX.y<-NULL
tmp$NRX<-apply(cbind(tmp$NRX.x,tmp$NRX.y),1,sum,na.rm=T)
tmp$NRX.x<-NULL
tmp$NRX.y<-NULL
tmp$SALES<-apply(cbind(tmp$SALES.x,tmp$SALES.y),1,sum,na.rm=T)
tmp$SALES.x<-NULL
tmp$SALES.y<-NULL
tmp$UNITS<-apply(cbind(tmp$UNITS.x,tmp$UNITS.y),1,sum,na.rm=T)
tmp$UNITS.x<-NULL
tmp$UNITS.y<-NULL
tmp$NBRX<-apply(cbind(tmp$NBRX.x,tmp$NBRX.y),1,sum,na.rm=T)
tmp$NBRX.x<-NULL
tmp$NBRX.y<-NULL
TRx_1Mo_GPM<-rbind(TRx_1Mo_GPM,tmp)

# drop unneeded products
TRx_1Mo_GPM <- TRx_1Mo_GPM[!(TRx_1Mo_GPM$PRODUCT %in% 
                c("Anticoagulants Orals","Bronchial","Diabetes","DPP4","Jardiance excl combos"
                  ,"SGLT2","Trajenta excl combos","Spiriva Handihaler","Spiriva Respimat")),]

# drop products not on DLD data
TRx_1Mo_GPM <- TRx_1Mo_GPM[!(TRx_1Mo_GPM$PRODUCT %in% 
                c("Alogliptin","Duaklir Genuair","Incruse Ellipta")),]
TRx_1Mo_GPM<-droplevels(TRx_1Mo_GPM)

# check and drop products not on non-DLD data
setdiff(levels(TRx_1Mo$PRODUCT),levels(TRx_1Mo_GPM$PRODUCT))
setdiff(levels(TRx_1Mo_GPM$PRODUCT),levels(TRx_1Mo$PRODUCT))
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Janumet")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# align date ranges
dates<-intersect(TRx_1Mo$MONTH,TRx_1Mo_GPM$MONTH)
TRx_1Mo<-TRx_1Mo[TRx_1Mo$MONTH%in%dates,]
TRx_1Mo_GPM<-TRx_1Mo_GPM[TRx_1Mo_GPM$MONTH%in%dates,]

# check
rbind(t(xtabs(TRX~PRODUCT,TRx_1Mo_GPM)),t(xtabs(TRX_SCRIPTS~PRODUCT,TRx_1Mo)))
# so, neither is a subset of the other

### total sales 2016 by competitive group by GPM_MAJOR_BRICK

# subset on 2016
TRx_1Mo<-TRx_1Mo[format(TRx_1Mo$MONTH,format="%Y")=="2016",]
TRx_1Mo_GPM<-TRx_1Mo_GPM[format(TRx_1Mo_GPM$MONTH,format="%Y")=="2016",]

# comp groups
lookup<-data.frame(PRODUCT=levels(TRx_1Mo_GPM$PRODUCT),Group=c("RESP_1","CV_1","DIA_2","RESP_1"
                                                              ,"DIA_2","DIA_1","DIA_2","DIA_1"
                                                              ,"DIA_1","CV_1","RESP_2","DIA_1"
                                                              ,"RESP_2","RESP_1","CV_1","RESP_2"))
TRx_1Mo_GPM<-merge(TRx_1Mo_GPM,lookup,by="PRODUCT")

# drop fields
TRx_1Mo_GPM$PRODUCT<-NULL
TRx_1Mo_GPM$NRX<-NULL
TRx_1Mo_GPM$SALES<-NULL
TRx_1Mo_GPM$UNITS<-NULL
TRx_1Mo_GPM$NBRX<-NULL

# agg
sales2016_GPM<-aggregate(TRx_1Mo_GPM$TRX,
                         by=TRx_1Mo_GPM[,c("GPM_MAJOR_BRICK","GPM_GROUPED_BRICK","Group")],sum)
names(sales2016_GPM)<-c("GPM_MAJOR_BRICK","GPM_GROUPED_BRICK","Group","TRX_SCRIPTS")

### demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                             "factor","factor","factor","character",
                                                             "character","character","factor",
                                                             "factor","factor","factor","numeric",
                                                             "factor","factor","factor","factor",
                                                             "factor"))
# subset
demographics <- demographics[demographics$GRADUATION_DATE<=2016&
                               !is.na(demographics$FSA),]
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

# bricks
fsas<-levels(demographics$FSA)
bricks<-levels(TRx_1Mo_GPM$GPM_MAJOR_BRICK)
length(fsas)
length(bricks)
length(setdiff(fsas,bricks))
length(setdiff(bricks,fsas))
bnotf<-setdiff(bricks,fsas)

# years
demographics$years <- 2016-demographics$GRADUATION_DATE
demographics$GRADUATION_DATE <- NULL

# variables for prediction
discrete_vars <- c("PRIMARY_SPECIALTY","SCI_EXPERT",
                   "GP_SPECIALISTS","GENDER","SECONDARY_SPECIALTY",
                   "PRIMARY_LANGUAGE")

# OHE (change "" to "NA" to avoid name duplication)
for (discrete_var in discrete_vars) {
  levels(demographics[,discrete_var])<-c(levels(demographics[,discrete_var]),"NA1","NA2")
  demographics[is.na(demographics[,discrete_var]),discrete_var]<-"NA1"
  demographics[demographics[,discrete_var]=="",discrete_var]<-"NA2"
  demographics<-droplevels(demographics)
  demographics <- cbind(demographics,
                        model.matrix(as.formula(paste("~",discrete_var,"-1")),demographics))
  demographics[,discrete_var]<-NULL
}

demographics$PROVINCE<-NULL

# demographics of bricks (FSA)
demographics_fsa<-aggregate(demographics[,setdiff(names(demographics),c("FSA","ONEKEY_ID"))],
                            by=list(demographics$FSA),mean)
names(demographics_fsa)<-c("FSA",setdiff(names(demographics),c("FSA","ONEKEY_ID")))

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

census$Total <- as.numeric(ifelse(census$Total=="...","",census$Total))

# some FSAs on demographics are split but not split on census
demographics_fsa$FSA1<-substr(demographics_fsa$FSA,1,3)

# income
income <- census[census$Attribute=="Median after-tax income of households in 2015 ($)",]
income$Attribute <- NULL
names(income) <- c("FSA","Income")
x_fsa <- merge(demographics_fsa,income,by.x="FSA1",by.y="FSA",all.x=T)
x_fsa$Income[is.na(x_fsa$Income)] <- mean(x_fsa$Income[!is.na(x_fsa$Income)])

# age
age <- census[census$Attribute=="Average age of the population",]
age$Attribute <- NULL
names(age) <- c("FSA","Age")
x_fsa <- merge(x_fsa,age,by.x="FSA1",by.y="FSA",all.x=T)
x_fsa$Age[is.na(x_fsa$Age)] <- mean(x_fsa$Age[!is.na(x_fsa$Age)])

# Population, 2016
pop <- census[census$Attribute=="Population, 2016",]
pop$Attribute <- NULL
names(pop) <- c("FSA","Pop")
x_fsa <- merge(x_fsa,pop,by.x="FSA1",by.y="FSA",all.x=T)
x_fsa$Pop[is.na(x_fsa$Pop)] <- mean(x_fsa$Pop[!is.na(x_fsa$Pop)])

# scale to [0,1]
x_fsa$Income <- (x_fsa$Income-min(x_fsa$Income))/(max(x_fsa$Income)-min(x_fsa$Income)) 
x_fsa$Age <- (x_fsa$Age-min(x_fsa$Age))/(max(x_fsa$Age)-min(x_fsa$Age)) 
x_fsa$Pop <- (x_fsa$Pop-min(x_fsa$Pop))/(max(x_fsa$Pop)-min(x_fsa$Pop)) 
x_fsa$years <- (x_fsa$years-min(x_fsa$years))/(max(x_fsa$years)-min(x_fsa$years)) 

### predvars grouped/ungrouped
ugidx<-as.character(TRx_1Mo_GPM$GPM_MAJOR_BRICK)==as.character(TRx_1Mo_GPM$GPM_GROUPED_BRICK)
ug_brick<-unique(as.character(TRx_1Mo_GPM$GPM_MAJOR_BRICK[ugidx]))
g_brick<-unique(as.character(TRx_1Mo_GPM$GPM_MAJOR_BRICK[!ugidx]))

# pred vars for ungrouped
x_fsa_ug<-x_fsa[as.character(x_fsa$FSA)%in%ug_brick,]
x_fsa_ug$FSA1<-NULL
names(x_fsa_ug)<-c("GPM_MAJOR_BRICK",setdiff(names(x_fsa_ug),"FSA"))

# predvars for grouped
g_brick_map<-unique(TRx_1Mo_GPM[as.character(TRx_1Mo_GPM$GPM_MAJOR_BRICK)%in%g_brick,
                         c("GPM_MAJOR_BRICK","GPM_GROUPED_BRICK")])
g_brick_map[nchar(as.character(g_brick_map$GPM_MAJOR_BRICK))!=3,]

# see gpm_brick_notes.txt
# demographics/census of grouped brick is weighted average with weight 10 on major brick and
# weight 2.5 on minor bricks (since bricks with <5 pharmacies are grouped to major)

# x_fsa_g1
g_brick_map1<-g_brick_map[nchar(as.character(g_brick_map$GPM_MAJOR_BRICK))==3&
                            !(g_brick_map$GPM_MAJOR_BRICK%in%bnotf),]
g_brick_map1$GPM_GROUPED_BRICK<-strsplit(as.character(g_brick_map1$GPM_GROUPED_BRICK),"-")

# split out grouped bricks and weight
l<-length(g_brick_map1$GPM_MAJOR_BRICK)
g_brick_map1e<-data.frame(GPM_MAJOR_BRICK=character(0),GPM_GROUPED_BRICK=character(0),
                          wgt=numeric(0))
for(i in 1:l){
  ll<-length(g_brick_map1$GPM_GROUPED_BRICK[[i]])
  for(j in 1:ll){
    g_brick_map1e<-rbind(g_brick_map1e,
          data.frame(GPM_MAJOR_BRICK=as.character(g_brick_map1$GPM_MAJOR_BRICK[i]),
                     GPM_GROUPED_BRICK=g_brick_map1$GPM_GROUPED_BRICK[[i]][j],
          wgt=ifelse(as.character(g_brick_map1$GPM_MAJOR_BRICK[i])==
                                    g_brick_map1$GPM_GROUPED_BRICK[[i]][j],10,2.5)))
  }
}

# check GPM_GROUPED_BRICK on g_brick_map1e is on x_fsa
nondldnotx<-setdiff(g_brick_map1e$GPM_GROUPED_BRICK,x_fsa$FSA1)
g_brick_map1e<-g_brick_map1e[!(g_brick_map1e$GPM_GROUPED_BRICK%in%nondldnotx),]
(n<-aggregate(g_brick_map1e$wgt,list(g_brick_map1e$GPM_MAJOR_BRICK),length))

# normalize weights
z<-aggregate(g_brick_map1e$wgt,list(g_brick_map1e$GPM_MAJOR_BRICK),sum)
g_brick_map1e<-merge(g_brick_map1e,z,by.x="GPM_MAJOR_BRICK",by.y="Group.1")
g_brick_map1e$wgt<-g_brick_map1e$wgt/g_brick_map1e$x
g_brick_map1e$x<-NULL

# weight x_fsa
x_fsa_g1.<-merge(x_fsa,g_brick_map1e,by.x="FSA",by.y="GPM_GROUPED_BRICK")
predvars<-setdiff(names(x_fsa_g1.),c("GPM_MAJOR_BRICK","FSA","FSA1","wgt"))
x_fsa_g1.[,predvars]<-x_fsa_g1.[,predvars]*x_fsa_g1.$wgt
x_fsa_g1.$wgt<-NULL
x_fsa_g1<-aggregate(x_fsa_g1.[,predvars],list(x_fsa_g1.$GPM_MAJOR_BRICK),sum)
names(x_fsa_g1)<-c("GPM_MAJOR_BRICK",predvars)

# x_fsa_g2
g_brick_map2<-g_brick_map[nchar(as.character(g_brick_map$GPM_MAJOR_BRICK))==5&
                            nchar(as.character(g_brick_map$GPM_GROUPED_BRICK))==3,]
x_fsa_g2<-x_fsa[as.character(x_fsa$FSA)%in%g_brick_map2$GPM_MAJOR_BRICK,]
x_fsa_g2$FSA1<-NULL
names(x_fsa_g2)<-c("GPM_MAJOR_BRICK",setdiff(names(x_fsa_g2),"FSA"))

# x_fsa_g3
g_brick_map3<-g_brick_map[nchar(as.character(g_brick_map$GPM_MAJOR_BRICK))==5&
                            !(g_brick_map$GPM_MAJOR_BRICK%in%bnotf)&
                            !(nchar(as.character(g_brick_map$GPM_GROUPED_BRICK))==3),]
g_brick_map3$GPM_GROUPED_BRICK<-strsplit(as.character(g_brick_map3$GPM_GROUPED_BRICK),"-")

# split out grouped bricks and weight
l<-length(g_brick_map3$GPM_MAJOR_BRICK)
g_brick_map3e<-data.frame(GPM_MAJOR_BRICK=character(0),GPM_GROUPED_BRICK=character(0),
                          wgt=numeric(0))
for(i in 1:l){
  ll<-length(g_brick_map3$GPM_GROUPED_BRICK[[i]])
  for(j in 1:ll){
    g_brick_map3e<-rbind(g_brick_map3e,
                         data.frame(GPM_MAJOR_BRICK=as.character(g_brick_map3$GPM_MAJOR_BRICK[i]),
                                    GPM_GROUPED_BRICK=g_brick_map3$GPM_GROUPED_BRICK[[i]][j],
                                wgt=ifelse(substr(as.character(g_brick_map3$GPM_MAJOR_BRICK[i]),1,3)==
                                                 g_brick_map3$GPM_GROUPED_BRICK[[i]][j],10,2.5)))
  }
}

# check GPM_GROUPED_BRICK on g_brick_map1e is on x_fsa
nondldnotx<-setdiff(g_brick_map3e$GPM_GROUPED_BRICK,x_fsa$FSA1)
g_brick_map3e<-g_brick_map3e[!(g_brick_map3e$GPM_GROUPED_BRICK%in%nondldnotx),]
(n<-aggregate(g_brick_map3e$wgt,list(g_brick_map3e$GPM_MAJOR_BRICK),length))

# normalize weights
z<-aggregate(g_brick_map3e$wgt,list(g_brick_map3e$GPM_MAJOR_BRICK),sum)
g_brick_map3e<-merge(g_brick_map3e,z,by.x="GPM_MAJOR_BRICK",by.y="Group.1")
g_brick_map3e$wgt<-g_brick_map3e$wgt/g_brick_map3e$x
g_brick_map3e$x<-NULL

# weight x_fsa
x_fsa_g3.<-merge(x_fsa,g_brick_map3e,by.x="FSA1",by.y="GPM_GROUPED_BRICK")
predvars<-setdiff(names(x_fsa_g3.),c("GPM_MAJOR_BRICK","FSA","FSA1","wgt"))
x_fsa_g3.[,predvars]<-x_fsa_g3.[,predvars]*x_fsa_g3.$wgt
x_fsa_g3.$wgt<-NULL
x_fsa_g3<-aggregate(x_fsa_g3.[,predvars],list(x_fsa_g3.$GPM_MAJOR_BRICK),sum)
names(x_fsa_g3)<-c("GPM_MAJOR_BRICK",predvars)

# x_fsa_g4
g_brick_map4<-g_brick_map[g_brick_map$GPM_MAJOR_BRICK%in%bnotf,]
g_brick_map4$GPM_GROUPED_BRICK<-strsplit(as.character(g_brick_map4$GPM_GROUPED_BRICK),"-")

# split out grouped bricks
l<-length(g_brick_map4$GPM_MAJOR_BRICK)
g_brick_map4e<-data.frame(GPM_MAJOR_BRICK=character(0),GPM_GROUPED_BRICK=character(0),
                          wgt=numeric(0))
for(i in 1:l){
  ll<-length(g_brick_map4$GPM_GROUPED_BRICK[[i]])
  for(j in 1:ll){
    g_brick_map4e<-rbind(g_brick_map4e,
                         data.frame(GPM_MAJOR_BRICK=as.character(g_brick_map4$GPM_MAJOR_BRICK[i]),
                                    GPM_GROUPED_BRICK=g_brick_map4$GPM_GROUPED_BRICK[[i]][j],
                                    wgt=1))
  }
}

# check GPM_GROUPED_BRICK on g_brick_map1e is on x_fsa
nondldnotx<-setdiff(g_brick_map4e$GPM_GROUPED_BRICK,x_fsa$FSA1)
g_brick_map4e<-g_brick_map4e[!(g_brick_map4e$GPM_GROUPED_BRICK%in%nondldnotx),]
(n<-aggregate(g_brick_map4e$wgt,list(g_brick_map4e$GPM_MAJOR_BRICK),length))

# normalize weights
z<-aggregate(g_brick_map4e$wgt,list(g_brick_map4e$GPM_MAJOR_BRICK),sum)
g_brick_map4e<-merge(g_brick_map4e,z,by.x="GPM_MAJOR_BRICK",by.y="Group.1")
g_brick_map4e$wgt<-g_brick_map4e$wgt/g_brick_map4e$x
g_brick_map4e$x<-NULL

# weight x_fsa
x_fsa_g4.<-merge(x_fsa,g_brick_map4e,by.x="FSA",by.y="GPM_GROUPED_BRICK")
predvars<-setdiff(names(x_fsa_g4.),c("GPM_MAJOR_BRICK","FSA","FSA1","wgt"))
x_fsa_g4.[,predvars]<-x_fsa_g4.[,predvars]*x_fsa_g4.$wgt
x_fsa_g4.$wgt<-NULL
x_fsa_g4<-aggregate(x_fsa_g4.[,predvars],list(x_fsa_g4.$GPM_MAJOR_BRICK),sum)
names(x_fsa_g4)<-c("GPM_MAJOR_BRICK",predvars)

# check
dim(x_fsa_ug)
dim(x_fsa_g1)
dim(x_fsa_g2)
dim(x_fsa_g3)
dim(x_fsa_g4)
tmp<-c(as.character(x_fsa_ug$GPM_MAJOR_BRICK),as.character(x_fsa_g1$GPM_MAJOR_BRICK)
       ,as.character(x_fsa_g2$GPM_MAJOR_BRICK),as.character(x_fsa_g3$GPM_MAJOR_BRICK)
       ,as.character(x_fsa_g4$GPM_MAJOR_BRICK))
setdiff(as.character(TRx_1Mo_GPM$GPM_MAJOR_BRICK),tmp)

x_fsa$FSA1<-NULL

# merge x and y data for modelling - GPM
x_fsa_gpm<-rbind(x_fsa_ug,x_fsa_g1,x_fsa_g2,x_fsa_g3,x_fsa_g4)
y<-aggregate(TRx_1Mo_GPM$TRX,by=TRx_1Mo_GPM[,c("GPM_MAJOR_BRICK","Group")],sum)
names(y)<-c("GPM_MAJOR_BRICK","Group","TRX_SCRIPTS")
dat_gpm<-merge(x_fsa_gpm,y,by="GPM_MAJOR_BRICK")

# merge x and y data for modelling - DLD
TRx_1Mo_DLD<-merge(TRx_1Mo[,c("ONEKEY_ID","PRODUCT","TRX_SCRIPTS")],
                   demographics[,c("ONEKEY_ID","FSA")],by="ONEKEY_ID")
lookup<-data.frame(PRODUCT=levels(TRx_1Mo_DLD$PRODUCT),Group=c("RESP_1","CV_1","DIA_2","RESP_1",
                                                           "DIA_2","DIA_1","DIA_2",
                                                           "DIA_1","DIA_1","CV_1","RESP_2",
                                                           "RESP_2","DIA_1","RESP_2","RESP_1",
                                                           "CV_1"))
TRx_1Mo_DLD<-merge(TRx_1Mo_DLD,lookup,by="PRODUCT")
TRx_1Mo_DLD$PRODUCT<-NULL
y<-aggregate(TRx_1Mo_DLD$TRX_SCRIPTS,by=TRx_1Mo_DLD[,c("FSA","Group")],sum)
names(y)<-c("FSA","Group","TRX_SCRIPTS")
dat_dld<-merge(x_fsa,y,by="FSA")

# check
dim(dat_dld)
dim(dat_gpm)

## distribution of annual sales over FSA/bricks for DLD and GPM
hist(dat_dld$TRX_SCRIPTS[dat_dld$TRX_SCRIPTS<1e4])
summary(dat_dld$TRX_SCRIPTS)
hist(dat_gpm$TRX_SCRIPTS[dat_gpm$TRX_SCRIPTS<1e4])
summary(dat_gpm$TRX_SCRIPTS)
# distns are different for DLD and GPM 

# transform y
hist(log(dat_dld$TRX_SCRIPTS+1))
hist(log(dat_gpm$TRX_SCRIPTS+1))

# regression for DLD and GPM separately
# svm-radial
# 10CV for RMSE and 10CV for C
# see fsa.R
library("e1071")
datasets<-c("dat_dld","dat_gpm")
nd<-length(datasets)
groups<-unique(levels(dat_dld$Group),levels(dat_gpm$Group))
ng<-length(groups)
rrse<-matrix(NA,nrow=nd,ncol=ng,dimnames=list(c("dat_dld","dat_gpm"),groups))
rrseCI95<-matrix(NA,nrow=nd,ncol=ng,dimnames=list(c("dat_dld","dat_gpm"),groups))
nfolds<-10
cs<-10^(2:4)
nc<-length(cs)
gammas<-(1/length(predvars))*c(1.0,2.0)
ngammas<-length(gammas)
rmsegrid<-array(NA,dim=c(nd,ng,nfolds,nc,ngammas)
                ,dimnames=list(c("dat_dld","dat_gpm"),groups,1:10,cs,gammas))

for (dataset in datasets) {
  dat<-get(dataset)
  for (group in groups) {
    datg<-dat[dat$Group==group,c(predvars,"TRX_SCRIPTS")]
    n<-dim(datg)[1]
    ntest<-floor(n*0.1)
    ntrain<-n-ntest
    idx<-sample(1:n,n)
    rmse10CV<-numeric(nfolds)
    rmse10CVCI95<-numeric(nfolds)
    for (i in 1:nfolds) {
      testidx<-idx[((i-1)*ntest+1):(i*ntest)]
      trainidx<-setdiff(idx,testidx)
      train<-datg[trainidx,]
      test<-datg[testidx,]
      for (j in 1:nc) {
        for (k in 1:ngammas) {
          rmsegrid[dataset,group,,,][i,,][j,k]<-sqrt(svm(x=train[,predvars],y=log(train$TRX_SCRIPTS+1)
                             ,scale=FALSE
                             ,kernel="radial"
                             ,type="eps-regression"
                             ,gamma=gammas[k],cost=cs[j]
                             ,cross=nfolds,fitted=FALSE)$tot.MSE)
        }
      }
      # pull out opt hyperparameters
      pos<-which.min(rmsegrid[dataset,group,,,][i,,])
      c.opti<-pos%%nc
      c.opti<-ifelse(c.opti==0,nc,c.opti)
      gamma.opti<-(pos-c.opti)/nc+1
      # retrain
      rmse10CV[i]<-sqrt(mean((predict(svm(x=train[,predvars],y=log(train$TRX_SCRIPTS+1)
                                          ,scale=FALSE
                                          ,kernel="radial"
                         ,type="eps-regression"
                         ,gamma=gammas[gamma.opti],cost=cs[c.opti]
                         ,fitted=FALSE),test[,predvars])-log(test$TRX_SCRIPTS+1))^2))
      rmse10CVCI95[i]<-(mean(sqrt(ntest/qchisq(c(0.025,0.975),ntest)))-1)*rmse10CV[i]
    }
    rrse[dataset,group]<-sqrt(sum(rmse10CV^2)/ 
                          sum((log(datg$TRX_SCRIPTS+1)-mean(log(datg$TRX_SCRIPTS+1)))^2))
    rrseCI95[dataset,group]<-mean(abs(rrse[dataset,group]-sqrt(sum((rmse10CV-rmse10CVCI95)^2)/ 
                                sum((log(datg$TRX_SCRIPTS+1)-mean(log(datg$TRX_SCRIPTS+1)))^2))),
                                abs(rrse[dataset,group]-sqrt(sum((rmse10CV+rmse10CVCI95)^2)/ 
                                sum((log(datg$TRX_SCRIPTS+1)-mean(log(datg$TRX_SCRIPTS+1)))^2))))
    
  }
}

