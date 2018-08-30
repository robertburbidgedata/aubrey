### clustering of Aubrey data: demographics & census
### exc. CVM

rm(list=ls())

# SPIRIVA, 2013
product <- "SPIRIVA"
year <- 2013

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
print(dim(demographics))
names(demographics)
summary(demographics)

# subset
demographics <- demographics[demographics$GRADUATION_DATE<=year&
                               !is.na(demographics$ONEKEY_ID)&
                               demographics$GENDER!='U',]
demographics <- droplevels(demographics)

# drop redundant fields and check values
levels(demographics$ACCOUNT_TYPE)
demographics$ACCOUNT_TYPE <- NULL
demographics$NAME <- NULL
levels(demographics$PRIMARY_SPECIALTY)
levels(demographics$HCP_TYPE)
levels(demographics$HCP_CLASSIFICATION)
levels(demographics$ACCOUNT_STATUS)
demographics$ACCOUNT_STATUS <- NULL
demographics$LASTNAME <- NULL
demographics$FIRSTNAME <- NULL
demographics$MIDNAME <- NULL
levels(demographics$SCI_EXPERT)
demographics$FINDER_ID <- NULL
levels(demographics$GP_SPECIALISTS)
levels(demographics$GENDER)
levels(demographics$SECONDARY_SPECIALTY)
levels(demographics$PRIMARY_LANGUAGE)
demographics$PRIMARY_LANGUAGE[demographics$PRIMARY_LANGUAGE=="English"] <- "en"
demographics <- droplevels(demographics)
levels(demographics$MD_CONNECT)
demographics$MD_CONNECT <- NULL
print(dim(demographics))
summary(demographics)

# FSA2
demographics$FSA2 <- factor(substr(demographics$FSA,1,2))
length(levels(demographics$FSA))

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
                   c("ONEKEY_ID","PERIOD","TRX_SCRIPTS")]
TRx_1Mo$year <- as.numeric(substr(as.character(TRx_1Mo$PERIOD),1,4))
table(TRx_1Mo$year)
TRx_1Mo$quarter <- as.numeric(paste(substr(as.character(TRx_1Mo$PERIOD),1,4),
      ifelse(ceiling(as.numeric(substr(as.character(TRx_1Mo$PERIOD),5,6))/3.0)*3<10,"0",""),
      ceiling(as.numeric(substr(as.character(TRx_1Mo$PERIOD),5,6))/3.0)*3,sep=""))
TRx_3Mo <- aggregate(TRx_1Mo$TRX_SCRIPTS,list(TRx_1Mo$ONEKEY_ID,TRx_1Mo$quarter),sum)
names(TRx_3Mo) <- c("ONEKEY_ID","quarter","TRX_SCRIPTS")
table(TRx_3Mo$quarter)
nextperiod <- format(tail(seq(as.Date(paste(year,12,31,sep="-"),format="%Y-%m-%d"),
                              by="month",length=4),1),"%Y%m")
TRx_3Mo <- TRx_3Mo[TRx_3Mo$quarter==nextperiod,c("ONEKEY_ID","TRX_SCRIPTS")]
dim(TRx_3Mo)

# merge everything
dat <- merge(demographics, TRx_3Mo, by="ONEKEY_ID")
print(dim(dat))

# predictive vars
summary(dat)
print(length(levels(dat$FSA)))

dat$years <- year-dat$GRADUATION_DATE
dat$GRADUATION_DATE <- NULL
#dat$FSA <- NULL

# COPD prevalence 2013
# NAHVSMDB57:copd.COPD_BOTH_OVER12_PRC
copd2 <- data.frame(PROVINCE=c("Alberta","British Columbia","Manitoba","New Brunswick",
                              "Newfoundland","Nova Scotia","Nunavut","NWT","Ontario",
                              "Prince Edward Island","Quebec","Saskatchewan","Yukon"),
                   copd2=c(3.8,3.6,3.6,5.2,NA,5.9,NA,NA,3.8,5.7,4.5,4.8,5.6))
dat <- merge(dat,copd2,by="PROVINCE")
dat$copd2[is.na(dat$copd2)] <- mean(dat$copd2,na.rm=TRUE)

# OHE
for (var in discrete_vars) {
  dat <- cbind(dat,model.matrix(as.formula(paste("~",var,"-1")),dat))
  dat[,var] <- NULL
}
names(dat)
dat$SECONDARY_SPECIALTY <- NULL

# Aggregate to FSA
dat_agg <- aggregate(dat[,setdiff(names(dat),c("PROVINCE", "ONEKEY_ID","FSA2","FSA","TRX_SCRIPTS"))],
                     list(dat$FSA),mean)
y <- aggregate(dat$TRX_SCRIPTS,list(dat$FSA),sum)                     
dat_agg <- merge(dat_agg,y,by.x="Group.1",by.y="Group.1")
names(dat_agg)[1] <- "FSA"
(ld <- dim(dat_agg))
names(dat_agg)[ld[2]] <- "TRX_SCRIPTS"

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

#income$FSA2 <- substr(income$FSA,1,2)
income1 <- aggregate(income$Total,list(income$FSA),mean)
names(income1) <- c("FSA","Income")

dat_agg <- merge(dat_agg,income1,by="FSA")
dat_agg$Income[is.na(dat_agg$Income)] <- mean(dat_agg$Income[!is.na(dat_agg$Income)])
dat_agg$Income <- dat_agg$Income/max(dat_agg$Income) 

# age
age <- census[census$Attribute=="Average age of the population",]
age$Attribute <- NULL

#age$FSA2 <- substr(age$FSA,1,2)
age1 <- aggregate(age$Total,list(age$FSA),mean)
names(age1) <- c("FSA","Age")
dat_agg <- merge(dat_agg,age1,by="FSA")
dat_agg$Age[is.na(dat_agg$Age)] <- mean(dat_agg$Age[!is.na(dat_agg$Age)])
dat_agg$Age <- dat_agg$Age/max(dat_agg$Age) 

# Population, 2016
pop <- census[census$Attribute=="Population, 2016",]
pop$Attribute <- NULL

#pop$FSA2 <- substr(pop$FSA,1,2)
pop1 <- aggregate(pop$Total,list(pop$FSA),mean)
names(pop1) <- c("FSA","Pop")
dat_agg <- merge(dat_agg,pop1,by="FSA")
dat_agg$Pop[is.na(dat_agg$Pop)] <- mean(dat_agg$Pop[!is.na(dat_agg$Pop)])
dat_agg$Pop <- dat_agg$Pop/max(dat_agg$Pop) 

# k-means clustering
names(dat_agg)
ncenters <- 10
nstart <- 25
tot.withinss <- numeric(ncenters)
clustervars <- setdiff(names(dat_agg),c("FSA","FSA2","TRX_SCRIPTS"))
for (centers in 1:ncenters) {
  tot.withinss[centers] <- kmeans(dat_agg[,clustervars],
                                  centers,nstart=nstart)$tot.withinss
}
plot(1:ncenters,tot.withinss,type="l",xlab="No. centers",
     ylab="Total within cluster sum of squares")

# elbow
theta <- 180-(atan(-(tot.withinss[2:ncenters]/10000-tot.withinss[1:(ncenters-1)]/10000)/
  (2:ncenters-1:(ncenters-1)))/pi*180)
kopt <- which.max(theta[2:(ncenters-1)]-theta[1:(ncenters-2)])+1
km <- kmeans(dat_agg[,clustervars],
             kopt,nstart=nstart)
print(km$cluster)

dat_agg <- cbind(dat_agg,km$cluster)

# plot in PCA space
pca <- eigen(cov(dat_agg[,clustervars]))
dat_agg$pc1 <- as.matrix(dat_agg[,clustervars])%*%as.matrix(pca$vectors[,1])
dat_agg$pc2 <- as.matrix(dat_agg[,clustervars])%*%as.matrix(pca$vectors[,2])
sales.med <- median(dat_agg$TRX_SCRIPTS)
xmin <- min(dat_agg$pc1)-abs(min(dat_agg$pc1))/10
xmax <- max(dat_agg$pc1)+abs(max(dat_agg$pc1))/10
ymin <- min(dat_agg$pc2)-abs(min(dat_agg$pc2))/10
ymax <- max(dat_agg$pc2)+abs(max(dat_agg$pc2))/10
i <- dat_agg$TRX_SCRIPTS>=sales.med & dat_agg$`km$cluster`==1
plot(dat_agg$pc1[i],dat_agg$pc2[i],type="p",col="green",pch=0,xlab="PC1",ylab="PC2",
     main="k-means clustering in PC-space",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
i <- dat_agg$TRX_SCRIPTS>=sales.med & dat_agg$`km$cluster`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="green",pch=1)
i <- dat_agg$TRX_SCRIPTS<sales.med & dat_agg$`km$cluster`==1
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=0)
i <- dat_agg$TRX_SCRIPTS<sales.med & dat_agg$`km$cluster`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=1)

legend("bottomleft",c("Cluster 1","Cluster 2","High sales","Low sales"),
       col=c("black","black","green","red"),pch=c(0,1,13,13),ncol=2)

# means test
(xbar1 <- mean(dat_agg$TRX_SCRIPTS[dat_agg$`km$cluster`==1]))
s1 <- sd(dat_agg$TRX_SCRIPTS[dat_agg$`km$cluster`==1])
(n1 <- sum(dat_agg$`km$cluster`==1))
print(s1/sqrt(n1))
(xbar2 <- mean(dat_agg$TRX_SCRIPTS[dat_agg$`km$cluster`==2]))
s2 <- sd(dat_agg$TRX_SCRIPTS[dat_agg$`km$cluster`==2])
(n2 <- sum(dat_agg$`km$cluster`==2))
print(s2/sqrt(n2))

(t <- (xbar1-xbar2)/sqrt(s1^2/n1+s2^2/n2))
alpha = .05 
nu <- (s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
t.half.alpha = qt(1-alpha/2, df=nu-1) 
c(-t.half.alpha, t.half.alpha) 

# pam
library(cluster)

avg.width <- numeric(ncenters)
avg.width[1] <- NA

for (centers in 2:ncenters) {
  avg.width[centers] <- pam(dat_agg[,clustervars],
                                  centers)$silinfo$avg.width
}
plot(1:ncenters,avg.width,type="l",xlab="No. centers",
     ylab="Average width")

kopt <- which.max(avg.width)
pam1 <- pam(dat_agg[,clustervars],kopt)
print(pam1$clustering)

dat_agg <- cbind(dat_agg,pam1$clustering)

# plot in PCA space
pca <- eigen(cov(dat_agg[,clustervars]))
dat_agg$pc1 <- as.matrix(dat_agg[,clustervars])%*%as.matrix(pca$vectors[,1])
dat_agg$pc2 <- as.matrix(dat_agg[,clustervars])%*%as.matrix(pca$vectors[,2])
sales.med <- median(dat_agg$TRX_SCRIPTS)
xmin <- min(dat_agg$pc1)-abs(min(dat_agg$pc1))/10
xmax <- max(dat_agg$pc1)+abs(max(dat_agg$pc1))/10
ymin <- min(dat_agg$pc2)-abs(min(dat_agg$pc2))/10
ymax <- max(dat_agg$pc2)+abs(max(dat_agg$pc2))/10
i <- dat_agg$TRX_SCRIPTS>=sales.med & dat_agg$`pam1$clustering`==1
plot(dat_agg$pc1[i],dat_agg$pc2[i],type="p",col="green",pch=0,xlab="PC1",ylab="PC2",
     main="k-medoid clustering in PC-space",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
i <- dat_agg$TRX_SCRIPTS>=sales.med & dat_agg$`pam1$clustering`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="green",pch=1)
i <- dat_agg$TRX_SCRIPTS<sales.med & dat_agg$`pam1$clustering`==1
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=0)
i <- dat_agg$TRX_SCRIPTS<sales.med & dat_agg$`pam1$clustering`==2
points(dat_agg$pc1[i],dat_agg$pc2[i],col="red",pch=1)

legend("bottomleft",c("Cluster 1","Cluster 2","High sales","Low sales"),
       col=c("black","black","green","red"),pch=c(0,1,13,13),ncol=2)

# means test

(xbar1 <- mean(dat_agg$TRX_SCRIPTS[dat_agg$`pam1$clustering`==1]))
s1 <- sd(dat_agg$TRX_SCRIPTS[dat_agg$`pam1$clustering`==1])
(n1 <- sum(dat_agg$`pam1$clustering`==1))
print(s1/sqrt(n1))
(xbar2 <- mean(dat_agg$TRX_SCRIPTS[dat_agg$`pam1$clustering`==2]))
s2 <- sd(dat_agg$TRX_SCRIPTS[dat_agg$`pam1$clustering`==2])
(n2 <- sum(dat_agg$`pam1$clustering`==2))
print(s2/sqrt(n2))

(t <- (xbar1-xbar2)/sqrt(s1^2/n1+s2^2/n2))
alpha = .05 
nu <- (s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
t.half.alpha = qt(1-alpha/2, df=nu-1) 
c(-t.half.alpha, t.half.alpha) 

# # spectral clustering
# library(kernlab)
# 
# clustervars <- setdiff(names(dat_agg),
#                        c("FSA2","TRX_SCRIPTS","km$cluster","pc1","pc2","pam1$clustering"))
# ncenters <- 10
# tot.withinss <- numeric(ncenters)
# tot.withinss[1] <- NA
# 
# for (centers in 2:ncenters) {
#   tot.withinss[centers] <- sum(specc(dat_agg[,clustervars],centers=centers,
#                                      kernel="stringdot")$withinss)
# }
# plot(1:ncenters,tot.withinss,type="l",xlab="No. centers",
#      ylab="Total within cluster sum of squares")
# 
# # package not properly implemented

# sammon
library(MASS)
sam1 <- sammon(dist(dat_agg[,clustervars]))

sales.med <- median(dat_agg$TRX_SCRIPTS)
xmin <- min(sam1$points[,1])-abs(min(sam1$points[,1]))/10
xmax <- max(sam1$points[,1])+abs(max(sam1$points[,1]))/10
ymin <- min(sam1$points[,2])-abs(min(sam1$points[,2]))/10
ymax <- max(sam1$points[,2])+abs(max(sam1$points[,2]))/10
i <- dat_agg$TRX_SCRIPTS>=sales.med 
plot(sam1$points[i,1],sam1$points[i,2],type="p",col="green",pch=0,xlab="Sammon1",ylab="Sammon2",
     main="Sammon mapping",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
i <- dat_agg$TRX_SCRIPTS<sales.med
points(sam1$points[i,1],sam1$points[i,2],col="red",pch=0)
legend("bottomleft",c("High sales","Low sales"),
       col=c("green","red"),pch=0,ncol=2)

# kernel pca & k-means
library(kernlab)
kpca1 <- kpca(~.,data=dat_agg[,clustervars],kernel="rbfdot",kpar=list(sigma=1/length(clustervars)))
kpca1 <- kpca(~.,data=dat_agg[,clustervars],kernel="anovadot",
              kpar=list(sigma=1/length(clustervars),degree=4))

rot <- rotated(kpca1)

ncenters <- 10
nstart <- 25
tot.withinss <- numeric(ncenters)
for (centers in 1:ncenters) {
  tot.withinss[centers] <- kmeans(rot,
                                  centers,nstart=nstart)$tot.withinss
}
plot(1:ncenters,tot.withinss,type="l",xlab="No. centers",
     ylab="Total within cluster sum of squares")

# elbow
theta <- 180-(atan(-(tot.withinss[2:ncenters]/1e10-tot.withinss[1:(ncenters-1)]/1e10)/
                     (2:ncenters-1:(ncenters-1)))/pi*180)
kopt <- which.max(theta[2:(ncenters-1)]-theta[1:(ncenters-2)])+1
kerkm <- kmeans(rot,
             kopt,nstart=nstart)
print(kerkm$cluster)
npc <- dim(rot)[2]

rot <- cbind(rot,kerkm$cluster)

# plot in kernel PCA space
sales.med <- median(dat_agg$TRX_SCRIPTS)
xmin <- min(rot[,1])-abs(min(rot[,1]))/10
xmax <- max(rot[,1])+abs(max(rot[,1]))/10
ymin <- min(rot[,2])-abs(min(rot[,2]))/2
ymax <- max(rot[,2])+abs(max(rot[,2]))/10
i <- dat_agg$TRX_SCRIPTS>=sales.med & rot[,npc+1]==1
plot(rot[i,1],rot[i,2],type="p",col="green",pch=0,xlab="PC1",ylab="PC2",
     main="k-means clustering in kernel PC-space",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
i <- dat_agg$TRX_SCRIPTS>=sales.med & rot[,npc+1]==2
points(rot[i,1],rot[i,2],col="green",pch=1)
i <- dat_agg$TRX_SCRIPTS<sales.med & rot[,npc+1]==1
points(rot[i,1],rot[i,2],col="red",pch=0)
i <- dat_agg$TRX_SCRIPTS<sales.med & rot[,npc+1]==2
points(rot[i,1],rot[i,2],col="red",pch=1)
legend("bottomleft",c("Cluster 1","Cluster 2","High sales","Low sales"),
       col=c("black","black","green","red"),pch=c(0,1,13,13),ncol=4)

# means test
(xbar1 <- mean(dat_agg$TRX_SCRIPTS[rot[,npc+1]==1]))
(n1 <- sum(rot[,npc+1]==1))
s1 <- sd(dat_agg$TRX_SCRIPTS[rot[,npc+1]==1])
s1/sqrt(n1)
(xbar2 <- mean(dat_agg$TRX_SCRIPTS[rot[,npc+1]==2]))
(n2 <- sum(rot[,npc+1]==2))
s2 <- sd(dat_agg$TRX_SCRIPTS[rot[,npc+1]==2])
s2/sqrt(n2)

(t <- (xbar1-xbar2)/sqrt(s1^2/n1+s2^2/n2))
alpha = .05 
nu <- (s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
t.half.alpha = qt(1-alpha/2, df=nu-1) 
c(-t.half.alpha, t.half.alpha) 

# more features?
levels(census$Attribute)

# # smoking
# # https://www.statcan.gc.ca/tables-tableaux/sum-som/l01/cst01/health74b-eng.htm
# copd2 <- data.frame(PROVINCE=c("Alberta","British Columbia","Manitoba","New Brunswick",
#                                "Newfoundland","Nova Scotia","Nunavut","NWT","Ontario",
#                                "Prince Edward Island","Quebec","Saskatchewan","Yukon"),
#                     copd2=c(3.8,3.6,3.6,5.2,NA,5.9,NA,NA,3.8,5.7,4.5,4.8,5.6))
# dat <- merge(dat,copd2,by="PROVINCE")
# dat$copd2[is.na(dat$copd2)] <- mean(dat$copd2,na.rm=TRUE)
 
# transforms?
for (var in clustervars) {
  x <- dat_agg[,var]
  y <- dat_agg$TRX_SCRIPTS
  plot(x,y,type="p",xlab=var,ylab="TRX_SCRIPTS")
}


