rm(list=ls())

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
names(TRx_1Mo)
levels(TRx_1Mo$PRODUCT)

# drop unneeded products
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Anticoagulants Orals","Bronchial","Diabetes",
                                          "Komboglyze")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# scripts by product by month
sales<-aggregate(TRx_1Mo$TRX_SCRIPTS,by=list(TRx_1Mo$PRODUCT,TRx_1Mo$PERIOD),sum)
names(sales)<-c("PRODUCT","PERIOD","TRX_SCRIPTS")
sales$PERIOD <- as.Date(paste(sales$PERIOD,"01",sep=""),format="%Y%m%d")

# check excl.
i<-sales$PRODUCT=="Jardiance"
plot(sales$PERIOD[i],sales$TRX_SCRIPTS[i],type="l")
i<-sales$PRODUCT=="Jardiance excl combos"
lines(sales$PERIOD[i],sales$TRX_SCRIPTS[i],col="blue")
# latter is subset of former
i<-sales$PRODUCT=="Trajenta"
plot(sales$PERIOD[i],sales$TRX_SCRIPTS[i],type="l")
i<-sales$PRODUCT=="Trajenta excl combos"
lines(sales$PERIOD[i],sales$TRX_SCRIPTS[i],col="blue")
# latter is subset of former
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Jardiance excl combos","Trajenta excl combos")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# check Spiriva
i<-sales$PRODUCT=="Spiriva"
plot(sales$PERIOD[i],sales$TRX_SCRIPTS[i],type="l",ylim=c(0,max(sales$TRX_SCRIPTS[i])))
i<-sales$PRODUCT=="Spiriva Handihaler"
lines(sales$PERIOD[i],sales$TRX_SCRIPTS[i],col="blue")
i<-sales$PRODUCT=="Spiriva Respimat"
lines(sales$PERIOD[i],sales$TRX_SCRIPTS[i],col="green")
# subsets
TRx_1Mo <- TRx_1Mo[!(TRx_1Mo$PRODUCT %in% c("Spiriva Handihaler","Spiriva Respimat")),]
TRx_1Mo<-droplevels(TRx_1Mo)

# look-up table for Comp group
lookup<-data.frame(PRODUCT=levels(TRx_1Mo$PRODUCT),Group=c("RESP_1","CV_1","DIA_2","RESP_1",
                                                           "DIA_2","DIA_1","DIA_1","DIA_2",
                                                           "DIA_1","DIA_1","CV_1","RESP_2",
                                                           "RESP_2","DIA_1","RESP_2","RESP_1",
                                                           "CV_1"))
TRx_1Mo<-merge(TRx_1Mo,lookup,by="PRODUCT")

# scripts by group by month
sales<-aggregate(TRx_1Mo$TRX_SCRIPTS,by=list(TRx_1Mo$Group,TRx_1Mo$PERIOD),sum)
names(sales)<-c("Group","PERIOD","TRX_SCRIPTS")
sales$PERIOD <- as.Date(paste(sales$PERIOD,"01",sep=""),format="%Y%m%d")

# multivariate ts
groups<-levels(sales$Group)
ng<-length(groups)
dat <- sales[sales$Group==groups[1],c("PERIOD","TRX_SCRIPTS")]
names(dat) <- c("PERIOD",groups[1])
for (i in 2:ng) {
  newdat <- sales[sales$Group==groups[i],c("PERIOD","TRX_SCRIPTS")]
  names(newdat) <- c("PERIOD",groups[i])
  dat <- merge(dat,newdat,by="PERIOD",all.x=TRUE)
}

# ts plots
plot(dat$PERIOD,dat[,2]/1e4,type="l",col=2,main="CA Sales",xlab="",ylab="TRX_SCRIPTS (0000s)",
     ylim=c(0,max(max(dat[,2:(ng+1)],na.rm=TRUE)*1.3)/1e4))
for (i in 3:(ng+1)){
  lines(dat$PERIOD,dat[,i]/1e4,col=i)
}
legend("topleft",groups,col=2:(ng+1),lty=1,ncol=5)

# linear interpolate NA for DIA_1
dat$DIA_1[29]<-mean(dat$DIA_1[c(28,30)])

# correct suspect datum in RESP_2
dat$RESP_2[60] <- NA

# ARIMA for NAs
(n <- length(dat$PERIOD))
for(i in 1:ng){
  nas<-is.na(dat[,groups[i]])
  lagnas<-c(FALSE,nas[1:(n-1)])
  predstarts<-(nas)&(!lagnas)
  leadnas<-c(nas[2:n],FALSE)
  predends<-(nas)&(!leadnas)
  narima<-sum(predstarts)
  if(narima){
    for(j in 1:narima){
      ps<-(1:n)[predstarts][j]-1
      if(ps){
        train <- dat[1:ps,groups[i]]
        npred<-(1:n)[predends][j]-(1:n)[predstarts][j]+1
        arima.mod <- arima(train,order=c(1,2,1),seasonal=list(order=c(0,1,1),period=12))
        dat[(1:n)[predstarts][j]:(1:n)[predends][j],groups[i]] <- 
          predict(arima.mod,npred)$pred[1:npred]
      }
    }
  }
}

# sanity check
plot(dat$PERIOD,dat[,2]/1e4,type="l",col=2,main="CA Sales",xlab="",ylab="TRX_SCRIPTS (0000s)",
     ylim=c(0,max(max(dat[,2:(ng+1)],na.rm=TRUE)*1.3)/1e4))
for (i in 3:(ng+1)){
  lines(dat$PERIOD,dat[,i]/1e4,col=i)
}
legend("topleft",groups,col=2:(ng+1),lty=1,ncol=5)

# ARIMA forecasting
apply(!is.na(dat[,groups]),2,sum)
ntrain <- 24
ntest <- 12
ntrialsmax <- n - (ntrain + ntest) + 1
rmse <- matrix(NA,ntrialsmax,ng)
for(g in 1:ng) {
  idx<-(1:n)[!is.na(dat[,groups[g]])]
  ntrials <- length(idx) - (ntrain + ntest) + 1
  for(i in 1:ntrials) {
    train <- dat[idx[i:(i+ntrain-1)],groups[g]]
    test <- dat[idx[(i+ntrain):(i+ntrain+ntest-1)],groups[g]]
    arima.mod <- try(arima(train,order=c(1,1,1)),
                     silent=TRUE)
    if (class(arima.mod)=="try-error")
      rmse[i,g] <- NA
    else
      rmse[i,g] <- sqrt(mean((predict(arima.mod,ntest)$pred[1:ntest]-test)^2))/mean(test)
  }
}

colnames(rmse)<-groups
(res<-data.frame(mean=apply(rmse,2,mean,na.rm=TRUE),
                 se=apply(rmse,2,sd,na.rm=TRUE)/sqrt(apply(!is.na(rmse),2,sum))))
i<-!is.na(res$se)
mean(res$mean[i])
sqrt(sum(res$se[i]^2))/sum(i)/sqrt(sum(i))

# VAR forecasting
library(vars)
ntrain <- 24
ntest <- 12
ntrialsmax <- n - (ntrain + ntest) + 1
rmse <- matrix(NA,ntrialsmax,ng)
for(i in 1:ntrials) {
  train <- dat[i:(i+ntrain-1),groups]
  test <- dat[(i+ntrain):(i+ntrain+ntest-1),groups]
  vars<-groups[apply(!is.na(train[,groups]),2,sum)==ntrain]
  var.mod <- VAR(train[,vars],p=1,season=12,type="both")
  fcst<-predict(var.mod,n.ahead=12)$fcst
  nv<-length(vars)
  fcstfcst<-matrix(0,ntest,nv)
  for(j in 1:nv){
    fcstfcst[,j]<-fcst[[vars[j]]][,"fcst"]
  }
  rmse[i,] <- sqrt(apply((fcstfcst-test)^2,2,mean))/apply(test,2,mean)
}

# pretty bad apart from last four trials where all groups active
colnames(rmse)<-groups
idx<-(ntrials-3):ntrials
(res<-data.frame(mean=apply(rmse[idx,],2,mean),
                 se=apply(rmse[idx,],2,sd)/sqrt(length(idx))))
mean(res$mean)
sqrt(sum(res$se^2))/ng/sqrt(sum(ng))




