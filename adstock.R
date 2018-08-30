# ADSTOCK CA Sales vs Engagement
# (this code has become slightly mangled and may need edited)

rm(list=ls())

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
print(dim(TRx_1Mo))
names(TRx_1Mo)
TRx_1Mo$PERIOD <- as.Date(paste(TRx_1Mo$PERIOD,"01",sep=""),format="%Y%m%d")
sales <- aggregate(TRx_1Mo$TRX_SCRIPTS,by=TRx_1Mo[,c("PERIOD","PRODUCT")],sum)
names(sales) <- c("PERIOD","PRODUCT","TRX_SCRIPTS")
summary(sales)
(products<-levels(sales$PRODUCT))
TRx_1Mo <- TRx_1Mo[,c("PRODUCT","PERIOD","TRX_SCRIPTS")]

mainproducts <- c("Pradax","Spiriva","Xarelto","Anticoagulants Orals","Bronchial")
np <- length(mainproducts)

# multivariate ts
dat <- sales[sales$PRODUCT==mainproducts[1],c("PERIOD","TRX_SCRIPTS")]
names(dat) <- c("PERIOD",mainproducts[1])
mainproductsnames<-mainproducts
for (i in 2:np) {
  name <- mainproducts[i]
  newdat <- sales[sales$PRODUCT==name,c("PERIOD","TRX_SCRIPTS")]
  validname <- sub(" ",".",name)
  validname <- sub("-",".",name)
  names(newdat) <- c("PERIOD",validname)
  mainproductsnames[i]<-validname
  dat <- merge(dat,newdat,by="PERIOD",all.x=TRUE)
}
(n <- length(dat$PERIOD))

# ARIMA for NAs
(n <- length(dat$PERIOD))
for(i in 1:np){
  nas<-is.na(dat[,mainproducts[i]])
  lagnas<-c(FALSE,nas[1:(n-1)])
  predstarts<-(nas)&(!lagnas)
  leadnas<-c(nas[2:n],FALSE)
  predends<-(nas)&(!leadnas)
  narima<-sum(predstarts)
  if(narima){
    for(j in 1:narima){
      train <- dat[1:((1:n)[predstarts][j]-1),mainproducts[i]]
      npred<-(1:n)[predends][j]-(1:n)[predstarts][j]+1
      arima.mod <- arima(train,order=c(1,2,1),seasonal=list(order=c(0,1,1),period=12))
      dat[(1:n)[predstarts][j]:(1:n)[predends][j],mainproducts[i]] <- 
        predict(arima.mod,npred)$pred[1:npred]
    }
  }
}

# interpolate to daily
datd<-data.frame(Date=seq(from=dat$PERIOD[1],
                          to=seq(from=dat$PERIOD[n],length.out=2,by="month")[2]-1,by="day"))
datd$PERIOD<-as.Date(paste(format(datd$Date,format="%Y%m"),"01",sep=""),format="%Y%m%d")
datd<-merge(datd,dat,by="PERIOD")
datd$days<-as.numeric(lapply(datd$PERIOD,function(x) seq(from=x,length.out=2,by="month")[2]-x))
datd[,mainproducts]<-datd[,mainproducts]/datd$days
nd<-length(datd$Date)

for (name in mainproducts){
  xy<-spline(x=datd$Date,y=datd[,name],n=nd/30.5)
  #plot(xy$x,xy$y,type="l")
  datd[,paste(name,"i")]<-spline(xy$x,xy$y,n=nd)$y
  #plot(datd$Date,datd$`Pradax i`,type="l")
}

# sanity check
plot(datd$Date,datd[,9],type="l",col=2,main="CA Sales",xlab="",ylab="TRX_SCRIPTS (00,000s)",
     ylim=c(0,max(max(datd[,9:(9+np-1)],na.rm=TRUE)*1.3)))
for (i in 10:(9+np-1)){
  lines(datd$Date,datd[,i],col=i-7)
}
legend("topleft",mainproducts,col=2:(np+1),lty=1,ncol=3)

# Engagement_Info
engagement_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Info.txt",
                              header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor",
                                                                "factor","Date","factor","factor",
                                                                "factor","factor","factor",
                                                                "factor","factor","factor","factor",
                                                                "factor"))
names(engagement_info)
dim(engagement_info)
summary(engagement_info)
engagements <- aggregate(engagement_info$ONEKEY_ID,
                         engagement_info[,c("CALL_DATE","PRODUCT_DETAILED")],length)
names(engagements) <- c("CALL_DATE","PRODUCT_DETAILED","Calls")
summary(engagements)
levels(engagements$PRODUCT_DETAILED)
mainproducts

# merge
spiriva<-merge(datd[,c("Date","Spiriva i")],
              engagements[engagements$PRODUCT_DETAILED=="SPIRIVA",c("CALL_DATE","Calls")],
              by.x="Date",by.y="CALL_DATE")
names(spiriva)<-c("Date","Spiriva Sales","Spiriva Calls")
pradaxa<-merge(datd[,c("Date","Pradax i")],
               engagements[engagements$PRODUCT_DETAILED=="PRADAXA",c("CALL_DATE","Calls")],
               by.x="Date",by.y="CALL_DATE")
names(pradaxa)<-c("Date","Pradaxa Sales","Pradaxa Calls")

# ts plot Spiriva
par(mar=c(5, 4, 4, 4))
plot(spiriva$Date,spiriva$`Spiriva Sales`,type="l",col=3,xlab="",ylab="TRX_SCRIPTS", 
     main="CA/Aubrey: Sales and Engagements: Spiriva",ylim=c(0,max(spiriva$`Spiriva Sales`)*1.25))
par(new=TRUE)
plot(spiriva$Date, spiriva$`Spiriva Calls`, type="h", axes=FALSE, col=3, xlab="", ylab="")
axis(side=4, at=pretty(c(0,max(spiriva$`Spiriva Calls`))))
mtext("Engagements", side=4, line=3)
legend("topright",c("TRX_SCRIPTS","Engagements"),col=3,lty=c(1,0),pch=c(NA,"|"))

# adstock Spiriva
adstock<-function(x,rate=0){
  return(as.numeric(filter(x=x,filter=rate,method="recursive")))
}

# AdstockRate<-function(Impact,Ads){
#   modFit<-nls(Impact~a+b*adstock(Ads,rate),
#               start=c(a=1,b=1,rate=0.5))
#   if(summary(modFit)$coefficients[3,1]>0){
#     AdstockRate=summary(modfit)$coefficients[3,1]
#   }
#   else{
#     library(minpack.lm)
#     nls.out<-nlsLM(Impact~a+b*adstock(Ads,rate),start=list(a=1,b=1,rate=0.5),
#                    lower=c(a=0,b=0,rate=0),upper=c(a=Inf,b=Inf,rate=1)) 
#     AdstockRate=summary(nls.out)$coefficients[3,1]
#   }
#   return(AdstockRate)
# }
# 
# (phi<-AdstockRate(spiriva$`Spiriva Sales`,spiriva$`Spiriva Calls`))

y<-spiriva$`Spiriva Sales`
x<-spiriva$`Spiriva Calls`
modFit<-nls(y~a+b*adstock(x,phi),start=c(a=1,b=1,phi=0.5))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

lm1<-lm(y~adstock(x,phi))
summary(lm1)
plot(lm1)
plot(spiriva$Date,lm1$residuals,type="p")
# poorly specified model, suggests downtrending exogenous factor

t<-as.numeric(spiriva$Date-spiriva$Date[1])

########## 

# ts plot Pradaxa
par(mar=c(5, 4, 4, 4))
plot(pradaxa$Date,pradaxa$`Pradaxa Sales`,type="l",col=2,xlab="",ylab="TRX_SCRIPTS", 
     main="CA/Aubrey: Sales and Engagements: Pradaxa",ylim=c(0,max(pradaxa$`Pradaxa Sales`)*1.25))
par(new=TRUE)
plot(pradaxa$Date, pradaxa$`Pradaxa Calls`, type="h", axes=FALSE, col=2, xlab="", ylab="")
axis(side=4, at=pretty(c(0,max(pradaxa$`Pradaxa Calls`))))
mtext("Engagements", side=4, line=3)
legend("topright",c("TRX_SCRIPTS","Engagements"),col=2,lty=c(1,0),pch=c(NA,"|"))

# adstock Pradaxa
adstock<-function(x,rate=0){
  return(as.numeric(filter(x=x,filter=rate,method="recursive")))
}

y<-pradaxa$`Pradaxa Sales`
x<-pradaxa$`Pradaxa Calls`
modFit<-nls(y~a+b*adstock(x,phi),start=c(a=1500,b=0.1,phi=0.99))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

lm1<-lm(y~adstock(x,phi))
summary(lm1)
plot(lm1)
plot(pradaxa$Date,lm1$residuals,type="p")
# poorly specified model, suggests downtrending exogenous factor

t<-as.numeric(pradaxa$Date-pradaxa$Date[1])
modFit<-nls(y~a+b*adstock(x,phi)+bt*t,start=c(a=1172,b=0.1,bt=0.1,phi=0.93))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

lm2<-lm(y~adstock(x,phi)+t)
summary(lm2)
plot(lm2)
plot(spiriva$Date,lm2$residuals,type="p")
# better, although residuals are autocorrelated

# modFit<-nls(y~a+b*adstock(x,phi)+bt*t,start=c(a=3656,b=0.13,bt=0.1,phi=0.9))
library(minpack.lm)
modFit<-nlsLM(y~a+b*adstock(x,phi)+bt*t,start=list(a=1100,b=0.1,bt=0.01,phi=0.93),
              lower=c(a=0,b=0,bt=0,phi=0),upper=c(a=Inf,b=Inf,bt=Inf,phi=1))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

# try log
modFit<-nlsLM(log(y)~a+b*adstock(x,phi),start=list(a=log(1100),b=log(0.1),phi=0.93),
              lower=c(a=-Inf,b=-Inf,phi=0),upper=c(a=Inf,b=Inf,phi=1))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

logy<-log(y)
lm2<-lm(logy~adstock(x,phi))
summary(lm2)
plot(lm2)
plot(pradaxa$Date,lm2$residuals,type="p")

# try log-log
modFit<-nlsLM(log(y)~a+b*adstock(log(x),phi),start=list(a=log(1100),b=0.1,phi=0.93),
              lower=c(a=-Inf,b=0,phi=0),upper=c(a=Inf,b=Inf,phi=1))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

logx<-log(x)
lm2<-lm(logy~adstock(logx,phi))
summary(lm2)
plot(lm2)
plot(pradaxa$Date,lm2$residuals,type="p")

# try sqrt
modFit<-nlsLM(sqrt(y)~a+b*adstock(x,phi),start=list(a=sqrt(1100),b=0.1,phi=0.93),
              lower=c(a=0,b=0,phi=0),upper=c(a=Inf,b=Inf,phi=1))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

sqrty<-sqrt(y)
lm2<-lm(sqrty~adstock(x,phi))
summary(lm2)
plot(lm2)
plot(pradaxa$Date,lm2$residuals,type="p")

# try sqrt & trend
modFit<-nlsLM(sqrt(y)~a+b*adstock(x,phi)+bt*t,start=list(a=sqrt(900),b=0.1,bt=0,phi=0.93),
              lower=c(a=0,b=0,bt=0,phi=0),upper=c(a=Inf,b=Inf,bt=Inf,phi=1))
summary(modFit)$coefficients
phi<-summary(modFit)$coefficients["phi","Estimate"]

sqrty<-sqrt(y)
lm2<-lm(sqrty~adstock(x,phi))
summary(lm2)
plot(lm2)
plot(pradaxa$Date,lm2$residuals,type="p")
