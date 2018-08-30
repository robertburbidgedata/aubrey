# VAR on CA Sales by product

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

# gant chart
barplot(as.numeric(aggregate(sales$PERIOD,by=list(sales$PRODUCT),max)$x),
        names.arg=products,horiz=TRUE,
        col="black",axisnames=FALSE,xlim=as.numeric(c(min(sales$PERIOD),max(sales$PERIOD))))
par(new=FALSE)
barplot(as.numeric(aggregate(sales$PERIOD,by=list(sales$PRODUCT),min)$x),
        names.arg=products,horiz=TRUE,
        col="white",axisnames=FALSE)

library(DiagrammeR)
df <- data.frame(task=products,
                 status="done",
                 start=aggregate(sales$PERIOD,by=list(sales$PRODUCT),min)$x,
                 end=aggregate(sales$PERIOD,by=list(sales$PRODUCT),max)$x)
library(tidyr)
library(dplyr)
# m<-mermaid(
#   paste0(
#     # mermaid "header", each component separated with "\n" (line break)
#     "gantt", "\n", 
#     "dateFormat  YYYY-MM-DD", "\n", 
#     "title CA Products", "\n",
#     # unite the first two columns (task & status) and separate them with ":"
#     # then, unite the other columns and separate them with ","
#     # this will create the required mermaid "body"
#     paste(df %>%
#             unite(i, task, status, sep = ":") %>%
#             unite(j, i, start, end, sep = ",") %>%
#             .$j, 
#           collapse = "\n"
#     ), "\n"
#   )
# )
# m$x$config = list(ganttConfig = list(
#   axisFormatter = list(list(
#     "%b %d, %Y" 
#     ,htmlwidgets::JS(
#       'function(d){ return d.getDay() == 1 }' 
#     )
#   ))
# ))

library(timevis)
library(knitr)
library(rmarkdown)

data <- data.frame(
  id      = 1:length(products),
  content = products,
  start   = aggregate(sales$PERIOD,by=list(sales$PRODUCT),min)$x,
  end     = aggregate(sales$PERIOD,by=list(sales$PRODUCT),max)$x
)

timevis(data)
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

# ts plots
plot(dat$PERIOD,dat[,2]/1e5,type="l",col=2,main="CA Sales",xlab="",ylab="TRX_SCRIPTS (00,000s)",
     ylim=c(0,max(max(dat[,2:(np+1)],na.rm=TRUE)*1.3)/1e5))
for (i in 3:(np+1)){
  lines(dat$PERIOD,dat[,i]/1e5,col=i)
}
legend("topleft",mainproducts,col=2:(np+1),lty=1,ncol=3)

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

# sanity check
plot(dat$PERIOD,dat[,2]/1e5,type="l",col=2,main="CA Sales",xlab="",ylab="TRX_SCRIPTS (00,000s)",
     ylim=c(0,max(max(dat[,2:(np+1)],na.rm=TRUE)*1.3)/1e5))
for (i in 3:(np+1)){
  lines(dat$PERIOD,dat[,i]/1e5,col=i)
}
legend("topleft",mainproducts,col=2:(np+1),lty=1,ncol=3)

# VAR
ntrain <- 36
ntest <- 12
(ntrials <- n - (ntrain + ntest) + 1)
rmse <- numeric(ntrials)

library(vars)
rmse <- matrix(NA,ntrials,np)
for(i in 1:ntrials) {
  train <- dat[i:(i+ntrain-1),2:(np+1)]
  test <- dat[(i+ntrain):(i+ntrain+ntest-1),2:(np+1)]
  var.mod <- VAR(train,p=1,season=12,type="both")
  fcst<-predict(var.mod,n.ahead=12)$fcst
  fcstfcst<-matrix(0,ntest,np)
  for(j in 1:np){
    fcstfcst[,j]<-fcst[[mainproductsnames[j]]][,"fcst"]
  }
  rmse[i,] <- sqrt(apply((fcstfcst-test)^2,2,mean))/apply(test,2,mean)
}
colnames(rmse)<-mainproducts
data.frame(mean=apply(rmse,2,mean),se=apply(rmse,2,sd)/sqrt(ntrials))
