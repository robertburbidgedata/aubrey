# time series plots of sales
rm(list=ls())
products <- c("SPIRIVA","JARDIANCE","PRADAXA")

# TRx_1Mo.txt
TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","factor",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
names(TRx_1Mo)

for (product in products) {
  productcc <- paste(substr(product,1,1),tolower(substring(product,2)),sep="")
  TRx_1Mo1 <- TRx_1Mo[substr(TRx_1Mo$PRODUCT,1,6)==substr(productcc,1,6),
                      c("ONEKEY_ID","PERIOD","TRX_UNITS")]
  n <- aggregate(TRx_1Mo1$TRX_UNITS,list(TRx_1Mo1$PERIOD),length)
  names(n) <- c("PERIOD","N")
  n$PERIOD <- paste(n$PERIOD,"01",sep="")
  n$PERIOD <- as.Date(n$PERIOD,format="%Y%m%d")
  y <- aggregate(TRx_1Mo1$TRX_UNITS,list(TRx_1Mo1$PERIOD),sum)
  names(y) <- c("PERIOD","Sales")
  y$PERIOD <- paste(y$PERIOD,"01",sep="")
  y$PERIOD <- as.Date(y$PERIOD,format="%Y%m%d")
  
  # plots
  par(mar=c(5, 4, 4, 4))
  plot(n$PERIOD,n$N,type="l",col="blue",xlab="",ylab="", 
       main=paste("CA/Aubrey No. IDs and Sales:",product))
  par(new=TRUE)
  plot(y$PERIOD, y$Sales/1e6, type="l", axes=FALSE, col="green", xlab="", ylab="No. IDs")
  axis(side=4, at=pretty(range(y$Sales/1e6)))
  mtext("Sales (000,000s)", side=4, line=3)
  legend("topleft",c("No. IDs","Sales"),col=c("blue","green"),lty=1)
  
  # decomposition
  plot(decompose(ts(y$Sales,frequency=12,
                    start=as.numeric(c(format(y$PERIOD[1],"%Y"),format(y$PERIOD[1],"%m"))))))
}