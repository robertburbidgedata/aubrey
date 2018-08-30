# missing years - sales vs years?
startmth<-as.Date("2016-09-01")
endmth<-as.Date("2017-08-01")

### predictive features for all ONEKEY_IDs on demographics
demographics <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Demographics.txt",
                           header=TRUE, sep="|",colClasses=c("factor","factor","character","factor",
                                                             "factor","factor","factor","character",
                                                             "character","character","factor",
                                                             "factor","factor","factor","numeric",
                                                             "factor","factor","factor","factor",
                                                             "factor"))

demographics$years <- 2017-demographics$GRADUATION_DATE

TRx_1Mo <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/TRx_1Mo.txt",
                      header=TRUE, sep="\t",colClasses=c("factor","factor","character",
                                                         "numeric","numeric","numeric",
                                                         "factor","factor"))
TRx_1Mo$PRODUCT_CODE<-NULL
TRx_1Mo$FINDER_ID<-NULL
TRx_1Mo$MONTH<-as.Date(paste(TRx_1Mo$PERIOD,"01",sep=""),format="%Y%m%d")
TRx_1Mo$PERIOD<-NULL
idx<-TRx_1Mo$MONTH>=startmth&TRx_1Mo$MONTH<=endmth
tmpsales<-aggregate(TRx_1Mo$TRX_SCRIPTS[idx],list(TRx_1Mo$ONEKEY_ID[idx]),sum)
names(tmpsales)<-c("ONEKEY_ID","TRX_SCRIPTS")
tmpsalesyears<-merge(tmpsales,demographics[,c("ONEKEY_ID","years")],"ONEKEY_ID")
plot(tmpsalesyears$years,tmpsalesyears$TRX_SCRIPTS,type="p")
mean(tmpsalesyears$TRX_SCRIPTS[is.na(tmpsalesyears$years)])
# seems safe to replace missing year with zero
