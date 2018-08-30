### Exploration of CA_ICE_Engagement_Notes.txt
notes <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Notes.txt",
                    header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor","Date",
                                                      "factor","factor","factor","factor",
                                                      "factor","factor","factor","character",
                                                      "character","character"))
print(dim(notes))
names(notes)
summary(notes)

# min CALL_DATE by HCP
date_min_notes <- aggregate(notes$CALL_DATE,by=list(notes$ONEKEY_ID),min)
hist(date_min_notes$x,breaks=100)

# max CALL_DATE by HCP
date_max_notes <- aggregate(notes$CALL_DATE,by=list(notes$ONEKEY_ID),max)
hist(date_max_notes$x,breaks=100)

# no. visits
no_visits_notes <- aggregate(notes$CALL_DATE,by=list(notes$ONEKEY_ID),length)
hist(no_visits_notes$x,breaks=100,xlim=c(0,50))
summary(no_visits_notes$x)
sum(no_visits_notes$x>20)/length(no_visits_notes$x)
summary(no_visits_notes$x[no_visits_notes$x<=20])


### Exploration of CA_ICE_Engagement_Samples.txt
samples <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Samples.txt",
                    header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor","Date",
                                                      "factor","factor","factor","factor",
                                                      "factor","factor","factor","factor",
                                                      "factor","numeric"))
print(dim(samples))
names(samples)
summary(samples)

# min CALL_DATE by HCP
date_min_samples <- aggregate(samples$CALL_DATE,by=list(samples$ONEKEY_ID),min)
hist(date_min_samples$x,breaks=100)

# max CALL_DATE by HCP
date_max_samples <- aggregate(samples$CALL_DATE,by=list(samples$ONEKEY_ID),max)
hist(date_max_samples$x,breaks=100)

# no. visits
no_visits_samples <- aggregate(samples$CALL_DATE,by=list(samples$ONEKEY_ID),length)
hist(no_visits_samples$x,breaks=100,xlim=c(0,50))
summary(no_visits_samples$x)

# no. samples
no_samples <- aggregate(samples$SAMPLE_QUANTITY,by=list(samples$ONEKEY_ID),sum)
hist(no_samples$x,breaks=100,xlim=c(0,500))
summary(no_samples$x)

### Exploration of CA_ICE_Engagement_Info.txt
info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_Engagement_Info.txt",
                    header=TRUE, sep="|",colClasses=c("factor","factor","factor","factor",
                                                      "factor",
                                                      "Date",
                                                      "factor","factor","factor","factor",
                                                      "factor","factor","factor","factor",
                                                      "numeric","factor"))
print(dim(info))
names(info)
summary(info)

# min CALL_DATE by HCP
date_min_info <- aggregate(info$CALL_DATE,by=list(info$ONEKEY_ID),min)
hist(date_min_info$x,breaks=100)

# max CALL_DATE by HCP
date_max_info <- aggregate(info$CALL_DATE,by=list(info$ONEKEY_ID),max)
hist(date_max_info$x,breaks=100)

# no. visits
no_visits_info <- aggregate(info$CALL_DATE,by=list(info$ONEKEY_ID),length)
hist(no_visits_info$x,breaks=100,xlim=c(0,50))
summary(no_visits_info$x)
