### freq tables
rm(list=ls())
discrete_vars <- c("CVM","STSEG","PLANNED_DETAILS","PRIMARY_SPECIALTY","PROVINCE","GENDER",
                   "PRIMARY_LANGUAGE")

# CVM_Info
cvm_info <- read.delim("/wps_scratch/usr_scratch/burbidge/Aubrey/CA_ICE_CVM_Info.txt",
                    header=TRUE, sep="|",colClasses=c("factor","numeric","factor","factor","factor",
                                                      "factor"))
print(dim(cvm_info))
names(cvm_info)
summary(cvm_info)
for (var in union(c("CVM_YEAR","PRODUCT"),intersect(names(cvm_info),discrete_vars))) {
  print(var)
  print(table(cvm_info[,var]))
}

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
levels(demographics$PRIMARY_SPECIALTY) <- c(levels(demographics$PRIMARY_SPECIALTY),"Other")
demographics$PRIMARY_SPECIALTY[!(demographics$PRIMARY_SPECIALTY %in% c("Family Medicine",
                                            "General Practice"))] <- as.factor("Other")
demographics <- droplevels(demographics)
for (var in intersect(names(demographics),discrete_vars)) {
  print(var)
  print(table(demographics[,var]))
}

# CVM_Info & Demographics
length(setdiff(cvm_info$ONEKEY_ID,demographics$ONEKEY_ID))
length(intersect(cvm_info$ONEKEY_ID,demographics$ONEKEY_ID))
length(setdiff(demographics$ONEKEY_ID,cvm_info$ONEKEY_ID))

