if (.Platform$OS.type == "unix") {
  # if unix machine (i.e. Richard)
  setwd("/docs/help/msis_project")
  fileSources = file.path("code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
  SHARED_FOLDER <- file.path("/dropbox","analyses","results_shared","help","sykdomspulsen_project")
} else {
  # if NOT unix machine (i.e. Hanne)
  setwd("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project") 
  fileSources = file.path("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project/code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
  SHARED_FOLDER <- file.path("C:/Users/Hanne/Dropbox/00 NMBU/Masteroppgave/results_shared/sykdomspulsen_project")
}

if(!dir.exists(SHARED_FOLDER)){
  stop("DROPBOX FOLDER ISNT CORRECTLY SPECIFIED")
  quit()
}

# specify the folder for today's results
SHARED_FOLDER_TODAY <- file.path(SHARED_FOLDER,lubridate::today())
# create that folder
dir.create(SHARED_FOLDER_TODAY)

#test test
# LOADS IN ALL R SCRIPTS THAT ARE LOCATED IN THE "code" folder
sapply(fileSources, source, .GlobalEnv)

library(data.table)
library(ggplot2)


### SYKDOMSPULSEN DATA

s1 <- data.table(read.table("data_raw/Utbruddsdata_20180419_2007_2009.txt"))
s2 <- data.table(read.table("data_raw/Utbruddsdata_20180419_2010_2012.txt"))
s3 <- data.table(read.table("data_raw/Utbruddsdata_20180419_2013_2015.txt"))
s4 <- data.table(read.table("data_raw/Utbruddsdata_20180419_2016_2017.txt"))

# MAKE SURE WE ONLY USE "ALL AGES" -- NO NEED FOR THE SEPARATE AGE GROUPS
s <- rbind(s1,s2,s3,s4)[age=="Totalt"]

# HANNE: MAYBE SMART TO RENAME SOME VARIABLES???
# THIS WAY WE CAN LABEL SYKDOMSPULSEN VARIABLES WITH THE PREFIX s_
setnames(s,"status","s_status")


# remove s1, s2, s3, s4, from the RAM of the computer, because they are big and we don't have that much space
rm("s1")
rm("s2")
rm("s3")
rm("s4")

nrow(s)

### PREPARE MSIS DATA

# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# read in the raw data
data <- data.table(readxl::read_excel("data_raw/msis_07_17.xlsx"))
xtabs(~data$Diag)
xtabs(~data$ar)
#data <- data[Diag %in% c("E. coli-enteritt unntatt EHEC")]
# make a "municip" variable so that it is the same as in "mergingData"
data[,municip:=sprintf("municip%s",KommuneNr)]

# merge in the dataset that says "kommune X -> kommune Y"
nrow(data) # number of rows in the dataset before merging
data <- merge(data,mergingData,by.x=c("municip","ar"),by.y=c("municip","year"))
nrow(data) # number of rows in the dataset after merging -- ARE THESE THE SAME?????

# delete "municip", because we now use "municipEnd"
data[,municip:=NULL]
data[,Bokomm:=NULL]

setnames(data,"municipEnd","municip")

# Aggregate registrations in msis, pr municip, yr and week
data <- data[,.(num=.N),by=.(municip,ar,uke)]


# Create skeleton table to include all municip, years and weeks
skeleton <- data.table(expand.grid(
  municip=unique(mergingData[year==2018]$municipEnd),
  ar=2007:2017,
  uke=1:52
))

fullData <- merge(skeleton,data,by=c("municip","ar","uke"),all.x=T)
fullData[is.na(num),num:=0]
nrow(fullData)

#
fullData[,num1:=shift(num,n=1L,type="lag"),by=.(municip,uke)]
fullData[,num2:=shift(num,n=2L,type="lag"),by=.(municip,uke)]
fullData[,num3:=shift(num,n=3L,type="lag"),by=.(municip,uke)]
fullData[,num4:=shift(num,n=4L,type="lag"),by=.(municip,uke)]
fullData[,num5:=shift(num,n=5L,type="lag"),by=.(municip,uke)]

fullData[,last5yrAverage:=(num1+num2+num3+num4+num5)/5]

# take a look at the data examples
fullData[municip=="municip0301" & uke==3]
fullData[municip=="municip5054" & uke==30]

# create the thresholds
fullData[,threshold:=qpois(p=0.95,lambda=last5yrAverage)]
fullData[,outbreak:=num>threshold]
fullData[,outbreak_lastwk:=shift(outbreak,type="lag"),by=.(municip)]
fullData[,outbreak2wks:=outbreak & outbreak_lastwk]
xtabs(~fullData$outbreak+fullData$outbreak2wks)


# HANNE: MAYBE SMART TO RENAME SOME VARIABLES???
# THIS WAY WE CAN LABEL MSIS VARIABLES WITH THE PREFIX msis_
setnames(fullData,"outbreak","msis_outbreak")
setnames(fullData,"outbreak2wks","msis_outbreak2wks")
setnames(fullData,"threshold","msis_threshold")
setnames(fullData,"last5yrAverage","msis_last5yrAverage")

mergedData <- merge(s, fullData,
                    by.x=c("location","year","week"),
                    by.y=c("municip","ar","uke"))
nrow(mergedData)

mergedData <- tryCatch({
  municipNumbers <- data.table(readxl::read_excel("data_raw/municipNumbers.xlsx"))
  municipNumbers[,popCat:=cut(pop,breaks=0,5000,10000,50000,99999999,include.lowest = T)]
  merge(mergedData, municipNumbers,by=c("location"))
},
error=function(err){
  mergedData
})

# to aggregate
#[row,column,by]
#mergedData[year==2918,
#           .(
#             
#           ),keyby=.(
#             
#           )]



### GET YOUR FULL DATASET HERE
# VESUV + POPULATION + MSIS + SYKDOMPULSEN

### THEN DO SOME CORRELATION/GRAPHING ANALYSES

## THEN DO THE PPV/SENS/NPV

mergedData[,num_w_future1:=shift(num,n=1L,type="lead"),by=.(location)]
mergedData[,num_w_past1:=shift(num,n=1L,type="lag"),by=.(location)]

# take a look at the correlations
#cor(mergedData$n,mergedData$numx1)

mergedData[,season:=sprintf("%s/%s",year-1,year)]
mergedData[week>26,season:=sprintf("%s/%s",year,year+1)]

corr <- na.omit(mergedData[
  ,.(
    corr_past1=cor(n,num_w_past1),
    corr0=cor(n,num),
    corr_future1=cor(n,num_w_future1)
  ),by=.(
    location,season
  )])

corr[,.(
  corr_past1=mean(corr_past1),
  corr0=mean(corr0),
  corr_future1=mean(corr_future1)
),keyby=.(season)]

corr[,.(
  corr_past1=mean(corr_past1),
  corr0=mean(corr0),
  corr_future1=mean(corr_future1)
),keyby=.()]

mean(corr$corrminus1)
mean(corr$corr0)
mean(corr$corrplus1)

hist(corr$corr)

mergedData[,fylke:=substr(location,1,9)]

f <- mergedData[,
                .(
                  n=sum(n),
                  num=sum(num)
                  ),by=.(
                    fylke,
                    year,
                    week
                  )]

plot(f$n~f$num)

corr <- na.omit(f[num<80
  ,.(
    corr0=cor(n,num)
  ),by=.(fylke,year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]


#x <- melt.data.table(mergedData[year>=2014,c("n","num","location","year","week")],id.vars = c("location","year","week"))
#x[,id:=paste0(location,year,week)]
#ICC::ICCbare("id","value",data=x)

## CALCULATIONS SP vs MSIS (gold)

mergedData[,spthreshold2_vs_msis:=as.character(NA)]
mergedData[msis_outbreak==TRUE & s_status!="Normal", spthreshold2_vs_msis:="TP"]
mergedData[msis_outbreak==FALSE & s_status=="Normal", spthreshold2_vs_msis:="TN"]
mergedData[msis_outbreak==FALSE & s_status!="Normal", spthreshold2_vs_msis:="FP"]
mergedData[msis_outbreak==TRUE & s_status=="Normal", spthreshold2_vs_msis:="FN"]
xtabs(~mergedData$spthreshold2_vs_msis)

# Level 2:  No outbreak=normal+medium / Outbreak=high

mergedData[,spthreshold4_vs_msis:=as.character(NA)]
mergedData[msis_outbreak==TRUE & s_status=="High", spthreshold4_vs_msis:="TP"]
mergedData[msis_outbreak==FALSE & s_status!="High", spthreshold4_vs_msis:="TN"]
mergedData[msis_outbreak==FALSE & s_status=="High", spthreshold4_vs_msis:="FP"]
mergedData[msis_outbreak==TRUE & s_status!="High", spthreshold4_vs_msis:="FN"]


TP <- function(var){
  sum(var=="TP",na.rm=T)
}

FP <- function(var){
  sum(var=="FP",na.rm=T)
}

TN <- function(var){
  sum(var=="TN",na.rm=T)
}

FN <- function(var){
  sum(var=="FN",na.rm=T)
}

PPV <- function(var){
  return(TP(var)/(TP(var)+FP(var)))
}

NPV <- function(var){
  return(TN(var)/(TN(var)+FN(var)))
}

SENS <- function(var){
  return(TP(var)/(TP(var)+FN(var)))
}

SPEC <- function(var){
  return(TN(var)/(TN(var)+FP(var)))
}

long <- melt.data.table(mergedData[,c(
  "location","year","week","msis_threshold","n","spthreshold2_vs_msis","spthreshold4_vs_msis"
),with=F],id.vars = c("location","year","week","msis_threshold","n"))

results <- long[,.(
  tp=TP(value),
  fn=FN(value),
  ppv=PPV(value),
  npv=NPV(value),
  sens=SENS(value),
  spec=SPEC(value)
),keyby=.(variable)]

na.omit(results)

openxlsx::write.xlsx(na.omit(results), file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis_lags.xlsx"))

xtabs(~mergedData$SP_vs_MSIS_threshold2_1)


## PREPARE VESUV #############################

# read in the raw data
v <- data.table(readxl::read_excel("data_raw/Vesuv_2007_2017.xlsx"))
nrow(v)

# make a "municip" variable so that it is the same as in "mergingDataV"
v[,municip:=sprintf("municip%s",KommuneNr)]

# add week numbers, must have "lubridate" 
library(lubridate)
v[,date:=as.Date(`DatoReg`,format="%d.%m.%Y")]
v[,uke:=strftime(date, format="%V")]


# Delete all rows where column Kommune is empty
vNull<-v[municip!="municipNA"]

nrow(vNull)

# Remove all registrations done from Helseinstitusjoner
vInstitution <- vNull[Type!="Helseinstitusjon"]

nrow(vInstitution)

# Create column called vesuv_outbreak, with all <- 1
vInstitution$vesuv_outbreak<-1

# delete columns, so only location, year, week and vesuv_outbreak is left 
vFull<- vInstitution
vFull[, c("DatoReg", "KommuneNr", "date", "Kommune", "Type"):=NULL] 

setnames(vFull,"uke","week")

vFull
nrow(vFull)


# Convert from old to new municipality numbers also in Vesuv
# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# find out who is missing
data2 <- merge(vFull,mergingData,by.x=c("municip","year"),by.y=c("municip","year"),all.x=T)
data2[is.na(municipEnd)]

xtabs(~data2[is.na(municipEnd)]$municip)
xtabs(~data2[is.na(municipEnd)]$municip+data2[is.na(municipEnd)]$year)
#end

# merge in the dataset that says "kommune X -> kommune Y"
nrow(vFull) # number of rows in the dataset before merging
vFull <- merge(vFull,mergingData,by.x=c("municip","year"),by.y=c("municip","year"))
nrow(vFull) # number of rows in the dataset after merging -- ARE THESE THE SAME????? 

# delete "municip", because we now use "municipEnd"
vFull[,municip:=NULL]

setnames(vFull,"municipEnd","location")

setcolorder(vFull, c("location", "year", "week", "vesuv_outbreak"))



## MERGE ALL DATASETS #######

vFull # Vesuv dataset
nrow(vFull)
mergedDataPop # msis and Sykdomspulsen + population 
nrow(mergedDataPop)


### Merge vesuv dataset with mergedDataPop
# the all.x=T means keep ALL the rows from mergedDataPop, even if they cant be merged with vesuv
# fullData <- merge(mergedDataPop, vFull, by=c("location","year","week"), all.x=T) GIVES ERROR MESSAGE:
# Error in bmerge(i, x, leftcols, rightcols, io, xo, roll, rollends, nomatch,  : x.'week' is a character column being joined to i.'week' which is type 'double'. 
# Therefor:

vFullChar<-vFull[, week:=as.numeric(week)]
str(vFullChar)
nrow(vFullChar)
str(mergedDataPop)
nrow(mergedDataPop)

fullData <- merge(mergedDataPop, vFullChar, by=c("location","year","week"), all.x=T)
nrow(fullData)
fullData

# You will then notice that A LOT of the data didn’t merge (as you only have 1400 rows for vesuv). 
# This means that everything that *didn’t* merge, is NOT an outbreak. So we fill in that information:

fullData[is.na(vesuv_outbreak),vesuv_outbreak:=0] ### I have put in =0 instead of =1, is this wrong?
fullData


# delete columns
fullData[, c("threshold0", "threshold2", "threshold4","type","age","n","num", "num1","num2", "num3","num4", "num5","msis_threshold", "msis_last5yrAverage"):=NULL] 

setcolorder(fullData, c("location","locationName", "pop", "year", "week", "vesuv_outbreak","msis_outbreak","s_status"))

fullData


fullData[is.na(vesuv_outbreak),vesuv_outbreak:=0]

# take a look at the data examples
fullData[location=="municip0301" & week==3]
fullData[location=="municip5054" & week==30]



### CALCULTIONS SYKDOMSPULSEN VS VESUV (GOLD STANDARD)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullData[vesuv_outbreak==1 & s_status!="Normal"])

# TRUE NEGATIVE= 
TN1 <- nrow(fullData[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData[vesuv_outbreak==1 & s_status=="Normal"]) 


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullData[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullData[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullData[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullData[vesuv_outbreak==1 & s_status!="High"])


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv.xlsx"))




## CALCULATIONS when pop > 5000

fullData5000 <- fullData[pop>5000]
nrow(fullData5000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullData5000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData5000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData5000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData5000[vesuv_outbreak==1 & s_status=="Normal"]) 


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullData5000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullData5000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullData5000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullData5000[vesuv_outbreak==1 & s_status!="High"])


results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv_pop5000.xlsx"))


## CALCULATIONS when pop > 50 000

fullData50000 <- fullData[pop>50000]
nrow(fullData50000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullData50000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData50000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData50000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData50000[vesuv_outbreak==1 & s_status=="Normal"]) 


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullData50000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullData50000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullData50000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullData50000[vesuv_outbreak==1 & s_status!="High"])



# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)


results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv_pop50000.xlsx"))


## CALCULATIONS when pop > 10 000

fullData10000 <- fullData[pop>10000]
nrow(fullData10000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullData10000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData10000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData10000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData10000[vesuv_outbreak==1 & s_status=="Normal"]) 


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullData10000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullData10000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullData10000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullData10000[vesuv_outbreak==1 & s_status!="High"])


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)


results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv_pop10000.xlsx"))




### CALCULTIONS MSIS VS VESUV (GOLD STANDARD)


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullData[vesuv_outbreak==1 & msis_outbreak!="FALSE"])

# TRUE NEGATIVE= 
TN1 <- nrow(fullData[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV"]] <- data.frame(Outbreak="TRUE",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV"]] <- data.frame(Outbreak="TRUE",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS"]] <- data.frame(Outbreak="TRUE",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC"]] <- data.frame(Outbreak="TRUE",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"msis_vs_vesuv.xlsx"))



## CALCULATIONS when pop > 5000

fullData5000 <- fullData[pop>5000]
nrow(fullData5000)


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullData5000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData5000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData5000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData5000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV"]] <- data.frame(Outbreak="TRUE",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV"]] <- data.frame(Outbreak="TRUE",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS"]] <- data.frame(Outbreak="TRUE",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC"]] <- data.frame(Outbreak="TRUE",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"msis_vs_vesuv_pop5000.xlsx"))




## CALCULATIONS when pop > 50 000

fullData50000 <- fullData[pop>50000]
nrow(fullData50000)


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullData50000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData50000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData50000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData50000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV"]] <- data.frame(Outbreak="TRUE",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV"]] <- data.frame(Outbreak="TRUE",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS"]] <- data.frame(Outbreak="TRUE",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC"]] <- data.frame(Outbreak="TRUE",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"msis_vs_vesuv_pop50000.xlsx"))



## CALCULATIONS when pop > 10 000

fullData10000 <- fullData[pop>10000]
nrow(fullData10000)

# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullData10000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullData10000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData10000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullData10000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV"]] <- data.frame(Outbreak="TRUE",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV"]] <- data.frame(Outbreak="TRUE",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS"]] <- data.frame(Outbreak="TRUE",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC"]] <- data.frame(Outbreak="TRUE",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"msis_vs_vesuv_pop10000.xlsx"))



#






































