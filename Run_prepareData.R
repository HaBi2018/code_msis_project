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




#############################
##   MSIS DATA             ##
#############################


# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# read in the raw data
data <- data.table(readxl::read_excel("data_raw/msis_07_17.xlsx"))
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
# fullData[,outbreak:=num>threshold]

fullData[,outbreak:=(num>threshold)&(num>=2)]

# HANNE: MAYBE SMART TO RENAME SOME VARIABLES???
# THIS WAY WE CAN LABEL MSIS VARIABLES WITH THE PREFIX msis_
setnames(fullData,"outbreak","msis_outbreak")
setnames(fullData,"threshold","msis_threshold")
setnames(fullData,"last5yrAverage","msis_last5yrAverage")

nrow(fullData)




#############################
##  SYKDOMSPULSEN DATA     ##
#############################

if (.Platform$OS.type == "unix") {
  data<-readRDS("data_raw/resYearLineMunicip_gastro.RDS")
} else {
  data<-readRDS("C:/Users/Hanne/Dropbox/00 NMBU/Masteroppgave/results_shared/sykdomspulsen_project/resYearLineMunicip_gastro.RDS")
}
# USE "ALL AGES" -- NO NEED FOR THE SEPARATE AGE GROUPS
prepS <-data[age=="Totalt"]

# LABEL SYKDOMSPULSEN VARIABLES WITH THE PREFIX s_
setnames(prepS,"status","s_status")
# setnames(prepS,"n", "s_n")
setnames(prepS,"threshold0", "s_threshold0")
setnames(prepS,"threshold2", "s_threshold2")
setnames(prepS,"threshold4", "s_threshold4")
setnames(prepS,"threshold6", "s_threshold6")
setnames(prepS,"zscore", "s_zscore")

# REMOVE DATA FROM YEAR 2006 and 2018
prepS2<-prepS[year!="2006"]
prepS3<-prepS2[year!="2018"]

# REMOVE COLUMNS NOT IN USE
prepS3[, c("displayDay", "wkyr", "x", "consult", "pop", "HelligdagIndikator", "cumE1", "cumL1", "cumU1", "failed", "age", "type", "county"):=NULL] 

setcolorder(prepS3, c("location", "locationName", "year", "week", "n", "s_threshold0", "s_threshold2", "s_threshold4", "s_threshold6", "s_status"))


s<-prepS3
nrow(s)





#############################
##  VESUV DATA             ##
#############################


# read in the raw data
v <- data.table(readxl::read_excel("data_raw/Vesuv_2007_2017_b.xlsx"))
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
xtabs(~vNull$Type,addNA=T)
#vInstitution <- vNull[Type!="Helseinstitusjon"]
vInstitution <- vNull[Type!="Helseinstitusjon" | is.na(Type)]
xtabs(~vInstitution$Type,addNA=T)

nrow(vInstitution)

# Create column called vesuv_outbreak, with all <- 1
vInstitution$vesuv_outbreak<-1

# delete columns, so only location, year, week, Antall and vesuv_outbreak is left 
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

setcolorder(vFull, c("location", "year", "week","Antall", "vesuv_outbreak"))

nrow(vFull) #nrow=537

# Aggregate all "double" rows in Vesuv by municp, year, week, and sum Antall
vFull <-vFull[,.(v_n=sum(Antall)),by=.(location, year,week, vesuv_outbreak)]
nrow(vFull) #nrow=510




#############################
##  MERGE ALL DATASETS     ##
#############################


# DATASETS:
## MSIS = fullData
## SP = s
## VESUV = vFull



# MERGE SP AND MSIS

mergedData <- merge(s, fullData,
                    by.x=c("location","year","week"),
                    by.y=c("municip","ar","uke"))
nrow(mergedData)
nrow(fullData)



# ADD POPULATION NUMBERS

# read in the popuation numbers
municipNumbers <- data.table(readxl::read_excel("data_raw/municipNumbers.xlsx"))

# merge with tabel showing population numbers
mergedDataPop <- merge(mergedData, municipNumbers,by=c("location"))

# CHECK, same amount of rows?
nrow(mergedData)
nrow(mergedDataPop)




# MERGE WITH (SP + MSIS) WITH VESUV

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

fullDataSVM <- merge(mergedDataPop, unique(vFullChar), by=c("location","year","week"), all.x=T)
nrow(fullDataSVM)
nrow(mergedDataPop)


fullDataSVM

# You will then notice that A LOT of the data didn’t merge (as you only have 1400 rows for vesuv). 
# This means that everything that *didn’t* merge, is NOT an outbreak. So we fill in that information:

fullDataSVM[is.na(vesuv_outbreak),vesuv_outbreak:=0] 
fullDataSVM


# delete columns
fullDataSVM[, c("type","age","msis_last5yrAverage","num1","num2","num3","num4","num5"):=NULL] 

fullDataSVM[is.na(vesuv_outbreak),vesuv_outbreak:=0]

# take a look at the data examples
fullDataSVM[location=="municip0301" & week==3]
fullDataSVM[location=="municip5054" & week==30]

setcolorder(fullDataSVM, c("location", "locationName", "year", "week", "pop", "n","num","v_n", "s_zscore", "s_threshold0", "s_threshold2", "s_threshold4", "s_threshold6","msis_threshold","vesuv_outbreak","msis_outbreak","s_status"))

fullDataSVM






#openxlsx::write.xlsx(fullDataSVM,file=file.path(SHARED_FOLDER_TODAY,"fullDataSVM.xlsx"))






