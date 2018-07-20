if (.Platform$OS.type == "unix") {
  # if unix machine (i.e. Richard)
  setwd("/docs/help/msis_project")
  fileSources = file.path("code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
  SHARED_FOLDER <- file.path("/dropbox","analyses","results_shared","help","sykdomspulsen_project")
} else {
  # if NOT unix machine (i.e. Hanne)
  setwd("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project") 
  fileSources = file.path("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project/code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
  SHARED_FOLDER <- file.path("~","Dropbox","sykdomspulsen_project")
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

# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# read in the raw data
data <- data.table(readxl::read_excel("data_raw/msis_07_17.xlsx"))
# make a "municip" variable so that it is the same as in "mergingData"
data[,municip:=sprintf("municip%s",KommuneNr)]

# find out who is missing
data2 <- merge(data,mergingData,by.x=c("municip","ar"),by.y=c("municip","year"),all.x=T)
data2[is.na(municipEnd)]

xtabs(~data2[is.na(municipEnd)]$municip)
xtabs(~data2[is.na(municipEnd)]$municip+data2[is.na(municipEnd)]$ar)
#end

# merge in the dataset that says "kommune X -> kommune Y"
nrow(data) # number of rows in the dataset before merging
data <- merge(data,mergingData,by.x=c("municip","ar"),by.y=c("municip","year"))
nrow(data) # number of rows in the dataset after merging -- ARE THESE THE SAME?????

# delete "municip", because we now use "municipEnd"
data[,municip:=NULL]
data[,Bokomm:=NULL]

setnames(data,"municipEnd","municip")

data <- data[,.(num=.N),by=.(municip,ar,uke)]

skeleton <- data.table(expand.grid(
  municip=unique(mergingData[year==2018]$municipEnd),
  ar=2007:2017,
  uke=1:52
    ))

fullData <- merge(skeleton,data,by=c("municip","ar","uke"),all.x=T)
fullData[is.na(num),num:=0]

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

# HANNE: MAYBE SMART TO RENAME SOME VARIABLES???
# THIS WAY WE CAN LABEL MSIS VARIABLES WITH THE PREFIX msis_
setnames(fullData,"outbreak","msis_outbreak")
setnames(fullData,"threshold","msis_threshold")
setnames(fullData,"last5yrAverage","msis_last5yrAverage")

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
mergedData <- merge(s, fullData,
                    by.x=c("location","year","week"),
                    by.y=c("municip","ar","uke"))
nrow(mergedData)



## CALCULATIONS

# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(mergedData[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(mergedData[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(mergedData[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(mergedData[msis_outbreak==TRUE & s_status=="Normal"]) 



# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(mergedData[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(mergedData[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(mergedData[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(mergedData[msis_outbreak==TRUE & s_status!="High"])


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Outbreak="Medium+High",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Outbreak="Medium+High",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Outbreak="Medium+High",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Outbreak="Medium+High",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis.xlsx"))

# LVL2
# PPV = TP/TP+FP
TP2/(TP2+FP2)

# NPV = TN/TN+FN
TN2/(TN2+FN2)

# sensitivity (TPR) =  TP/(TP+FN)
TP2/(TP2+FN2)

# specificity (SPC) = TN/(TN+FP)
TN2/(TN2+FP2)

## RERUN ANALYSIS UNDER DIFFERENT STRATA

# read in the popuation numbers
municipNumbers <- data.table(readxl::read_excel("data_raw/municipNumbers.xlsx"))

# DOES POP ALREADY EXIST AS A VARIABLE??? IF SO, WE NEED TO DELETE IT
# mergedData[,pop:=NULL]

# merge with tabel showing population numbers
mergedDataPop <- merge(mergedData, municipNumbers,by=c("location"))

# CHECK same amount of rows???
nrow(mergedData)
nrow(mergedDataPop)

# Table showing all municipalities with population > 5000
#dataPop5000<-subset(mergedDataPop, pop > 5000, 
#                    select=c(location, year, week, n, s_status, msis_outbreak, pop)) 
# THIS WAY IS BETTER TO DO SUBSETTING, AS WE ARE USING DATA.TABLE

dataPop5000 <- mergedDataPop[pop>5000]
# OR THIS WAY, IF YOU WANT TO REDUCE THE NUMBER OF COLUMNS
dataPop5000 <- mergedDataPop[pop>5000,c("location", "year", "week", "n", "s_status", "msis_outbreak", "pop")]

nrow(dataPop5000)



## CALCULATIONS when pop > 5000

# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status=="Normal"]) 



# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status!="High"])


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

# LVL1
# PPV = TP/TP+FP
TP1/(TP1+FP1)

# NPV = TN/TN+FN
TN1/(TN1+FN1)

# sensitivity (TPR) =  TP/(TP+FN)
TP1/(TP1+FN1)

# specificity (SPC) = TN/(TN+FP)
TN1/(TN1+FP1)

# LVL2
# PPV = TP/TP+FP
TP2/(TP2+FP2)

# NPV = TN/TN+FN
TN2/(TN2+FN2)

# sensitivity (TPR) =  TP/(TP+FN)
TP2/(TP2+FN2)

# specificity (SPC) = TN/(TN+FP)
TN2/(TN2+FP2)



# Table showing all municipalities with population > 50000

dataPop50000 <- mergedDataPop[pop>50000]
# OR THIS WAY, IF YOU WANT TO REDUCE THE NUMBER OF COLUMNS
dataPop50000 <- mergedDataPop[pop>50000,c("location", "year", "week", "n", "s_status", "msis_outbreak", "pop")]

nrow(dataPop50000)



## CALCULATIONS when pop > 50000

# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="Normal"]) 



# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="High"])


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

# LVL1
# PPV = TP/TP+FP
TP1/(TP1+FP1)

# NPV = TN/TN+FN
TN1/(TN1+FN1)

# sensitivity (TPR) =  TP/(TP+FN)
TP1/(TP1+FN1)

# specificity (SPC) = TN/(TN+FP)
TN1/(TN1+FP1)

# LVL2
# PPV = TP/TP+FP
TP2/(TP2+FP2)

# NPV = TN/TN+FN
TN2/(TN2+FN2)

# sensitivity (TPR) =  TP/(TP+FN)
TP2/(TP2+FN2)

# specificity (SPC) = TN/(TN+FP)
TN2/(TN2+FP2)




## PREPARE VESUV 

# read in the raw data
v <- data.table(readxl::read_excel("data_raw/Vesuv_2007_2017.xlsx"))

nrow(v)


# add week numbers, must have "lubridate" - THIS ONLY GIVE ERROR MESSAGE ON CHARACKTER (V$DATOrEG). Not made for data.table?
library(lubridate)
x<-as.POSIXlt(v$DatoReg)
v[,date:=as.Date(`Dato registrert`,format="%d.%m.%Y")]
v[,uke:=strftime(date, format="%V")]

vInstitution <- v[Type!="Helseinstitusjon"]


### NEED TO CONVERT FROM NAMES TO MUNICIPALITY NUMBERS - or do this manually?


### NEED TO CONVERT FROM OLD TO NEW NUMBERS


# Exclude all rows where Column Type = Helseinstitusjon, 
nrow()







# Delete all rows where column Kommune is empty





# Add column vesuv_outbreak
v2$vesuv_outbreak<-1


# delete columns 
vFull<- v2[c("ar", "uke", "vesuv_outbreak","municip")]


vesuv<-VFull






## MERGE DATASETS (SP + M) with VESUV

fullData <- merge(mergedDataPop, vesuv, by=c(“municip”,”ar”,”uke”), all.x=T)

# the all.x=T means keep ALL the rows from mergedDataPop, even if they cant be merged with vesuv



# You will then notice that A LOT of the data didn’t merge (as you only have 1400 rows for vesuv). 
# This means that everything that *didn’t* merge, is NOT an outbreak. So we fill in that information:
  
fullData[is.na(vesuv_outbreak),vesuv_outbreak:=1]


# CALCULATIONS SP + M + V
## NEED TO FILL IN RIGHT DATA SETS, AND RIGHT ARGUMENTS

# EX 
# TRUE POSITIVE= 
# TP1 <- nrow(fullData[(vesuv==1 or msis_outbreak==TRUE) & s_status!="Normal"])



# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="Normal"]) 



# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="High"])







# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

# LVL1
# PPV = TP/TP+FP
TP1/(TP1+FP1)

# NPV = TN/TN+FN
TN1/(TN1+FN1)

# sensitivity (TPR) =  TP/(TP+FN)
TP1/(TP1+FN1)

# specificity (SPC) = TN/(TN+FP)
TN1/(TN1+FP1)

# LVL2
# PPV = TP/TP+FP
TP2/(TP2+FP2)

# NPV = TN/TN+FN
TN2/(TN2+FN2)

# sensitivity (TPR) =  TP/(TP+FN)
TP2/(TP2+FN2)

# specificity (SPC) = TN/(TN+FP)
TN2/(TN2+FP2)














































########################################################
##
##        Opprette fiktiv tabell som inneholder alle kommuner, ?r og uker
##
##########################################################

k.ant <- 435
aa.ant <- 10
u.ant <- 53

komm <- NULL
for(i in 1:k.ant)
  komm <- rbind(komm, rep(i, aa.ant*u.ant))
komm <- c(t(komm))

aar <- NULL
for(i in 1:k.ant)
  for(j in 1:aa.ant)
    aar <- rbind(aar, rep(j, u.ant))
aar <- c(t(aar))

uke <- rep(rep(1:u.ant), k.ant*aa.ant)

hdata <- data.frame(komm = komm, aar = aar, uke = uke, antall = rep(0, k.ant*aa.ant*u.ant))

head(hdata)

#################################################
##
##   Hente inn MSIS tabell 
##
#########################################################

library(xlsx)
msis<-read.xlsx("C:/Users/Hanne/Documents/NMBU/Masteroppgave/Analayser i R/18.06.26/MSIS_07_17_komm_aar_uke_excel.xlsx",1)


## Legge til kolonne  kalt "antall", og agregere pr uke ###############

msis$antall<-1

MSIS <- aggregate(msis$antall, msis[, c("komm", "aar", "uke")],FUN=sum) 


##################################################
##
##   Gjenst?r: Finne en kode / snutt som fletter MSIS og fiktiv tabell (hdata) slik 
##    at den koblede tabellen innholder alle komm, ?r og uker, med antall fra MSIS
##
##################################################


## Datasett 1: MSIS
## Datasett 2: hdata

## fors?ker snutt under, men f?r den ikke til ? fungere
for(i in 1:dim(MSIS)[[1]])
  
  hdata[hdata$komm == MSIS[i, ]$komm & hdata$aar == MSIS[i, ]$aar & hdata$uke == MSIS[i, ]$uke, ]$antall <- MSIS[i, ]$antall






## fors?ker ved ? koble to tabeller, outer join

koblet<-merge(hdata,MSIS, by=c("komm", "aar", "uke"),all=TRUE)
koblet

##  Endre NA til 0 i msis tabell

koblet[is.na(koblet)]<- 0
koblet

## legg sammen to kolonner i en ny kolonne

koblet$Antall<-koblet$antall.x+koblet$antall.y
koblet

###### dette medf?rer at alle komm, aar og uker som ikke inneholder verdi>0 forsvinner.





















##################################################
##
##    Algoritme for ? finne signal
##
##################################################

## Finne ukesnitt for hver kommune

ukesnitt <- NULL
for(k in 1:k.ant){
  this.uke <- NULL
  for (i in 1:u.ant){
    this.uke <- c(this.uke, mean(hdata[komm == k & uke == i, ]$antall))
  }
  ukesnitt <- rbind(ukesnitt, this.uke)
}

## beregner standardavvik for all kommuner og for alle ?r

aaravvik <- NULL
for(k in 1:k.ant){
  this.avvik <- NULL
  for(i in 1:aa.ant){
    this.avvik <- c(this.avvik, sqrt(var(hdata[komm == k & aar == i, ]$antall)))
  }
  aaravvik <- cbind(aaravvik, this.avvik)
}

#########################################################
##
##      Kjernefunksjon: Finne signal
##
#########################################################


antsigma <- 5

for (k in 1:k.ant){
  for (i in 1:aa.ant){
    for (j in 1:u.ant){
      if(hdata[komm == k & aar == i & uke == j,]$antall > ukesnitt[k, j] + antsigma * aaravvik[i, k])
        print(c("Signal for kommune ", k, " Aar: ", i, "uke: ", j, "Antall: ",hdata[komm == k & aar == i & uke == j,]$antall ))
    }
  }
}







