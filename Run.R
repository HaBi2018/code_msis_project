if (.Platform$OS.type == "unix") {
  # if unix machine (i.e. Richard)
  setwd("/docs/help/msis_project")
  fileSources = file.path("code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
} else {
  # if NOT unix machine (i.e. Hanne)
  setwd("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project") 
  fileSources = file.path("C:/Users/Hanne/Documents/NMBU/Masteroppgave/msis_project/code_msis_project/code", list.files("code_msis_project/code", pattern = "*.[rR]$"))
}

#test test
# LOADS IN ALL R SCRIPTS THAT ARE LOCATED IN THE "code" folder
sapply(fileSources, source, .GlobalEnv)

library(data.table)

# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# now we need to convert "Bokommune" to "municip" (i.e. kommunenummer)
bokommToKommuneNr <- data.table(readxl::read_excel("code_msis_project/structural_data/Kommunenummer_STUDIEN434.xlsx"))
# make a "municip" variable so that it is the same as in "mergingData"
bokommToKommuneNr[,municip:=sprintf("municip%s",Kommunenr)]
# delete useless variables
bokommToKommuneNr[,Komm:=NULL]
bokommToKommuneNr[,Kommunenr:=NULL]
bokommToKommuneNr[,Opprettet:=NULL]
bokommToKommuneNr[,Opphevet:=NULL]

# read in the raw data
data <- data.table(readxl::read_excel("data_raw/msis_07_17.xlsx"))
# change "år" to "ar" (never use norwegian characters!!!)
# setnames(data,"år","ar") HAR ENDRET TIL AR I EXCEL

# give each datapoint a "kommunenummer"
nrow(data) # number of rows in the dataset before merging
data <- merge(data,bokommToKommuneNr,by.x="Bokomm",by.y="Kommunenavn")
nrow(data) # number of rows in the dataset after merging -- ARE THESE THE SAME?????

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
 
  municip=unique(data$municip),
  ar=2007:2017,
  uke=1:52
    ))

fullData <- merge(skeleton,data,by=c("municip","ar","uke"),all.x=T)
fullData[is.na(num),num:=0]

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







