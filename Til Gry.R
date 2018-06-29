
########################################################
##
##        Opprette fiktiv tabell som inneholder alle kommuner, år og uker
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
##   Gjenstår: Finne en kode / snutt som fletter MSIS og fiktiv tabell (hdata) slik 
##    at den koblede tabellen innholder alle komm, år og uker, med antall fra MSIS
##
##################################################


## Datasett 1: MSIS
## Datasett 2: hdata

## forsøker snutt under, men får den ikke til å fungere
for(i in 1:dim(MSIS)[[1]])
  
  hdata[hdata$komm == MSIS[i, ]$komm & hdata$aar == MSIS[i, ]$aar & hdata$uke == MSIS[i, ]$uke, ]$antall <- MSIS[i, ]$antall






## forsøker ved å koble to tabeller, outer join

koblet<-merge(hdata,MSIS, by=c("komm", "aar", "uke"),all=TRUE)
koblet

##  Endre NA til 0 i msis tabell

koblet[is.na(koblet)]<- 0
koblet

## legg sammen to kolonner i en ny kolonne

koblet$Antall<-koblet$antall.x+koblet$antall.y
koblet

###### dette medfører at alle komm, aar og uker som ikke inneholder verdi>0 forsvinner.








##################################################
##
##    Algoritme for å finne signal
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

## beregner standardavvik for all kommuner og for alle år

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







