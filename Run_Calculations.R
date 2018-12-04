

## RUN Run_PrepareData.R

# DATASETS:
## MSIS = fullData (n=num)
## SP = s (n=n)
## SP + MSIS = mergedData
## SP+ msis + POP = mergedDataPop
## VESUV = vFull (n=Antall)
## ALL MERGED: fullDataSVM



#############################################################
##  CORRELATION/GRAPHING ANALYSES                          ##
#############################################################



#mergedData <- tryCatch({
#  municipNumbers <- data.table(readxl::read_excel("data_raw/municipNumbers.xlsx"))
# municipNumbers[,popCat:=cut(pop,breaks=0,5000,10000,50000,99999999,include.lowest = T)]
# merge(mergedData, municipNumbers,by=c("location"))
#},
#error=function(err){
# mergedData
#})

# to aggregate
#[row,column,by]
#mergedData[year==2018,
#           .(
#             
#           ),keyby=.(
#             
#           )]





### GET YOUR FULL DATASET HERE (VESUV + POPULATION + MSIS + SYKDOMPULSEN)


fullDataSVM



######################################
## CORRELATION SP VS MSIS           ##
######################################


#### MUNICIPALITY LEVEL  ####################################


m <- mergedData[,
                .(
                  n=sum(n),
                  num=sum(num)
                ),by=.(
                  location,
                  year,
                  week
                )]

plot(m$n~m$num)

## Scatterplot - correlation N$n~N$num

plot(m$n~m$num, main="Korrelasjon mellom antall tilfeller av mage-tarminfeksjon på kommunenivå",
     pch=1, frame =T, cex.main=0.8, cex.lab=0.7,
     xlab="Sykdomspulsen ", ylab="MSIS" )


cor.test(N$n, N$num) 



corr <- na.omit(m[num<80
                  ,.(
                    corr0=cor(n,num)
                  ),by=.(location,year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]



mergedData[,num_w_future1:=shift(num,n=1L,type="lead"),by=.(location)]
mergedData[,num_w_past1:=shift(num,n=1L,type="lag"),by=.(location)]

#cor(mergedData$n,mergedData$numx1)   GIVES ERRor - # supply both 'x' and 'y' or a matrix-like 'x'OR: 

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

mean(corr$corr_past1)
mean(corr$corr0)
mean(corr$corr_future1)

hist(corr$corr0)

## COORELATION - COUNTY LEVEL ##############

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

plot(f$n~f$num, main="Korrelasjon mellom antall tilfeller av mage-tarminfeksjon på fylkesnivå",
     pch=1, frame =T, cex.main=0.8, cex.lab=0.7,
     xlab="Sykdomspulsen ", ylab="MSIS" )

corr <- na.omit(f[num<80
                  ,.(
                    corr0=cor(n,num)
                  ),by=.(fylke,year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]



## CORRELATION - NATIONAL LEVEL ##############

N <- mergedData[,
                .(
                  n=sum(n),
                  num=sum(num)
                ),by=.(
                  year,
                  week
                )]

plot(N$n~N$num, main="Korrelasjon mellom antall tilfeller av mage-tarminfeksjon på nasjonalt nivå",
     pch=1, frame =T, cex.main=0.8, cex.lab=0.7,
     xlab="Sykdomspulsen ", ylab="MSIS" )

corr <- na.omit(N[num<80
                  ,.(
                    corr0=cor(n,num)
                  ),by=.(year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]


hist(corr$corr0)

cor.test(N$n, N$num) 






########################################################################
## CALCULATIONS                                                       ##
########################################################################


########################################
##  CALCULATIONS SP vs MSIS (gold)    ## 
########################################


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(mergedData[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(mergedData[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(mergedData[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(mergedData[msis_outbreak==TRUE & s_status=="Normal"]) 

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(mergedData[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(mergedData[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(mergedData[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(mergedData[msis_outbreak==TRUE & s_status!="High"])

TP2
TN2
FP2
FN2



# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Comp="SPvsMSIS(Gold)",Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis.xlsx"))




## RERUN ANALYSIS UNDER DIFFERENT STRATA ###############################

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

# dataPop5000 <- mergedDataPop[pop>5000]
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

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop5000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop5000[msis_outbreak==TRUE & s_status!="High"])

TP2
TN2
FP2
FN2

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
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis_pop5000.xlsx"))


# Table showing all municipalities with population > 50000

dataPop50000 <- mergedDataPop[pop>50000]
# OR THIS WAY, IF YOU WANT TO REDUCE THE NUMBER OF COLUMNS
# dataPop50000 <- mergedDataPop[pop>50000,c("location", "year", "week", "n", "s_status", "msis_outbreak", "pop")]

nrow(dataPop50000)



## CALCULATIONS when pop > 50 000

# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="Normal"]) 

TP1
TN1
FP1
FN1


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop50000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop50000[msis_outbreak==TRUE & s_status!="High"])

TP2
TN2
FP2
FN2

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
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis_pop50000.xlsx"))


# Table showing all municipalities with population > 10 000

dataPop10000 <- mergedDataPop[pop>10000]
# OR THIS WAY, IF YOU WANT TO REDUCE THE NUMBER OF COLUMNS
# dataPop10000 <- mergedDataPop[pop>10000,c("location", "year", "week", "n", "s_status", "msis_outbreak", "pop")]

nrow(dataPop10000)


## CALCULATIONS when pop > 10 000

# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(dataPop10000[msis_outbreak==TRUE & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(dataPop10000[msis_outbreak==FALSE & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(dataPop10000[msis_outbreak==FALSE & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(dataPop10000[msis_outbreak==TRUE & s_status=="Normal"]) 

TP1
TN1
FP1
FN1


# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(dataPop10000[msis_outbreak==TRUE & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(dataPop10000[msis_outbreak==FALSE & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(dataPop10000[msis_outbreak==FALSE & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(dataPop10000[msis_outbreak==TRUE & s_status!="High"])

TP2
TN2
FP2
FN2


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
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis_pop10000.xlsx"))





##################################################
##  CALCULATIONS SP VS VESUV (GOLD STANDARD)    ## 
##################################################



# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM[vesuv_outbreak==1 & s_status!="Normal"])

# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM[vesuv_outbreak==1 & s_status=="Normal"]) 

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullDataSVM[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullDataSVM[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullDataSVM[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullDataSVM[vesuv_outbreak==1 & s_status!="High"])

TP2
TN2
FP2
FN2


# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV1"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="Medium+High",var="PPV1",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV1"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="Medium+High",var="NPV1",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS1"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="Medium+High",var="Sensitivity1",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC1"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="Medium+High",var="Specificity1",value=tmp)

# LVL2
# PPV = TP/TP+FP
(tmp <- TP2/(TP2+FP2))
results[["PPV2"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="High",var="PPV2",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN2/(TN2+FN2))
results[["NPV2"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="High",var="NPV2",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP2/(TP2+FN2))
results[["SENS2"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="High",var="Sensitivity2",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN2/(TN2+FP2))
results[["SPEC2"]] <- data.frame(Comp="SPvsVesuv(Gold)",Outbreak="High",var="Specificity2",value=tmp)


results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv.xlsx"))




## CALCULATIONS when pop > 5000

fullDataSVM5000 <- fullDataSVM[pop>5000]
nrow(fullDataSVM5000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & s_status=="Normal"]) 

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & s_status!="High"])

TP2
TN2
FP2
FN2


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

fullDataSVM50000 <- fullDataSVM[pop>50000]
nrow(fullDataSVM50000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & s_status=="Normal"]) 

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & s_status!="High"])

TP2
TN2
FP2
FN2


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

fullDataSVM10000 <- fullDataSVM[pop>10000]
nrow(fullDataSVM10000)


# Level 1:   No outbreak=normal / Outbreak=Medium+High
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & s_status!="Normal"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM10000[vesuv_outbreak==0 & s_status=="Normal"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullData1SVM0000[vesuv_outbreak==0 & s_status!="Normal"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & s_status=="Normal"]) 

TP1
TN1
FP1
FN1

# Level 2:  No outbreak=normal+medium / Outbreak=high
# TRUE POSITIVE= 
TP2 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & s_status=="High"])
# TRUE NEGATIVE= 
TN2 <- nrow(fullDataSVM10000[vesuv_outbreak==0 & s_status!="High"])
# FALSE POSITIVE= 
FP2 <- nrow(fullDataSVM10000[vesuv_outbreak==0 & s_status=="High"])
# FALSE NEGATIVE= 
FN2 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & s_status!="High"])

TP2
TN2
FP2
FN2


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





####################################################
##  CALCULATIONS MSIS VS VESUV (GOLD STANDARD)    ## 
####################################################


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 

TP1
TN1
FP1
FN1

# CALCULATE:
# PPV = TP/TP+FP
# NPV = TN/TN+FN
# sensitivity (TPR) =  TP/(TP+FN)
# specificity (SPC) = TN/(TN+FP)

results <- list()
# LVL1
# PPV = TP/TP+FP
(tmp <- TP1/(TP1+FP1))
results[["PPV"]] <- data.frame(Comp="MSISvsVesuv(Gold)",Outbreak="TRUE",var="PPV",value=tmp)

# NPV = TN/TN+FN
(tmp <- TN1/(TN1+FN1))
results[["NPV"]] <- data.frame(Comp="MSISvsVesuv(Gold)",Outbreak="TRUE",var="NPV",value=tmp)

# sensitivity (TPR) =  TP/(TP+FN)
(tmp <- TP1/(TP1+FN1))
results[["SENS"]] <- data.frame(Comp="MSISvsVesuv(Gold)",Outbreak="TRUE",var="Sensitivity",value=tmp)

# specificity (SPC) = TN/(TN+FP)
(tmp <- TN1/(TN1+FP1))
results[["SPEC"]] <- data.frame(Comp="MSISvsVesuv(Gold)",Outbreak="TRUE",var="Specificity",value=tmp)

results <- rbindlist(results)
results
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"msis_vs_vesuv.xlsx"))



## CALCULATIONS when pop > 5000

#fullData5000 <- fullData[pop>5000]
#nrow(fullData5000)


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM5000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM5000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


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

#fullData50000 <- fullData[pop>50000]
#nrow(fullData50000)


# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM50000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM50000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


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

#fullData10000 <- fullData[pop>10000]
#nrow(fullData10000)

# No outbreak= FALSE / Outbreak= TRUE
# TRUE POSITIVE= 
TP1 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & msis_outbreak!="FALSE"])
# TRUE NEGATIVE= 
TN1 <- nrow(fullDataSVM10000[vesuv_outbreak==0 & msis_outbreak=="FALSE"]) 
# FALSE POSITIVE= 
FP1 <- nrow(fullDataSVM10000[vesuv_outbreak==0 & msis_outbreak!="FALSE"])
# FALSE NEGATIVE= 
FN1 <- nrow(fullDataSVM10000[vesuv_outbreak==1 & msis_outbreak=="FALSE"]) 


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












########################################################################################################
## CALCULATIONS SP vs MSIS (gold) - THRESHOLDs


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

openxlsx::write.xlsx(na.omit(results), file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_msis_thresholds.xlsx"))

xtabs(~mergedData$spthreshold2_vs_msis)













###########################################################################################
## CALCULATIONS SP vs VESUV (gold) - THRESHOLDs

fullDataSVM[,spthreshold2_vs_vesuv:=as.character(NA)]
fullDataSVM[vesuv_outbreak==1 & s_status!="Normal", spthreshold2_vs_vesuv:="TP"]
fullDataSVM[vesuv_outbreak==0 & s_status=="Normal", spthreshold2_vs_vesuv:="TN"]
fullDataSVM[vesuv_outbreak==0 & s_status!="Normal", spthreshold2_vs_vesuv:="FP"]
fullDataSVM[vesuv_outbreak==1 & s_status=="Normal", spthreshold2_vs_vesuv:="FN"]
xtabs(~fullDataSVM$spthreshold2_vs_msis)

# Level 2:  No outbreak=normal+medium / Outbreak=high

fullDataSVM[,spthreshold4_vs_msis:=as.character(NA)]
fullDataSVM[vesuv_outbreak==1 & s_status=="High", spthreshold4_vs_vesuv:="TP"]
fullDataSVM[vesuv_outbreak==0 & s_status!="High", spthreshold4_vs_vesuv:="TN"]
fullDataSVM[vesuv_outbreak==0 & s_status=="High", spthreshold4_vs_vesuv:="FP"]
fullDataSVM[vesuv_outbreak==1 & s_status!="High", spthreshold4_vs_vesuv:="FN"]


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

long <- melt.data.table(fullDataSVM[,c(
  "location","year","week","n","spthreshold2_vs_vesuv","spthreshold4_vs_vesuv"
),with=F],id.vars = c("location","year","week","n"))

results <- long[,.(
  tp=TP(value),
  fn=FN(value),
  ppv=PPV(value),
  npv=NPV(value),
  sens=SENS(value),
  spec=SPEC(value)
),keyby=.(variable)]

na.omit(results)

openxlsx::write.xlsx(na.omit(results), file=file.path(SHARED_FOLDER_TODAY,"sykdomspulsen_vs_vesuv_thresholds.xlsx"))

xtabs(~fullDataSVM$SP_vs_VESUV_threshold2_1)











