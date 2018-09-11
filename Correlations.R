####################################
## CORRELATIONS - SP vs MSIS
####################################


## CORRELATION - MUNICIPALITY LEVEL  ####################################

N <- mergedData[,
                .(
                  n=sum(n),
                  num=sum(num)
                ),by=.(
                  location,
                  year,
                  week
                )]

plot(N$n~N$num)

corr <- na.omit(N[num<80
                  ,.(
                    corr0=cor(n,num)
                  ),by=.(year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year) ]


hist(corr$corr0)



# Correlation - municipality level, with lags, pr seasons


mergedData[,num_w_future1:=shift(num,n=1L,type="lead"),by=.(location)]
mergedData[,num_w_past1:=shift(num,n=1L,type="lag"),by=.(location)]


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

plot(N$n~N$num)

corr <- na.omit(N[num<80
                  ,.(
                    corr0=cor(n,num)
                  ),by=.(year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]


hist(corr$corr0)









