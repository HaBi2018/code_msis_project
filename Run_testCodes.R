# Aggregate/count outbreak signals pr system, pr year

results<-fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]



# to aggregate
#[row,column,by]
#mergedData[year==2018,
#           .(
#             
#           ),keyby=.(
#             
#           )]


vFull <-vFull[,.(v_n=sum(Antall)),by=.(location, year,week, vesuv_outbreak)]


fullDataSVMA<-fullDataSVM[]



#location=="municip0301





fullDataSVM[,.(numS=sum(s_status!="Normal"),numM=sum(msis_outbreak==T),numV=sum(vesuv_outbreak==1)),by=.(year,location)]



##### SIGNALS PR. MUNICIP, ALL SYSTEMS IN 2017 - få signaler pr uke, gir ikke mye mening

toPlot <- fullDataSVM[location=="municip0301",.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year,week)]

setorder(toPlot,year,week)
toPlot[,xValue:=1:nrow(toPlot)]

library(ggplot2)

long <- melt.data.table(toPlot,id.vars = c("year","week", "xValue"))


breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2012], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("Date",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_2municip0301_2017.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)






















# KORRELASJON PR. ÅR sp OG MSIS

mergedData

mergedData[,num_w_future1:=shift(num,n=1L,type="lead"),by=.(location)]
mergedData[,num_w_past1:=shift(num,n=1L,type="lag"),by=.(location)]


corr <- na.omit(mergedData[
  ,.(
    corr_past1=cor(n,num_w_past1),
    corr0=cor(n,num),
    corr_future1=cor(n,num_w_future1)
  ),by=.(
    location,year
  )])

corr[,.(
  corr_past1=mean(corr_past1),
  corr0=mean(corr0),
  corr_future1=mean(corr_future1)
),keyby=.(year)]

corr[,.(
  corr_past1=mean(corr_past1),
  corr0=mean(corr0),
  corr_future1=mean(corr_future1)
),keyby=.()]

mean(corr$corr_past1)
mean(corr$corr0)
mean(corr$corr_future1)

hist(corr$corr0)






# GRAF 2012
toPlot <- fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year,week)]

setorder(toPlot,year,week)
toPlot[,xValue:=1:nrow(toPlot)]

library(ggplot2)

long <- melt.data.table(toPlot,id.vars = c("year","week", "xValue"))

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(1)]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)

p <- ggplot(data=long, mapping=aes(x=xValue,y=value))
p <- p + geom_bar(stat="identity")
p <- p + facet_wrap(~variable,ncol=1)
p <- p + scale_x_continuous("Date",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
#p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_1.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2012], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("Date",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_2_2012.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)






# GRAF ALL YEAR


toPlot <- fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year,week)]

setorder(toPlot,year,week)
toPlot[,xValue:=1:nrow(toPlot)]

library(ggplot2)

long <- melt.data.table(toPlot,id.vars = c("year","week", "xValue"))

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(1)]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)

p <- ggplot(data=long, mapping=aes(x=xValue,y=value))
p <- p + geom_bar(stat="identity")
p <- p + facet_wrap(~variable,ncol=1)
p <- p + scale_x_continuous("Date",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
#p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_1.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2007:2017], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("Date",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_2_allYear.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)










mergedData[,fylke:=substr(location,1,9)]

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








Korr <- mergedData[,
                   .(
                     n=sum(n),
                     num=sum(num)
                   ),by=.(
                     location,
                     year,
                     week
                   )]

plot(Korr$n~Korr$num)

corr <- na.omit(Korr[num<80
                     ,.(
                       corr0=cor(n,num)
                     ),by=.(location,year)])

corr[,.(
  corr0=mean(corr0,na.rm=T)
),keyby=.(year)]















