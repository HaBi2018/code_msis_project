# Aggregate/count outbreak signals pr system, pr year

results<-fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]

results
openxlsx::write.xlsx(results,file=file.path(SHARED_FOLDER_TODAY,"signaler.xlsx"))



## Graph signals total ######

toPlot <- fullDataSVM[,.(
  Sykdomspulsen=sum(s_status!="Normal"), 
  MSIS=sum(msis_outbreak==T),
  Vesuv=sum(vesuv_outbreak==1)
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
p <- p + scale_x_continuous("Ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
#p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_1.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)

# Graph signals 2017

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2017], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("År og ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_y_continuous("Antall utbruddsignaler")

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_graph2017.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)

# Graph signals 2012

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2012], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("År og ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_y_continuous("Antall utbruddsignaler")

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_graph2012.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)


# Graph signals 2007

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2007], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("År og ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_y_continuous("Antall utbruddsignaler")

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_graph2007.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)

# Graph signals 2014

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2014], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("År og ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_y_continuous("Antall utbruddsignaler")

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_graph2014.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)



## AGGREGATIONS ####################################################################

# Aggregate/count outbreak signals pr system, pr year, level 2 (SP=high)

resultsSignals2<-fullDataSVM[,.(
  numShigh=sum(s_status=="High"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]


openxlsx::write.xlsx(resultsSignals2,file=file.path(SHARED_FOLDER_TODAY,"signaler_high.xlsx"))

resultsSignals2
mean(resultsSignals2$numShigh)



# Aggregate registrations in SP, Vesuv and MSIS pr year

resultsReg<-fullDataSVM[,.(
  numSreg=sum(n), 
  numMreg=sum(num),
  numVreg=sum(v_n,na.rm=T)
  ), by=.(year)]

openxlsx::write.xlsx(resultsReg,file=file.path(SHARED_FOLDER_TODAY,"registrations_year.xlsx"))

resultsReg

mean(resultsReg$numSreg)
mean(resultsReg$numMreg)



# Aggregate registrations/n in outbreaks pr system pr year

res1 <- fullDataSVM[s_status!="Normal",.(
  numSregO=sum(n)
),by=.(year)]

res2 <- fullDataSVM[msis_outbreak==T,.(
  numMregO=sum(num)
),by=.(year)]

res3 <- fullDataSVM[vesuv_outbreak==1,.(
  numVregO=sum(v_n,na.rm=T)
),by=.(year)]

results <- merge(res1,res2,by="year",all.x=T)
results <- merge(results,res3,by="year",all.x=T)
results

openxlsx::write.xlsx(results,file=file.path(SHARED_FOLDER_TODAY,"reg.IN.outbreaks_year.xlsx"))






##  Median & variation

res1 <- fullDataSVM[s_status!="Normal",.(
  numSregO=sum(n)
),by=.(year,week, location)]

summary(res1)


res2 <- fullDataSVM[msis_outbreak==T,.(
  numMregO=sum(num)
),by=.(year, week, location)]

summary(res2)


res3 <- fullDataSVM[vesuv_outbreak==1,.(
  numVregO=sum(v_n,na.rm=T)
),by=.(year, week, location)]

summary(res3)

