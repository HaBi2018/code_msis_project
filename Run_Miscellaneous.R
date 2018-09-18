# Aggregate/count outbreak signals pr system, pr year

results<-fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]


openxlsx::write.xlsx(results,file=file.path(SHARED_FOLDER_TODAY,"signaler.xlsx"))

results
mean(results$numS)
# mean(results$numM)  - calculate manually for year 2012-2017
mean(results$numV)

#Graph

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
p <- ggplot(data=long[year==2017], mapping=aes(x=xValue,y=value,group=variable,colour=variable))
p <- p + geom_line()
p <- p + scale_x_continuous("Ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_2.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)




# Aggregate/count outbreak signals pr system, pr year, level 2 (SP=high)

resultsSignals2<-fullDataSVM[,.(
  numShigh=sum(s_status=="High"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]


openxlsx::write.xlsx(resultsSignals2,file=file.path(SHARED_FOLDER_TODAY,"signaler_high.xlsx"))

resultsSignals2
mean(resultsSignals2$numShigh)



# Aggregate registrations in SP and MSIS pr year

resultsReg<-fullDataSVM[,.(
  numSreg=sum(n), 
  numMreg=sum(num)
),by=.(year)]

openxlsx::write.xlsx(resultsReg,file=file.path(SHARED_FOLDER_TODAY,"registrations_year.xlsx"))

resultsReg

mean(resultsReg$numSreg)
mean(resultsReg$numMreg)



# Look at big known outbreaks

# Røros, municip5025, 2007, week 20

fullDataSVM[location=="municip5025" & year==2007 & week==20]
