# Aggregate/count outbreak signals pr system, pr year

fullDataSVM[,.(
    numS=sum(s_status!="Normal"), 
    numM=sum(msis_outbreak==T),
    numV=sum(vesuv_outbreak==1)
  ),by=.(year)]
  

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
p





##############################################
# Lagre tabelloversikt for signaler i excel

results<-fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year)]

na.omit(results)

openxlsx::write.xlsx(na.omit(results), file=file.path("C:/Users/Hanne/Documents/NMBU/Masteroppgave/Analayser i R/180830","signaler.xlsx"))




###############################################
# Aggregate all "double" rows in Vesuv by municp, year, week, and sum Antall

# Dataset Vesuv = vFull
nrow(vFull) #nrow = 537

#vFull<- tapply(vFull$Antall, vFull[, c("location", "year","week","vesuv_outbreak" )], sum)


#vFull <-vFull[,.(vFull$Antall),by=.(location, year,week, vesuv_outbreak,Antall)]
nrow(vFull) #nrow=533

vFull <-vFull[,.(vFull$Antall),by=.(location, year,week, vesuv_outbreak)]
