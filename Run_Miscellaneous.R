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

p <- ggplot(data=long, mapping=aes(x=xValue,y=value))
p <- p + geom_bar(stat="identity")
p <- p + facet_wrap(~variable)
p


# Yearly grafs showing outbreak signals/n pr system pr week pr year
