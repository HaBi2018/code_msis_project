# Aggregate/count outbreak signals pr system, pr year

fullDataSVM[,.(
    numS=sum(s_status!="Normal"), 
    numM=sum(msis_outbreak==T),
    numV=sum(vesuv_outbreak==1)
  ),by=.(year)]
  

#Graph

fullDataSVM[,.(
  numS=sum(s_status!="Normal"), 
  numM=sum(msis_outbreak==T),
  numV=sum(vesuv_outbreak==1)
),by=.(year,week)]



# Yearly grafs showing outbreak signals/n pr system pr week pr year
