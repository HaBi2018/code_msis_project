
## INVESTIGATION - KNOWN OUTBREAKS IN VESUV


# Outbreak Ringerike, 2012, week 50
fullDataSVM[location=="municip0605" & (year==2012 & week==50)]

#Outbreak R�ros, 2007, week 20
fullDataSVM[location=="municip5025" & (year==2007 & week==20)]


#Outbreak Ringerike, 2017, week 11
fullDataSVM[location=="municip0605" & (year==2017 & week==11)]

#Outbreak Ringerike, 2017, week 11, lag +/- one week
fullDataSVM[location=="municip0605" & (year==2017 & week==10)]
fullDataSVM[location=="municip0605" & (year==2017 & week==12)]


#Outbreak Ullensaker, 2008, week 39
fullDataSVM[location=="municip0235" & (year==2008 & week==39)]

#Outbreak Hamar, 2008, week 36
fullDataSVM[location=="municip0403" & (year==2008 & week==36)]

#Outbreak M�lselv, 2014, week 19
fullDataSVM[location=="municip1924" & (year==2014 & week==19)]

#Outbreak �lesund, 2013, week 9
fullDataSVM[location=="municip1504" & (year==2013 & week==9)]

#Outbreak Stavanger, 2007, week 50
fullDataSVM[location=="municip1103" & (year==2007 & week==50)]


#Outbreak M�lselv, 2015, week 9
fullDataSVM[location=="municip1924" & (year==2015 & week==9)]

#Outbreak M�lselv, 2015, week 9, lag +/- one week
fullDataSVM[location=="municip1924" & (year==2015 & week==8)]
fullDataSVM[location=="municip1924" & (year==2015 & week==10)]



##  Strong outbreak signals in Sykdomspulsen      #####################################

#Looking at outbreak signals in SP with high magnitude

#fullDataSVM[s_status!="Normal" & n>(s_threshold6+15)]
#fullDataSVM[s_status!="Normal" & n>550]

#results<-fullDataSVM[s_status!="Normal" & n>(s_threshold6+15)]
#openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"SP_outbreaks_Magnitude.xlsx"))


results<-fullDataSVM[s_zscore>=6 & n>5] 
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"SP_outbreaks_zscore6.n5.xlsx"))

results<-fullDataSVM[s_zscore>=7 & n>5] 
openxlsx::write.xlsx(results, file=file.path(SHARED_FOLDER_TODAY,"SP_outbreaks_zscore7.n5.xlsx"))



