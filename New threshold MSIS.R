
#############################
##   MSIS DATA             ##
#############################


# read in a "merging" dataset that says "which kommune goes to a different kommune"
mergingData <- GenNorwayMunicipMerging()

# read in the raw data
data <- data.table(readxl::read_excel("data_raw/msis_07_17.xlsx"))
# make a "municip" variable so that it is the same as in "mergingData"
data[,municip:=sprintf("municip%s",KommuneNr)]

# merge in the dataset that says "kommune X -> kommune Y"
nrow(data) # number of rows in the dataset before merging
data <- merge(data,mergingData,by.x=c("municip","ar"),by.y=c("municip","year"))
nrow(data) # number of rows in the dataset after merging -- ARE THESE THE SAME?????

# delete "municip", because we now use "municipEnd"
data[,municip:=NULL]
data[,Bokomm:=NULL]

setnames(data,"municipEnd","municip")

# Aggregate registrations in msis, pr municip, yr and week
data <- data[,.(num=.N),by=.(municip,ar,uke)]


# Create skeleton table to include all municip, years and weeks
skeleton <- data.table(expand.grid(
  municip=unique(mergingData[year==2018]$municipEnd),
  ar=2007:2017,
  uke=1:52
))

fullData <- merge(skeleton,data,by=c("municip","ar","uke"),all.x=T)
fullData[is.na(num),num:=0]
nrow(fullData)

#
fullData[,num1:=shift(num,n=1L,type="lag"),by=.(municip,uke)]
fullData[,num2:=shift(num,n=2L,type="lag"),by=.(municip,uke)]
fullData[,num3:=shift(num,n=3L,type="lag"),by=.(municip,uke)]
fullData[,num4:=shift(num,n=4L,type="lag"),by=.(municip,uke)]
fullData[,num5:=shift(num,n=5L,type="lag"),by=.(municip,uke)]

fullData[,last5yrAverage:=(num1+num2+num3+num4+num5)/5]

# take a look at the data examples
fullData[municip=="municip0301" & uke==3]
fullData[municip=="municip5054" & uke==30]

# create the thresholds
fullData[,threshold:=qpois(p=0.95,lambda=last5yrAverage)]
fullData[,outbreak:=(num>threshold)&(num!=1)]

# HANNE: MAYBE SMART TO RENAME SOME VARIABLES???
# THIS WAY WE CAN LABEL MSIS VARIABLES WITH THE PREFIX msis_
setnames(fullData,"outbreak","msis_outbreak")
setnames(fullData,"threshold","msis_threshold")
setnames(fullData,"last5yrAverage","msis_last5yrAverage")

nrow(fullData)
fullData

res2 <- fullData[msis_outbreak==T,.(
  numMregO=sum(num)
),by=.(ar, uke, municip)]

summary(res2)











# Graph signals 2017

breaks <- unique(long[,c("year","week","xValue")])
breaks <- breaks[week %in% c(seq(1,52,4))]
breaks[,label:=sprintf("%s-%s",year,week)]

print(breaks)
p <- ggplot(data=long[year==2017], mapping=aes(x=xValue,y=value,group=variable,colour=variable,))
p <- p + geom_line(size=1)
p <- p + scale_x_continuous("År og ukenummer",labels=breaks$label,breaks=breaks$xValue)
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + scale_y_continuous("Antall utbruddsignaler")

ggsave(filename = file.path(
  SHARED_FOLDER_TODAY,
  "figure_graphTEST.png"),
  height=210,
  width=297,
  units="mm",
  plot=p)







## Scatterplot - correlation N$n~N$num

plot(N$n~N$num, main="Sykdomspulsen og MSIS. Antall tilfeller av mage-tarminfeksjon",
     pch=1, frame =T, cex.main=0.8, cex.lab=0.7,
     xlab="Sykdomspulsen ", ylab="MSIS" )


axis(side=1, at=c(200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000), labels=c(200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000))
axis(side=2, at=c(50, 100, 150, 200, 250, 300, 350), labels = c(50, 100, 150, 200, 250, 300, 350))


# Regression line (not used)
# abline(lm(N$n~N$num))


plot(N$num~N$n, main="Sykdomspulsen og MSIS. Antall tilfeller av mage-tarminfeksjon", cex=0,5, pch=1, frame = FALSE,
     xlab="MSIS", ylab="Sykdomspulsen ")
























