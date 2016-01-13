#! /usr/bin/Rscript --vanilla 
args <- commandArgs(TRUE)
temps <- read.csv(args[1],colClasses=c("character","numeric"))
temps$date<-as.POSIXlt(temps$date)
data_date<-strsplit(as.character(mean(temps$date))," ")[[1]][1]

# Uncomment to reverse the data to the correct order in ascending time if it comes in backwards.
# 
#temps<-temps[nrow(temps):1,]
#

plotname<-paste("pressure-plot-",data_date,".png",sep="")
png(plotname,width=850,height=500)
plot(x=temps$date,y=temps$amount,type="l", xaxt="n",ylim=c(95000,110000),main=paste("Pressure vs. Time for",data_date,sep=" "),xlab="Time",ylab="Pressure (Pa)")
ticklocs=round.POSIXt(quantile(temps$date,seq(from=0,to=1,length.out = 25)),units = "hours")
axis.POSIXct(1,at=ticklocs,format="%l%p",labels=TRUE,tick=TRUE)
dev.off()