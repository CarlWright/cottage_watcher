#! /usr/bin/Rscript --vanilla 
args <- commandArgs(TRUE)
temps <- read.csv(args[1],colClasses=c("character","numeric"))
temps$date<-as.POSIXlt(temps$date)
data_date<-strsplit(as.character(mean(temps$date))," ")[[1]][1]
#temps<-temps[nrow(temps):1,]
plotname<-paste("pressure-plot-",data_date,".png",sep="")
png(plotname,width=850,height=500)
plot(x=temps$date,y=temps$amount,type="l", xaxt="n",ylim=c(95000,100000),main=paste("Pressure vs. Time for",data_date,sep=" "),xlab="Time\nDay",ylab="Pressure (Pa)")
axis.POSIXct(1,temps$date,format="%l:%H%p%n%a",labels=TRUE,tick=FALSE)
dev.off()