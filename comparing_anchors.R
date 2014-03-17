
data = read.csv('anchor_configuration_results.csv')
head(data)
plays = dim(data)[1]
configs = dim(data)[2]-1
plot(rep(1,plays),data$config1,xlim=c(1,10),type='n',axes=F,xlab='Anchor configuration',ylab='CCA predicted date',ylim=c(1615,1588))
for (i in 2:(configs)) {
  segments(rep(i-1,plays),data[,i],rep(i,plays),data[,i+1])
}
axis(1,at = 1:9)
axis(2)
text(rep(9.2,plays),data[,10],data$abbrv,adj=c(0,0),cex=.4)

data$abbrv[order(data$config5)]
par(mar=c(4,5,2,4))
plot(data$config5[order(data$config5)],ylim=c(1615,1585),las=1,ylab='Predicted Year from CCA',pch=19,col='lightgrey',cex=1.1)
abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
axis(4,las=1)
text(1:41,data$config5[order(data$config5)],data$abbrv[order(data$config5)],cex=.75,adj=c(.5,-1))
text(1:41,data$config5[order(data$config5)],round(data$config5[order(data$config5)])-c(rep(1500,25),rep(1600,41-25)),cex=.75,adj=c(.5,1.75))

round(data$config5[order(data$config5)])-c(rep(1500,25),rep(1600,41-25))
