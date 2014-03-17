
setwd("~/Documents/shakespeare-chronordination")
library(scales)
data = read.csv('cca_prediction_march2014.csv')

plot(data$cca_prediction[order(data$cca_prediction)],ylim=c(1615,1585),las=1,ylab='Predicted Year from CCA',pch=19,col='lightgrey',cex=1.1)
abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
#axis(4,las=1)
text(1:41,data$cca_prediction[order(data$cca_prediction)],data$Title[order(data$cca_prediction)],cex=.75,adj=c(.5,-1))
text(1:41,data$cca_prediction[order(data$cca_prediction)],round(data$cca_prediction[order(data$cca_prediction)])-c(rep(1500,25),rep(1600,41-25)),cex=.75,adj=c(.5,1.75))

boot = read.table('monday_temp.txt')
names = read.csv('cca_bootstrap_play_titles.csv',header=F)
#names = read.table('raw_data_titles.txt')
names = names[,1]
names

boot_means = apply(boot,1,mean)
plot(sort(boot_means),ylim=c(1615,1590),type='n',ylab='Predicted Year from CCA',las=1)
x = apply(boot,1,quantile,probs=c(.025,.975))
abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
segments(c(1:41),x[1,][order(boot_means)],c(1:41),x[2,][order(boot_means)],col='darkgrey')
points(sort(boot_means),pch=19,col='darkgrey')
text(1:41,sort(boot_means)+2,names[order(boot_means)],cex=.5)

cbind(boot_means,names)

output = data.frame(names,boot_means,t(x))
names(output) = c('play','mean_cca','lower_95','upper_95')
head(output)


combined = merge(data[,1:2],output,by.x='Title',by.y='play')
plot(combined$cca_prediction,combined$mean_cca,type='n',xlab='CCA prediction',ylab='Bootstrap mean')
text(combined$cca_prediction,combined$mean_cca,combined$Title,cex=.6)
abline(a=0,b=1)

write.csv(combined,'cca_bootstrap_confidence_intervals.csv',row.names=F)

theaters = read.csv('playhouse_status.csv')
plot(theaters,type='l')

all(theaters$date[theaters$status=='closed']-theaters$date[theaters$status=='open'] < 0)
sum(theaters$status=='closed')
sum(theaters$status=='open')

polygon(c(0,0,41,41),theaters$date[c(1,2,2,1)])
polygon(c(0,0,41,41),theaters$date[c(3,4,4,3)])

for (i in seq(1,38-1,2)) {
  polygon(c(0,0,41,41),theaters$date[c(i,i+1,i+1,i)],col=alpha('black',.1),border=NA)
}
