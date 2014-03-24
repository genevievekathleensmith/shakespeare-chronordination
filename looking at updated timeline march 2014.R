
setwd("~/Documents/shakespeare-chronordination")
library(scales)
#data = read.csv('cca_prediction_march2014.csv')
data = read.csv('cca_prediction_march2014_tempest.csv')


#boot = read.table('boots.txt')
boot = read.table('boots_tempest.txt')
names = read.csv('cca_bootstrap_play_titles.csv',header=F)
#names = read.table('raw_data_titles.txt')
names = names[,1]

boot_means = apply(boot,1,mean)
x = apply(boot,1,quantile,probs=c(.025,.975))
status = read.csv('playhouse_status_august_2013.csv')

open = status$date[status$status=='open']
close = status$date[status$status=='closed']


output = data.frame(names,boot_means,t(x))
names(output) = c('play','mean_cca','lower_95','upper_95')


combined = merge(data[,1:2],output,by.x='Title',by.y='play')
levels(combined$Title)[which(levels(combined$Title)=='ALL')] = 'AWW'
levels(combined$Title)[which(levels(combined$Title)=='KJ')] = 'JN'
levels(combined$Title)[which(levels(combined$Title)=='LEA')] = 'LR'
levels(combined$Title)[which(levels(combined$Title)=='RJ')] = 'ROM'
levels(combined$Title)[which(levels(combined$Title)=='TEM')] = 'TMP'
levels(combined$Title)[which(levels(combined$Title)=='TC')] = 'TRO'
levels(combined$Title)[which(levels(combined$Title)=='MW')] = 'WIV'
levels(combined$Title)[which(levels(combined$Title)=='WIN')] = 'WT'





plot(sort(combined$cca_prediction),ylim=c(1615,1590),type='n',ylab='Predicted Year from CCA',las=1)
points(1:41,combined$cca_prediction[order(combined$cca_prediction)])
anchors = which(combined$Title[order(combined$cca_prediction)]=='3H6'|combined$Title[order(combined$cca_prediction)]=='JC'|combined$Title[order(combined$cca_prediction)]=='PER')
points(c(1:41)[anchors],combined$cca_prediction[order(combined$cca_prediction)][anchors],col='black',pch=19)
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}
text(1:41,sort(combined$cca_prediction)+.5,combined$Title[order(combined$cca_prediction)],cex=.5)
text(1:41,combined$cca_prediction[order(combined$cca_prediction)]+1,round(combined$cca_prediction[order(combined$cca_prediction)])-c(rep(1500,25),rep(1600,41-25)),cex=.5,adj=c(.5,1.75))



plot(sort(combined$mean_cca),ylim=c(1615,1590),type='n',ylab='Predicted Year from CCA',las=1)
#abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
segments(c(1:41),combined$lower_95[order(combined$mean_cca)],c(1:41),combined$upper_95[order(combined$mean_cca)],col='darkgrey')
points(sort(combined$mean_cca),pch=19,col='darkgrey')
text(1:41,sort(combined$mean_cca)+2,combined$Title[order(combined$mean_cca)],cex=.5)
anchors = which(combined$Title[order(combined$mean_cca)]=='3H6'|combined$Title[order(combined$mean_cca)]=='JC'|combined$Title[order(combined$mean_cca)]=='PER')
points(c(1:41)[anchors],combined$cca_prediction[order(combined$cca_prediction)][anchors],col='black',pch=19)
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}


plot(sort(combined$mean_cca),ylim=c(1615,1590),type='n',ylab='Predicted Year from CCA',las=1)
#abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
segments(c(1:41),combined$lower_95[order(combined$mean_cca)],c(1:41),combined$upper_95[order(combined$mean_cca)],col='darkgrey')
points(sort(combined$mean_cca),pch=19,col='darkgrey')
text(1:41,sort(combined$mean_cca)+2,combined$Title[order(combined$mean_cca)],cex=.5)
points(1:41,combined$cca_prediction[order(combined$mean_cca)])
anchors = which(combined$Title[order(combined$mean_cca)]=='3H6'|combined$Title[order(combined$mean_cca)]=='JC'|combined$Title[order(combined$mean_cca)]=='PER')
points(c(1:41)[anchors],combined$cca_prediction[order(combined$cca_prediction)][anchors],col='black',pch=19)
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}



plot(sort(combined$mean_cca),ylim=c(1615,1590),type='n',ylab='Predicted Year from CCA',las=1)
#abline(h=c(1590,1600,1610),lty=3,col='lightgrey',lwd=2)
segments(c(1:41),combined$lower_95[order(combined$cca_prediction)],c(1:41),combined$upper_95[order(combined$cca_prediction)],col='darkgrey')
points(combined$mean_cca[order(combined$cca_prediction)],pch=19,col='darkgrey')
text(1:41,sort(combined$cca_prediction)+2,combined$Title[order(combined$mean_cca)],cex=.5)
points(1:41,sort(combined$cca_prediction))
anchors = which(combined$Title[order(combined$cca_prediction)]=='3H6'|combined$Title[order(combined$cca_prediction)]=='JC'|combined$Title[order(combined$cca_prediction)]=='PER')
points(c(1:41)[anchors],combined$cca_prediction[order(combined$cca_prediction)][anchors],col='black',pch=19)
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}





write.csv(combined,'cca_bootstrap_confidence_intervals_march2014.csv',row.names=F)
