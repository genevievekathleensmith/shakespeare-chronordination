#install.packages('scales')
#install.packages('ca')

data = read.csv('~/Documents/shakespeare-chronordination/SH DATA all counts A minus C plus ARD.csv')

library(scales)
library(ca)

dates = read.csv('~/Documents/shakespeare-chronordination/old_dates.csv')
dates = dates[,c(4,2)]

data = merge(data,dates)
data = data[order(data$oxford),]

props = prop.table(as.matrix(data[,3:11]),1)
head(props)

library(gplots)
svg("~/Documents/shakespeare-chronordination/heatmap_bw.svg",width=5)
heatmap.2(prop.table(as.matrix(data[,3:11]),1),Rowv=F,Colv=F,trace='none',key=F,keysize=.5,labRow=paste(data$Title,data$oxford,sep=", "), lmat=rbind(c(0,3,4),c(2,1,0)), lhei = c(.02,2.2),lwid = c(.3,1,1),labCol=NA,col=grey(c(100:1/100)),dendrogram='none')
#layout.show()
#axis(1,at = seq(.115,.4,length.out=9),labels=c(1:9),cex.axis=.5,lty=0)
#axis(1)
legend(.75,.35,col=grey(seq(0,1,.1)),legend=c('100 %',rep('',4),'50 %',rep('',4),'0%'),box.lty=0,cex=.75,pch=15,y.intersp=.7,pt.cex=1.25,title='Proportion\nof pauses\n')
dev.off()


svg('~/Documents/shakespeare-chronordination/test.svg',width=5,height=5)
par(mfcol=c(6,7))
par(mar=c(2,1,0,1))
for (i in 1:dim(props)[1]) {
  plot(colMeans(props),type='l',lwd=3,col='lightgrey',las=1,ylim=c(0,.6),axes=F,xlab='',ylab='')
  abline(h=c(0,.2,.4,.6),lty=2,col='lightgrey')
  lines(props[i,],lwd=3)
  text(8.5,.5,data$abbrv[i],cex=.8,font=2,adj=c(1,1))
  if (i%%6==0) {axis(1,at=c(1,9),labels=c('1st','9th'),cex=.5)}
}
dev.off()


svg('~/Documents/shakespeare-chronordination/test2.svg',width=6,height=4)
par(mfcol=c(1,3))
par(mar=c(5,4,1,1))

plot(1:9,props[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
for (i in which(data$oxford<1593)) {
  lines(1:9,props[i,],lwd=3,type='l')
}
axis(1,at=c(1,9),labels=c('1st','9th'))
axis(2,las=1)
text(8.5,.4,'Plays written before 1593\n(7 plays)',cex=.8,font=2,adj=c(1,1))

plot(1:9,props[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
for (i in which(data$oxford>=1598 & data$oxford<=1601)) {
  lines(1:9,props[i,],lwd=3,type='l')
}
axis(1,at=c(1,9),labels=c('1st','9th'))
#axis(2,las=1)
text(8.5,.4,'Plays written between 1598 and 1601\n(6 plays)',cex=.8,font=2,adj=c(1,1))


plot(1:9,props[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
for (i in which(data$oxford>1607)) {
  lines(1:9,props[i,],lwd=3,type='l')
}
axis(1,at=c(1,9),labels=c('1st','9th'))
#axis(2,las=1)
text(8.5,.4,'Plays written after 1607\n(6 plays)',cex=.8,font=2,adj=c(1,1))
dev.off()
