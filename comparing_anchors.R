
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
