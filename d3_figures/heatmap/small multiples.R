
names = read.csv('~/Dropbox/Bruster & Smith pause manuscript/playnames.csv')



data = read.csv('~/Dropbox/Bruster & Smith pause manuscript/tabledata.csv')
head(data)

cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)

#par(mfrow=c(41,1))
#par(mfrow=c(6,7))
#par(mar=c(.3,.7,.3,.7))
#plot(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,],type='l',xlab='',ylab='',axes=F)

#lines(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,])

for (i in 1:10) {
  plot(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[i,],type='l',xlab='',ylab='',axes=F)
  polygon(x=seq(1,9,1),y=cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[i,],type='l',xlab='',ylab='',axes=F)
}
?axis

plot(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,],type='l',xlab='',ylab='',axes=F)


library(scales)
polygon(x=c(seq(1,9,1),seq(9,1,-1)),y=c(cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,],rep(0,9)),col=alpha('black',.2))

par(mfrow=c(6,6))
par(mar=c(1,1,1,1))
#plot(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,],type='l',xlab='',ylab='',axes=F)

#lines(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[1,])

for (i in 1:36) {
  plot(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[i,],type='n',xlab='',ylab='',axes=F,ylim=c(0,750))
#  abline(h=c(250,500,750),col='grey',lty=3)
  polygon(x=c(seq(1,9,1),seq(9,1,-1)),y=c(cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[i,],rep(0,9)),border=NA,col=alpha('blue',.2))
  abline(h=c(250,500,750),col='white',lty=3,lwd=.8)
  lines(seq(1,9,1),cbind(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)[i,],lwd=1,col='blue')
  lines(c(1,9),c(0,0),lwd=.5)
}


min(data)
max(data)



head(data)

data.frame(data$first,data$second,data$third,data$fourth,data$fifth,data$sixth,data$seventh,data$eighth,data$ninth)


data = data/apply(data,1,sum)


first = data.frame(names,rep('first',length(names)),data$first)
second = data.frame(names,rep('second',length(names)),data$second)
third = data.frame(names,rep('third',length(names)),data$third)
fourth = data.frame(names,rep('fourth',length(names)),data$fourth)
fifth = data.frame(names,rep('fifth',length(names)),data$fifth)
sixth = data.frame(names,rep('sixth',length(names)),data$sixth)
seventh = data.frame(names,rep('seventh',length(names)),data$seventh)
eighth = data.frame(names,rep('eighth',length(names)),data$eighth)
ninth = data.frame(names,rep('ninth',length(names)),data$ninth)
names(first) = c('play','pause','prop')

names(second) = names(first)
names(third) = names(first)
names(fourth) = names(first)
names(fifth) = names(first)
names(sixth) = names(first)
names(seventh) = names(first)
names(eighth) = names(first)
names(ninth) = names(first)

final = rbind(first,second,third,fourth,fifth,sixth,seventh,eighth,ninth)

final$play = rep(seq(1,41,1),9)
final$pause = c(rep(1,41),rep(2,41),rep(3,41),rep(4,41),rep(5,41),rep(6,41),rep(7,41),rep(8,41),rep(9,41))

write.csv(final,'~/Documents/d3 working group/shakespeare/heatmap/sh_data.csv',row.names=F)
