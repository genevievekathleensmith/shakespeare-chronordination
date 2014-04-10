

data = read.csv('~/Documents/shakespeare-chronordination/SH DATA all counts A minus C plus ARD.csv')

PC = prcomp(prop.table(as.matrix(data[,2:10]),1))
biplot(PC)

library(ca)
library(scales)
CA = ca(data[,2:10])
plot(CA)

plot(CA$rowcoord[,1],CA$rowcoord[,2],xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',cex=CA$rowmass*50,las=1)

CA_predictions = data.frame(rescale(CA$rowcoord[,1],to=c(1589,1614),from=c(min(CA$rowcoord[,1]),max(CA$rowcoord[,1]))),data$Title,data$abbrv)
names(CA_predictions)=c('Year','Title','abbrv')

CA_predictions = CA_predictions[order(CA_predictions$Year),]

plot(CA_predictions$Year)
text(1:42,CA_predictions$Year,CA_predictions$Title,cex=.5)

#Plotting Ed3
plot(c(1:9),data[data$Title=='Taming of the Shrew',2:10]/sum(data[data$Title=='Taming of the Shrew',2:10]),type='l',ylab='Proportion of pauses',xlab='Pause',las=1,ylim=c(0,.6))
lines(c(1:9),data[data$Title=='Arden',2:10]/sum(data[data$Title=='Arden',2:10]))


source('~/Documents/shakespeare-chronordination/ca_boot.R')

out = ca_boot_function(data[,2:10])
#par(mar=c(10,4,10,2))
xlimit=.4
ylimit=.7
plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1)
for (i in 1:42) {polygon(out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
text(t(t(out[[3]]) %*% t(as.matrix(data[,2:10])))/apply(data[,2:10],1,sum),labels=data$abbrv,cex=.6,col='black')

for (i in 42) {polygon(out[4][[1]][[i]],col=alpha('lightgrey',0))}

head(CA_predictions)


cca = read.csv('~/Documents/shakespeare-chronordination/CCA_predictions_april2014.csv',header=F)
head(cca)
names(cca)=c('Play','Year')
cca$Play=sub("'","",cca$Play)


cca = cca[order(cca$Year),]
plot(cca$Year,ylim=c(1615,1589))
text(1:42,cca$Year+.5,cca$Play,cex=.4)


CA_alpha = CA_predictions[order(CA_predictions$abbrv),]
cca_alpha = cca[order(cca$Play),]

plot(cca_alpha$Year,CA_alpha$Year)
text(cca_alpha$Year,CA_alpha$Year,cca_alpha$Play,cex=.7)
#abline(a=0,b=1,lty=2)
