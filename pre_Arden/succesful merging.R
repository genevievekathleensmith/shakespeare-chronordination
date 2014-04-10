
dates = read.csv('dates.csv')
head(dates)
dates = dates[,c(1,2,4)]

data = read.csv('cca_bootstrap_confidence_intervals_march2014.csv')
head(data)
names(data)[1]='abbrv'

merged = merge(data,dates)

merged$abbrv
tar = read.csv('Tarlinskaja_PCA_date_predictions.csv')
head(tar)


x = merge(tar,merged)

names(x)

plot(rep(1,41),x$riverside,xlim=c(0,4),ylim=c(1615,1585),type='n',axes=F,xlab='',ylab='')
segments(rep(0,41),x$oxford,rep(1,41),x$riverside)
segments(rep(1,41),x$riverside,rep(2,41),x$cca_prediction)
segments(rep(2,41),x$cca_prediction,rep(3,41),x$mean_cca)
axis(1,at =0:3,labels=c('river','ox','cca','boot'))
axis(2,las=1)
text(rep(3.5,41),x$mean_cca,labels=x$Title,cex=.4)



write.csv(x,'d3_figures/parallel/date_data.csv',row.names=F)
