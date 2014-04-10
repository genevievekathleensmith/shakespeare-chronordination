
library(scales)
data = read.csv('~/Documents/shakespeare-chronordination/sorted_cca_predictions_march2014.csv')

head(data)

status = read.csv('~/Documents/shakespeare-chronordination/playhouse_status_august_2013.csv')

open = status$date[status$status=='open']
close = status$date[status$status=='closed']

plot(data$cca_prediction,ylim=c(1615,1590),type='n')
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}
text(1:41,data$cca_prediction,data$Title,cex=.5)

data$cca_prediction[data$Title=='TGV']
data$cca_prediction[data$Title=='WIV']

data$cca_prediction[data$cca_prediction>=data$cca_prediction[data$Title=='TGV']&data$cca_prediction<=data$cca_prediction[data$Title=='WIV']]
shifted = rescale(data$cca_prediction[data$cca_prediction>=data$cca_prediction[data$Title=='TGV']&data$cca_prediction<=data$cca_prediction[data$Title=='WIV']],to=c(open[3],data$cca_prediction[data$Title=='WIV']))

rescaled = c(data$cca_prediction[data$cca_prediction<data$cca_prediction[data$Title=='TGV']],shifted,data$cca_prediction[data$cca_prediction>data$cca_prediction[data$Title=='WIV']])

plot(data$cca_prediction,rescaled)

plot(rescaled,ylim=c(1615,1590),type='p',ylab='Date',las=1)
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}
text(1:41,rescaled+1,data$Title,cex=.5)


head(data)
data = data.frame(data,rescaled)

write.csv(data,'~/Documents/shakespeare-chronordination/rescaled_for_early_closing.csv',row.names=F)


plot(data$mean_cca,pch=19,col='grey',ylim=c(1615,1590))
segments(1:41,data$lower_95,1:41,data$upper_95,col='grey')
for (i in 1:length(close)) {
  polygon(c(0,0,41,41),c(close[i],open[i+1],open[i+1],close[i]),col=alpha('black',.1),border=NA)
}
points(1:41,rescaled,pch=19,col='black')
text(1:41,data$mean_cca+2.5,data$Title,cex=.4)
points(1:41,data$cca_prediction,col='blue')

legend('topright',legend=c('CCA score','Bootstrap mean','Shifted prediction'),col=c('blue','grey','black'),pch=c(1,19,19),cex=.7)

data$Title
