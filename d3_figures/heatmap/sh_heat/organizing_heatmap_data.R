
library(gplots)

data = read.csv('../..//SH DATA all counts A minus C Ed Bruster 11-5 Smith 11-15.csv')

head(data)
dates = read.csv('../..//alldates.csv')
head(dates)
levels(data$abbrv)
levles(dates$abbrv)
merged = merge(data,dates,by.x='abbrv',by.y='abbrv')
merged = merged[order(merged$oxford),]

head(merged)
merged_props = prop.table(as.matrix(merged[,3:11]),1)

head(merged_props)
merged_props = data.frame(merged[,c(1,2,14)],merged_props)

heatmap.2(as.matrix(merged_props[,4:(4+8)]),Rowv=F,Colv=F,dendrogram='none',trace='none',key=F,keysize=.5,lmat=rbind(c(0,3,4),c(2,1,0)), lwid = c(.5,1,1),labCol=NA,col=grey(c(100:1/100)),labRow=paste(merged_props$Title,merged_props$oxford,sep=', '))

stacked_props = stack(merged_props[,c(1,4:12)])
head(stacked_props)

heat_data = data.frame(stacked_props,rep(merged_props$Title,9))
head(heat_data)
names(heat_data) = c('prop','pause','play')
simple_data = heat_data
levels(simple_data$pause)=c(8,5,1,4,9,2,7,6,3)
simple_data$play = c(rep(seq(1,41,1),9))
head(simple_data)

write.csv(simple_data,'simple_data.csv',row.names=F)
write.csv(heat_data,'heat_data.csv',row.names=F)
