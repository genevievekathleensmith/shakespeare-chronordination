
### Analysis of Pause Positions in Shakespeare's Plays
### GK Smith 2014

# install and load necessary libraries
#install.packages('scales')
#install.packages('ca')
#install.packages('gplots)

library(scales)
library(ca)
library(gplots)

# read in raw pause count data and standard date data, merge together by play
counts = read.csv('~/Documents/shakespeare-chronordination/SH DATA.csv')
dates = read.csv('~/Documents/shakespeare-chronordination/oxford_dates.csv')
data = merge(counts,dates)

# sort data by original publication date, rename rows in sequential order, calculate pause proprotions
data = data[order(data$oxford),]
row.names(data) = 1:dim(data)[1]
props = prop.table(as.matrix(data[,3:11]),1)

# generate list of pause names with superscipts
pause_names = c(expression(paste(1^'st')),expression(paste(2^'nd')),expression(paste(3^'rd')),expression(paste(4^'th')),expression(paste(5^'th')),expression(paste(6^'th')),expression(paste(7^'th')),expression(paste(8^'th')),expression(paste(9^'th')))

# compile early, middle, and late period play proportions and set variables for plotting
early = props[data$oxford<1593 & data$abbrv != 'ARD',]
middle = props[data$oxford>=1598 & data$oxford<=1601,]
late = props[data$oxford>1607,]
periods = list(early,middle,late)
labs = c('Plays written before 1593\n(7 plays)\n','Plays written between\n1598 and 1601\n(6 plays)','Plays written after 1607\n(6 plays)\n')
ylabs = c('Percentage of pauses','','')
xlabs = c('','Pause position','')

# run PCA on proprotions
pca_results = prcomp(props)
# select 5 target plays
plays_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
# find their row indicies
play_indicies = charmatch(plays_of_interest,data$Title)

# run CA on counts
ca_results = ca(data[,3:11])

# load function for bootstrapping CA results
source('~/Documents/shakespeare-chronordination/ca_boot.R')
# run bootstrap CA on counts
#out = ca_boot_function(data[,3:11])

# read in count data for multi-author plots
multi = read.csv('~/Documents/shakespeare-chronordination/multiauthor counts for CONVEX HULLS.csv')
# run bootstrapped CA on multi-author counts
multi_out = ca_boot_function(multi[,3:11])
# find indicies of plays by Shakespeare
sh_ind = which(multi$Author=='Shakespeare')
# find indicies for plays of interest
multi_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
multi_indicies = charmatch(multi_of_interest,multi$Title)

# find indicies for plays by each author, set some plotting variables
fl = which(multi$Author=='Fletcher')
jo = which(multi$Author=='Jonson')
ky = which(multi$Author=='Kyd')
ml = which(multi$Author=='Marlowe')
mr = which(multi$Author=='Marston')
pe = which(multi$Author=='Peele')
authors = list(ml,ky,pe,mr,jo,fl)
author_names = c('Marlowe','Kyd','Peele','Marston','Jonson','Fletcher')

# read in results from MATLAB CCA analysis
matlab = read.csv('~/Documents/shakespeare-chronordination/MATLAB/CCA_rawpredictions.csv',header=F)
matlab$Title = data$Title[order(data$abbrv)]
anchors = c('3 Henry 6','Henry 5','Pericles')
matlab_indicies = charmatch(anchors,matlab$Title)
x_vals = matlab$V1[matlab_indicies]
ca_indicies = charmatch(anchors,data$Title)
y_vals = ca_results$rowcoord[ca_indicies,1]
mod = lm(y_vals~x_vals)

# read in Tarlinskaja data
tar = read.csv('~/Documents/shakespeare-chronordination/tarlinskaja April 21 Rev Bruster.csv')
tar_full = na.omit(tar)
# find indicies of plays of interest in Tarlinskaja dataset
tar_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
tar_indicies = charmatch(tar_of_interest,tar_full$title)
# run PCA on Tarlinskaja data
tar_pca = prcomp(tar_full[,4:46],scale.=T,center=T)
# rescale PC1 scores to years
oneway = data.frame(tar_full$title,rescale(-tar_pca$x[,1],to=c(1589.5,1614)))
anotherway = data.frame(tar_full$title,rescale(tar_pca$x[,1],to=c(1614,1589.5)))
# these should match each other:
#head(oneway)
#head(anotherway)
# build results file for output
names(oneway) = c('Title','Tarlinskaja_prediction')
write.csv(oneway,'~/Dropbox/Bruster & Smith pause manuscript/tarlinskaja_pca_predictions.csv',row.names=F)
write.csv(oneway,'~/Documents/shakespeare-chronordination/tarlinskaja_pca_predictions.csv',row.names=F)

# read in playhouse status data
status = read.csv('~/Documents/shakespeare-chronordination/playhouse_status.csv')
open = status$date[status$status=='open']
close = status$date[status$status=='closed']

# read in bootstrapped results from MATLAB CCA
boots = read.csv('~/Documents/shakespeare-chronordination/MATLAB/CCA_bootpredictions.csv',header=F)
boots$Title = matlab$Title
temp = data.frame(matlab,boots)
temp = temp[order(temp$V1),]
# calculate mean bootstrapped year
mean_boots = apply(temp[,3:1002],1,mean)
# calculate upper and lower 95% confidence intervals from bootstrapped years
upper_boots = apply(temp[,3:1002],1,quantile,probs=0.975)
lower_boots = apply(temp[,3:1002],1,quantile,probs=0.025)

# shift plays around opening/closing of theaters
shifted = c(temp$V1[temp$V1<close[1]], rescale(temp$V1[temp$V1 > close[1] & temp$V1 <= temp$V1[temp$Title=='Henry 5']],to=c(upper_boots[temp$Title=='2 Henry 6'],temp$V1[temp$Title=='Henry 5'])), temp$V1[temp$V1>temp$V1[temp$Title=='Henry 5']])

# compile data together
step1 = data.frame(data$Title,data$oxford)
step2 = data.frame(temp$Title,shifted)
names(step1) = c('title','oxford')
names(step2) = c('title','shifted')
step3 = merge(step1,step2)
step3 = step3[order(step3$shifted),]


### FIGURES ###

svg("~/Documents/shakespeare-chronordination/heatmap_bw.svg",width=5)
heatmap.2(prop.table(as.matrix(data[,3:11]),1),Rowv=F,Colv=F,trace='none',key=F,keysize=.5,labRow=paste(data$Title,data$oxford,sep=", "), lmat=rbind(c(0,3,4),c(2,1,0)), lhei = c(.02,2.2),lwid = c(.3,1,1),labCol=NA,col=grey(c(100:1/100)),dendrogram='none')
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


setEPS()
postscript("~/Documents/shakespeare-chronordination/figure1.eps",width=6.5,height=3)
par(mfcol=c(1,3))
par(mar=c(4.1,4,4,0.5))
for (i in 1:length(periods)){
  plot(1:9,periods[[i]][1,],ylim=c(0,.4),type='n',axes=F,xlab=xlabs[i],ylab=ylabs[i])
  abline(h=c(0,.2,.4),lty=2,col='darkgrey')
  x = apply(periods[[i]],2,t.test)
  y = vector()
  for (j in 1:9) { y = c(y,x[[j]]$conf.int[1]) }
  z = vector()
  for (j in 1:9) { z = c(z,x[[j]]$conf.int[2]) }
  polygon(c(1:9,9:1),c(y,rev(z)),border=NA,col='darkgrey',density=NA)
  lines(1:9,apply(periods[[i]],2,mean),lwd=3)
  #  axis(1,at=c(1,9),labels=c('1st','9th'))
  axis(1,at=1:9,labels=pause_names,cex.axis=.7)
  axis(2,las=1,at=seq(0,.4,.1),labels=seq(0,40,10))
  #  text(9,.38,labs[i],cex=.9,font=1,adj=c(1,1))
  title(labs[i],cex.main=1,adj=0)
}
dev.off()

setEPS()
postscript("~/Documents/shakespeare-chronordination/figure2.eps",width=6.5,height=3)
par(mfcol=c(1,2))
par(mar=c(4.1, 4.1, 0.1, 0))
par(cex=.7)
plot(pca_results$x,xlim=c(-.2,.2),ylim=c(-.2,.2),asp=1,las=1,xlab='Principal Component Axis 1',ylab='Principal Component Axis 2',axes=F)
abline(h=0,lty=2,col='darkgrey')
abline(v=0,lty=2,col='darkgrey')
text(c(-.15,-.1,.05,.15,.15),c(-.1,.12,-.17,-.12,.12),plays_of_interest,font=3)
segments(c(-.15,-.1,.05,.15,.15),c(-.1,.12,-.17,-.12,.12)*.9,pca_results$x[play_indicies,1],pca_results$x[play_indicies,2])
points(pca_results$x[play_indicies,1],pca_results$x[play_indicies,2],pch=21,bg='black')
axis(1,at=c(-.2,0,.2))
axis(2,las=1,at=c(-.2,0,.2))

plot(ca_results$rowcoord[,1],ca_results$rowcoord[,2],xlab='Correspondence Analysis Axis 1',ylab='Correspondence Analysis Axis 2',cex=sqrt(ca_results$rowmass)*10,las=1,axes=F,xlim=c(-2,2),ylim=c(-3.5,3.5))
abline(h=0,lty=2,col='darkgrey')
abline(v=0,lty=2,col='darkgrey')
text(c(-1.2,-1.3,-.3,1.2,1.2),c(-2.5,2.25,-3.2,-2.2,2.2),plays_of_interest,font=3)
segments(c(-1.2,-1.3,-.3,1.2,1.2),c(-2.5,2.25,-3.2,-2.2,2.2)*.9,ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2])
points(ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2],pch=21,bg='black',cex=sqrt(ca_results$rowmass[play_indicies])*10)
axis(1,at=c(-2,0,2))
axis(2,las=1,at=c(-3.5,0,3.5))
dev.off()


setEPS()
postscript("~/Documents/shakespeare-chronordination/figure3.eps",width=6.5,height=3)
par(mfcol=c(1,2))
par(mar=c(4.1, 4.1, 0.1, 0))
par(cex=.7)
plot(pca_results$rotation,xlim=c(-.5,.5),ylim=c(-.64,.64),asp=1,type='n',xlab='Principal Component Axis 1',ylab='Principal Component Axis 2',axes=F)
abline(h=0,lty=2,col='darkgrey')
abline(v=0,lty=2,col='darkgrey')
arrows(rep(0,9),rep(0,9),pca_results$rotation[,1],pca_results$rotation[,2],length=.1,lwd=2)
text(pca_results$rotation[,1]+c(.1,.15,.1,.15,-.1,-.1,.15,.11,.1),pca_results$rotation[,2],pause_names,cex=1.2)
axis(1,at=c(-.5,0,.5))
axis(2,las=1,at=c(-.5,0,.5))

plot(ca_results$colcoord[,1],ca_results$colcoord[,2],xlab='Correspondence Analysis Axis 1',ylab='Correspondence Analysis Axis 2',las=1,axes=F,xlim=c(-3,3),ylim=c(-5.5,5.5),type='n')
abline(h=0,lty=2,col='darkgrey')
abline(v=0,lty=2,col='darkgrey')
arrows(rep(0,9),rep(0,9),ca_results$colcoord[,1],ca_results$colcoord[,2],length=.1,lwd=2)
text(ca_results$colcoord[,1]+c(-.2,-.4,.55,-.25,0,.2,.5,.2,0),ca_results$colcoord[,2]+c(.2,0,+.25,-.25,-.75,-.5,0,-.5,1),pause_names,cex=1.2)
axis(1,at=c(-3,0,3))
axis(2,las=1,at=c(-4,0,4))
dev.off()

pdf("~/Documents/shakespeare-chronordination/figure4.pdf",width=6,height=6)
par(mfrow=c(1,1))
xlimit=-.75
ylimit=.7
plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='CA Axis 1',ylab='CA Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=2,col='darkgrey')
abline(v=0,lty=2,col='darkgrey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('darkgrey',.2),border=NA)}
centers = t(t(multi_out[[3]]) %*% t(as.matrix(multi[,3:11])))/apply(multi[,3:11],1,sum)
text(centers[multi_indicies,c(1,2)]*2,labels=multi$Title[multi_indicies],cex=.8,col='black',font=3)
for (i in multi_indicies) {polygon(multi_out[4][[1]][[i]],col=alpha('black',0.2))}
axis(1,at=c(-.7,0,.7))
axis(2,las=1,at=c(-.7,0,.7))
dev.off()

pdf('~/Documents/shakespeare-chronordination/figure5.pdf',width=8,height=6)
par(mfrow=c(2,3))
for (i in 1:length(authors)){
  plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='CA Axis 1',ylab='CA Axis 2',las=1,asp=1,axes=F)
  abline(h=0,lty=2,col='darkgrey')
  abline(v=0,lty=2,col='darkgrey')
  for (j in sh_ind) {polygon(multi_out[4][[1]][[j]],col=alpha('darkgrey',.2),border=NA)}
  for (k in authors[[i]]) {polygon(multi_out[4][[1]][[k]],col=alpha('black',.2))}
  axis(1,at=c(-.7,0,.7))
  axis(2,las=1,at=c(-.7,0,.7))
  title(main=author_names[i],adj=0)
}
dev.off()


setEPS()
postscript("~/Documents/shakespeare-chronordination/figure6.eps",width=6.5,height=4)
par(mfrow=c(1,1))
par(mar=c(4.1, 4.1, 0.1, 0))
par(cex=.7)
plot(x_vals,y_vals,axes=F,xlab='Year of composition',ylab='CA Axis 1 Score',xlim=c(1590,1614),pch=19,ylim=c(-1.5,1.5))
axis(1,at = seq(1590,1614,4))
axis(2,las=1)
abline(mod,lwd=2,lty=2)
text(x_vals+1,y_vals-.1,anchors,cex=.9,font=3,adj=c(0,0))
segments(rep(0,3),y_vals,x_vals,y_vals,lty=3)
segments(x_vals,y_vals,x_vals,rep(-2,3),lty=3)
segments(0,ca_results$rowcoord[data$Title=='Macbeth',1],matlab$V1[matlab$Title=='Macbeth'],mod$coefficients[1]+mod$coefficients[2]*matlab$V1[matlab$Title=='Macbeth'],lwd=2)
arrows(matlab$V1[matlab$Title=='Macbeth'],mod$coefficients[1]+mod$coefficients[2]*matlab$V1[matlab$Title=='Macbeth'],matlab$V1[matlab$Title=='Macbeth'],-1.6,length=.1,lwd=2)
text(matlab$V1[matlab$Title=='Macbeth']-.25,-1.5,'Macbeth\n  \n  ',cex=.9,adj=c(1,0),font=4)
text(matlab$V1[matlab$Title=='Macbeth']-.25,-1.5,paste('  \nCA score = ',round(ca_results$rowcoord[data$Title=='Macbeth',1],3),'\nPredicted year = ',matlab$V1[matlab$Title=='Macbeth'],sep=''),cex=.9,adj=c(1,0))
dev.off()

setEPS()
postscript("~/Documents/shakespeare-chronordination/figure7.eps",width=6.5,height=6.5)
plot(tar_pca$x[,1],tar_pca$x[,2],axes=F,xlab='Principal Component Axis 1',ylab='Principal Component Axis 2',asp=1,cex=1.2,xlim=c(9,-9),ylim=c(-6,6))
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
text(c(6,2,-2,-5,-8),c(5,3,-4,4,3),tar_full$title[tar_indicies],cex=.7,font=3)
segments(c(6,2,-2,-5,-8),c(5,3,-4,4,3)*.9,tar_pca$x[tar_indicies,1],tar_pca$x[tar_indicies,2])
points(tar_pca$x[tar_indicies,1],tar_pca$x[tar_indicies,2],pch=21,bg='black',cex=1.2)
axis(1)
axis(2,las=1)
dev.off()

setEPS()
postscript("~/Documents/shakespeare-chronordination/figure9.eps",width=6.5,height=6.5)
par(mfrow=c(1,1))
par(mar=c(4.1, 4.1, 0.1, 0))
par(cex=.7)
plot(temp$V1,1:42,xlim=c(1590,1620),pch=19,xlab='Time',ylab='Play order',axes=F,ylim=c(42,1),type='n')
for (i in 1:length(close)) {
  polygon(c(close[i],open[i],open[i],close[i]),c(0,0,42,42),col='lightgrey',border=NA)
}
segments(lower_boots,1:42,upper_boots,1:42)
points(temp$V1,1:42,pch=21,col='black',bg='white')
text(upper_boots+.5,1:42,temp$Title,cex=.7,adj=c(0),font=3)
axis(2,at=c(1,42),c('First','Last'),las=1)
axis(1)
points(shifted,1:42,col='black',pch=19)
dev.off()


setEPS()
postscript("~/Documents/shakespeare-chronordination/figure10.eps",width=6.5,height=6.5)
par(mfrow=c(1,1))
par(mar=c(4.1, 4.1, 0.1, 0))
par(cex=.7)
plot(step3$oxford,1:42,xlim=c(1590,1620),pch=19,xlab='Time',ylab='Play order',axes=F,ylim=c(42,1),type='n')
for (i in 1:length(close)) {
  polygon(c(close[i],open[i],open[i],close[i]),c(0,0,42,42),col='lightgrey',border=NA)
}
points(step3$oxford,1:42,pch=21,col='black',bg='white')
text(apply(step3[,2:3],1,max)+.5,1:42,temp$Title,cex=.7,adj=c(0),font=3)
axis(2,at=c(1,42),c('First','Last'),las=1)
axis(1)
arrows(step3$oxford,1:42,step3$shifted,1:42,length=.05,lwd=2)
dev.off()







to_export = data.frame(temp[,2],round(mean_boots,digits=2),round(upper_boots,digits=2),round(lower_boots,2),round(shifted,2))
names(to_export) = c('Title','mean_cca_bootstrap','upper_95_CI','lower_95_CI','Shifted_year')
write.csv(to_export,paste(paste('~/Documents/shakespeare-chronordination/data_table_for_doug',strsplit(date(),split=' ')[[1]][2],strsplit(date(),split=' ')[[1]][3],sep='_'),'.csv',sep=''),row.names=F)
