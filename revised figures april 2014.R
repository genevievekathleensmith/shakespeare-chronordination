#install.packages('scales')
#install.packages('ca')

data = read.csv('~/Documents/shakespeare-chronordination/SH DATA all counts A minus C plus ARD.csv')

library(scales)
library(ca)

dates = read.csv('~/Documents/shakespeare-chronordination/old_dates.csv')
dates = dates[,c(4,2)]

data = merge(data,dates)
data = data[order(data$oxford),]
row.names(data) = 1:dim(data)[1]

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

early = props[data$oxford<1593 & data$abbrv != 'ARD',]
middle = props[data$oxford>=1598 & data$oxford<=1601,]
late = props[data$oxford>1607,]

n.early=dim(early)[1]
n.mid=dim(middle)[1]
n.late=dim(late)[1]

pdf('~/Documents/shakespeare-chronordination/figure1.pdf',width=8,height=3.5)
par(mfcol=c(1,3))
par(mar = c(5.1, 4.1, 4.1, 1.1))
#par(mar=c(5,4,1,1))
plot(1:9,early[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='Proportion of pauses')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
x = apply(early,2,t.test)
y = vector()
for (i in 1:9) { y = c(y,x[[i]]$conf.int[1]) }
z = vector()
for (i in 1:9) { z = c(z,x[[i]]$conf.int[2]) }
polygon(c(1:9,9:1),c(y,rev(z)),border=NA,col='lightgrey',density=NA)
lines(1:9,apply(early,2,mean),lwd=3)
axis(1,at=c(1,9),labels=c('1st','9th'))
axis(2,las=1)
text(9,.38,'Plays written before 1593\n(7 plays)',cex=.9,font=2,adj=c(1,1))

plot(1:9,middle[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
x = apply(middle,2,t.test)
y = vector()
for (i in 1:9) { y = c(y,x[[i]]$conf.int[1]) }
z = vector()
for (i in 1:9) { z = c(z,x[[i]]$conf.int[2]) }
polygon(c(1:9,9:1),c(y,rev(z)),border=NA,col='lightgrey',density=NA)
lines(1:9,apply(middle,2,mean),lwd=3)
axis(1,at=c(1,9),labels=c('1st','9th'))
axis(2,las=1)
text(9,.38,'Plays written between 1598 and 1601\n(6 plays)',cex=.9,font=2,adj=c(1,1))


plot(1:9,late[1,],ylim=c(0,.4),type='n',axes=F,xlab='',ylab='')
abline(h=c(0,.2,.4),lty=2,col='lightgrey')
x = apply(late,2,t.test)
y = vector()
for (i in 1:9) { y = c(y,x[[i]]$conf.int[1]) }
z = vector()
for (i in 1:9) { z = c(z,x[[i]]$conf.int[2]) }
polygon(c(1:9,9:1),c(y,rev(z)),border=NA,col='lightgrey',density=NA)
lines(1:9,apply(late,2,mean),lwd=3)
axis(1,at=c(1,9),labels=c('1st','9th'))
axis(2,las=1)
text(9,.38,'Plays written after 1607\n(6 plays)',cex=.9,font=2,adj=c(1,1))
dev.off()





pca_results = prcomp(props)
plays_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
play_indicies = charmatch(plays_of_interest,data$Title)

# Axis 2 seems to be describing evenness of the distribution, high PC2 = very even, e.g. SPT, low PC2 = very uneven, e.g. ARD
# plot(props[6,],type='l') #ARD
# lines(props[26,],col='grey') #SPT

ca_results = ca(data[,3:11])

pdf('~/Documents/shakespeare-chronordination/figure2.pdf',width=12,height=6)
par(mfcol=c(1,2))

plot(pca_results$x,xlim=c(-.26,.26),ylim=c(-.24,.24),asp=1,cex=1.2,las=1,xlab='Principal Component Axis 1',ylab='Principal Component Axis 2',axes=F)
#text(pca_results$x[,1],pca_results$x[,2],row.names(data))
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
text(c(-.15,-.1,0,.15,.15),c(-.1,.12,-.17,-.12,.1),plays_of_interest,cex=.7,font=3)
segments(c(-.15,-.1,0,.15,.15),c(-.1,.12,-.17,-.12,.1)*.9,pca_results$x[play_indicies,1],pca_results$x[play_indicies,2])
points(pca_results$x[play_indicies,1],pca_results$x[play_indicies,2],pch=21,bg='grey',cex=1.2)
axis(1,at=c(-.2,0,.2))
axis(2,las=1,at=c(-.2,0,.2))

plot(ca_results$rowcoord[,1],ca_results$rowcoord[,2],xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',cex=sqrt(ca_results$rowmass)*10,las=1,axes=F,xlim=c(-1.7,1.7),ylim=c(-5.5,5.5))
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
text(c(-1,-.75,-.25,1,1),c(-1,2.25,-3,-2,2),plays_of_interest,cex=.7,font=3)
segments(c(-1,-.75,-.25,1,1),c(-1,2.25,-3,-2,2)*.9,ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2])
points(ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2],pch=21,bg='grey',cex=sqrt(ca_results$rowmass[play_indicies])*10)
axis(1,at=c(-1.5,0,1.5))
axis(2,las=1,at=c(-4,0,4))
dev.off()



pdf('~/Documents/shakespeare-chronordination/figure3.pdf',width=12,height=6)
par(mfcol=c(1,2))
plot(pca_results$rotation,xlim=c(-.5,.5),ylim=c(-.64,.64),asp=1,type='n',axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
arrows(rep(0,9),rep(0,9),pca_results$rotation[,1],pca_results$rotation[,2],length=.2,lwd=3)
text(pca_results$rotation[,1]+c(.1,.15,.1,.15,-.1,-.1,.15,.11,.1),pca_results$rotation[,2],names(pca_results$rotation[,1]))
axis(1,at=c(-.5,0,.5))
axis(2,las=1,at=c(-.5,0,.5))

plot(ca_results$colcoord[,1],ca_results$colcoord[,2],xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,axes=F,xlim=c(-3,3),ylim=c(-5.5,5.5),type='n')
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
#text(c(-1,-.75,-.25,1,1),c(-1,2.25,-3,-2,2),plays_of_interest,cex=.7)
#segments(c(-1,-.75,-.25,1,1),c(-1,2.25,-3,-2,2)*.9,ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2])
arrows(rep(0,9),rep(0,9),ca_results$colcoord[,1],ca_results$colcoord[,2],length=.2,lwd=3)
text(ca_results$colcoord[,1]+c(-.2,-.3,-.2,-.25,0,.2,.5,.2,0),ca_results$colcoord[,2]+c(.2,0,-.25,-.25,-.75,-.5,0,-.5,1),ca_results$colnames)
ca_results$colnames
axis(1,at=c(-3,0,3))
axis(2,las=1,at=c(-4,0,4))
dev.off()



source('~/Documents/shakespeare-chronordination/ca_boot.R')
out = ca_boot_function(data[,3:11])

#pdf('~/Documents/shakespeare-chronordination/figure4.pdf',width=6,height=6)
par(mfcol=c(1,1))
xlimit=.5
ylimit=.7
plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in 1:42) {polygon(out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
centers = t(t(out[[3]]) %*% t(as.matrix(data[,3:11])))/apply(data[,3:11],1,sum)
text(centers[play_indicies,c(1,2)],labels=data$Title[play_indicies],cex=.8,col='black',font=3)
for (i in play_indicies) {polygon(out[4][[1]][[i]],col=alpha('lightgrey',0),lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,las=1,at=c(-.5,0,.5))
#dev.off()






multi = read.csv('~/Documents/shakespeare-chronordination/multiauthor counts for CONVEX HULLS.csv')
head(multi)
multi_out = ca_boot_function(multi[,3:11])

sh_ind = which(multi$Author=='Shakespeare')

multi_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
multi_indicies = charmatch(multi_of_interest,multi$Title)

pdf('~/Documents/shakespeare-chronordination/figure4.pdf',width=6,height=6)
par(mfcol=c(1,1))
xlimit=.5
ylimit=.7
plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
centers = t(t(multi_out[[3]]) %*% t(as.matrix(multi[,3:11])))/apply(multi[,3:11],1,sum)
text(centers[multi_indicies,c(1,2)],labels=multi$Title[multi_indicies],cex=.8,col='black',font=3)
for (i in multi_indicies) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',0),lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,las=1,at=c(-.5,0,.5))
dev.off()


fl = which(multi$Author=='Fletcher')
jo = which(multi$Author=='Jonson')
ky = which(multi$Author=='Kyd')
ml = which(multi$Author=='Marlowe')
mr = which(multi$Author=='Marston')
pe = which(multi$Author=='Peele')


pdf('~/Documents/shakespeare-chronordination/figure5.pdf',width=8,height=6)
par(mfrow=c(2,3))

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in fl) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Fletcher')

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in jo) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Jonson')

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in ky) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Kyd')

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in ml) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Marlowe')

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in mr) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Marston')

plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',las=1,asp=1,axes=F)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
for (i in sh_ind) {polygon(multi_out[4][[1]][[i]],col=alpha('lightgrey',.2),border=NA)}
for (i in pe) {polygon(multi_out[4][[1]][[i]],border='black',lty=3)}
axis(1,at=c(-.5,0,.5))
axis(2,at=c(-.5,0,.5),las=1)
title(main='Peele')
dev.off()



matlab = read.csv('~/Documents/shakespeare-chronordination/MATLAB/CCA_rawpredictions.csv',header=F)
matlab
matlab$Title = sort(data$Title)
anchors = c('3 Henry 6','Henry 5','Pericles')
matlab_indicies = charmatch(anchors,matlab$Title)
x_vals = matlab$V1[matlab_indicies]

ca_indicies = charmatch(anchors,data$Title)
y_vals = ca_results$rowcoord[ca_indicies,1]

mod = lm(y_vals~x_vals)

pdf('~/Documents/shakespeare-chronordination/figure6.pdf',width=8,height=6)
plot(x_vals,y_vals,axes=F,xlab='Year of composition',ylab='Correspondence Axis 1 Score',xlim=c(1589,1614),pch=19,ylim=c(-1.5,1.5))
axis(1)
axis(2,las=1)
abline(mod,lwd=2,lty=2)
text(x_vals+1,y_vals-.1,anchors,cex=.9,font=3,adj=c(0,0))
segments(rep(0,3),y_vals,x_vals,y_vals,lty=3)
segments(x_vals,y_vals,x_vals,rep(-2,3),lty=3)

segments(0,ca_results$rowcoord[data$Title=='Macbeth',1],matlab$V1[matlab$Title=='Macbeth'],mod$coefficients[1]+mod$coefficients[2]*matlab$V1[matlab$Title=='Macbeth'],lwd=2)
arrows(matlab$V1[matlab$Title=='Macbeth'],mod$coefficients[1]+mod$coefficients[2]*matlab$V1[matlab$Title=='Macbeth'],matlab$V1[matlab$Title=='Macbeth'],-1.6,length=.1,lwd=2)
text(1606,-1.5,'Macbeth\n  \n  ',cex=.9,adj=c(0,0),font=4)
text(1606,-1.5,'  \nCA score = 0.7\nPredicted year = 1604.9',cex=.9,adj=c(0,0))
dev.off()





