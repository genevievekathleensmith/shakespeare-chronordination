#install.packages('scales')
#install.packages('ca')

data = read.csv('~/Documents/shakespeare-chronordination/SH DATA all counts A minus C Ed Bruster 11-5 Smith 11-15.csv')
library(scales)
library(ca)

head(data)
colorRampPalette(colors=c('blue','green'))(3)
heatmap(cor(t(data[,c(2:10)])),Rowv=NA,labRow=data$Title,labCol=NA,col=colorRampPalette(colors=c('red','royalblue'))(100),mar=c(1,4),asp=1,symm=TRUE)
heatmap(cor(t(data[,c(2:10)])),labRow=data$Title,labCol=NA,col=colorRampPalette(colors=c('red','royalblue'))(100),asp=1,symm=TRUE,mar=c(1,1))

pca_results = prcomp(data[,c(2:10)]/apply(data[,c(2:10)],1,sum))
biplot(pca_results)

plot(pca_results$x,xlim=c(-.2,.2),las=1,cex=1.5)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')

plays_of_interest = c('Romeo and Juliet','Richard 3','Hamlet','Macbeth','Tempest')
play_indicies = charmatch(plays_of_interest,data$Title)

text(c(-.1,-.1,0,.1,.1),c(.1,-.1,.1,.1,-.1),plays_of_interest,cex=.7)
segments(c(-.1,-.1,0,.1,.1),c(.1,-.1,.1,.1,-.1),pca_results$x[play_indicies,1],pca_results$x[play_indicies,2])
points(pca_results$x[play_indicies,1],pca_results$x[play_indicies,2],pch=21,bg='grey',cex=1.5)

summary(pca_results)


### STILL NEED TO CHECK THIS, CORRECT NAMES, ETC...
ca_results = ca(data[,c(2:10)])

plot(ca_results$rowcoord[,1],ca_results$rowcoord[,2],xlab='Correspondence Axis 1',ylab='Correspondence Axis 2',cex=ca_results$rowmass*50,las=1)
abline(h=0,lty=3,col='grey')
abline(v=0,lty=3,col='grey')
text(c(-1,-.5,0,1,1),c(2,-2,3,2,-2),plays_of_interest,cex=.7)
segments(c(-1,-.5,0,1,1),c(2,-2,3,2,-2)*.9,ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2])
points(ca_results$rowcoord[play_indicies,1],ca_results$rowcoord[play_indicies,2],pch=21,bg='grey',cex=ca_results$rowmass[play_indicies]*50)

dates = read.csv('~/Documents/shakespeare-chronordination/alldates.csv')


all(levels(dates$abbrv) == levels(data$abbrv))
names(data)
merged = merge(data,dates,by.x='abbrv',by.y='abbrv')

x3 = data.frame(prop.table(as.matrix(merged[,3:11]),1))

library(segmented)

par(mfrow=c(3,3))
par(mar=c(1,1,1,1))
for (i in 1:9) {
  plot(0,0,type='n',xlim=c(1590,1615),ylim=c(0,.4),las=1,axes=F)
  points(merged$oxford,x3[,i],pch=19,cex=.3)
  lines(lowess(x=merged$oxford,y=x3[,i],f=.3))
  axis(1)
  axis(2,las=1)
}


plot(0,0,type='n',xlim=c(1590,1615),ylim=c(0,1),las=1,axes=F)
points(merged$oxford,x3[,1]+x3[,2]+x3[,3]+x3[,4]+x3[,5],pch=19,cex=.3)
lines(lowess(x=merged$oxford,y=x3[,1]+x3[,2]+x3[,3]+x3[,4]+x3[,5],f=.3))
axis(1)
axis(2,las=1)
points(merged$oxford,x3[,6]+x3[,7]+x3[,8]+x3[,9],pch=19,cex=.3)
lines(lowess(x=merged$oxford,y=x3[,6]+x3[,7]+x3[,8]+x3[,9],f=.3))

early = x3[,1]+x3[,2]+x3[,3]+x3[,4]
late = x3[,5]+x3[,6]+x3[,7]+x3[,8]+x3[,9]


x <- merged$oxford
y <- early

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=1605)

plot(y~x)
abline(lin.mod)
plot(segmented.mod,add=T)
summary(segmented.mod)
#lines(lowess(x=merged$oxford,y=x3$second),col='blue')


# so, the Correspondence Analysis now includes the information we have about play length, reflecting shakespeare's contributions.
# what about error?

# have we sampled the plays fully? how would our results change if we were to re-sample them?

# let's do a bootstrap analysis!


# BEFORE I GO ANY FURTHER, I SHOULD POINT OUT THAT I AM NOT RESPONSIBLE FOR THE FOLLOWING CODE (except the modifications that I made)

#R code for conducting analyses
#The analyses presented in this paper were conducted using R (version 2.13). This appendix provides the R code for the correspondence analysis bootstrap resampling procedure, site selection and removal procedure, and for the production of several figures. This code is based in part on code provided by Greenacre (2007: Appendix B), but which was expanded and modified for the purposes of this analysis. The total system time required for running this script is typically less than 2 minutes (including the production of all figures) and could be reduced by running the R compiler. Computation time scales roughly linearly with the size of the matrix. Additional instructions and guidance for running this script are provided at the link below:
#http://www.mattpeeples.net/caboot.html


row.names(mydata) = temp$Title[temp$Author=='Shakespeare']

ca_boot_function = function(mydata) {
  data.ca <- ca(mydata)
  mydata = data[,2:10]
  data.ca <- ca(data[,2:10])
  

# define variables
rown <- nrow(mydata) # number of rows in data table
coln <- ncol(mydata) # number of columns in data table
nsim <- 1000 # number of bootstrapped simulations to run
cut <- 0.90 # set convex hull confidence interval for final plot
lab <- colnames(mydata) # define column (type) labels

# create nsim bootstrapped replicates from original data
data.rowsum <- apply(mydata,1,sum)

# create bootstrapped replicates then reorder by original row order
data.sim <- rmultinom(nsim,data.rowsum[1],prob=mydata[1,]) 
for (i in 2:rown) {
  data.sim <- cbind(data.sim,rmultinom(nsim,data.rowsum[i],prob=mydata[i,]))} 
data.sim <- t(data.sim)
data.sim2 <- matrix(rep(0,nsim*rown*coln),nrow=nsim*rown)
for (k in 1:nsim) {
  for (i in 1:rown) {
    data.sim2[(k-1)*rown+i,] <- data.sim[k+(i-1)*nsim,]}}

# define simulated column coordinates using row transition formula
data.rowsc <- data.ca$rowcoord[,1:2]

data.colsim <- t(t(data.rowsc) %*% data.sim2[1:rown,]) / apply(data.sim2[1:rown,],2,sum)


for (k in 2:nsim) {
  data.colsim <- rbind(data.colsim, t(t(data.rowsc) %*% data.sim2[((k-1)*rown+1):(k*rown),]) / apply(data.sim2[((k-1)*rown+1):(k*rown),],2,sum))}
data.colsim2 <- matrix(rep(0,nsim*coln*2),nrow=nsim*coln)

# define simulated row coordinates using column transition formula
data.colsc <- data.ca$colcoord[,1:2]
data.rowsim <- t(t(data.colsc) %*% t(data.sim2[1:rown,])) / apply(data.sim2[1:rown,],1,sum)
for (k in 2:nsim) {
  x <- t(t(data.colsc) %*% t(data.sim2[((k-1)*rown+1):(k*rown),]))
  data.rowsim <- rbind(data.rowsim, x / apply(data.sim2[((k-1)*rown+1):(k*rown),],1,sum))}
data.rowsim2 <- matrix(rep(0,nsim*rown*2),nrow=nsim*rown)

# populate row and column simulated point matrices with projected coordinates
for (j in 1:coln) {
  for (k in 1:nsim) {
    data.colsim2[(j-1)*nsim+k,] <- data.colsim[j+(k-1)*coln,]}}
for (j in 1:rown) {
  for (k in 1:nsim) {
    data.rowsim2[(j-1)*nsim+k,] <- data.rowsim[j+(k-1)*rown,]}}


# set up matrices for recording convex hull sizes
x2 <- matrix(0,nrow(mydata),1)
x3 <- matrix(0,nrow(mydata),1)
x2_a <- matrix(0,nrow(x2),1)
x3_a <- matrix(0,nrow(x3),1)
row.names(x2) <- row.names(mydata)
row.names(x3) <- row.names(mydata)
row.names(x2_a) <- row.names(x2)
row.names(x3_a) <- row.names(x3)

# Define convex hulls around row points and record hull sizes
poly.row <- list()
for (j in 1:nrow(mydata)) {
  points <- data.rowsim2[(nsim*(j-1)+1):(nsim*j),]
  hpts <- chull(points)
  hpts <- c(hpts,hpts[1])
  v <- points[hpts,]
  poly.row[[j]] <- v
  v <- rbind(unique(v),v[1,])
  v2 <- as.matrix(dist(v))
  x <- matrix(0,nrow(v2)-1,1)
  for (i in 1:nrow(x)) {
    x[i,1] <- v2[i+1,i]}
  x2[j,1] <- sum(x)
  x3[j,1] <- max(v2)}

# Define convex hulls around column points
poly.col <- list()
for (j in 1:coln) {
  points <- data.colsim2[(nsim*(j-1)+1):(nsim*j),]
  hpts <- chull(points)
  hpts <- c(hpts,hpts[1])
  poly.col[[j]] <- points[hpts,]}


return(list(data.ca,data.rowsc,data.colsc,poly.row))
}


out = ca_boot_function(data[,2:10])

names(out)
class(out)
out[1]
out[2]
out[3][[1]]

out[4]
# plot all bootstrapped data

plot(out[2][[1]],xlim=c(-1.5,3))
arrows(0,0,out[3][[1]][,1],out[3][[1]][,2])
text(out[3][[1]],labels=1:9)

ylimit = .75
xlimit = .55
plot(x=0,y=0,type='n',xlim=c(-xlimit,xlimit),ylim=c(-ylimit,ylimit),asp=1)
for (i in 1:41) {polygon(out[4][[1]][[i]],col=alpha('green',.4))}
?polygon

#points(ca_results$rowcoord[,1],ca_results$rowcoord[,2],col='green')


# plot original CA rows and columns
par(mfrow=c(1,2))
plot(data.ca) 

points()
plot(data.ca$rowcoord[,1],data.ca$rowcoord[,2],pch=19)
,col=alpha(temp$Author,.3)
abline(h=0,lty=3)
abline(v=0,lty=3)

palette(c('black','red','cyan','purple','orange','black','black','green','blue','black','black'))
plot(data.rowsc[,1],data.rowsc[,2],type='p',xlab='CA1',ylab='CA2',main='All Simulated Data',xlim=c(-.7,.7),ylim=c(-.6,.6))
data.col <- t(t(data.rowsc) %*% mydata) / apply(mydata,2,sum) # set column label positions
data.row <- t(t(data.colsc) %*% t(mydata)) / apply(mydata,1,sum) # set column label positions

# use last two digits of color code to set transparency
for (i in 1:length(poly.row)) { 
  polygon(poly.row[[i]],col=alpha('blue',.05),border=alpha('blue',.4))}


text(data.row[,1],data.row[,2],temp$Title[temp$Author=='Shakespeare'],font=2,cex=.3,col='black',adj=c(0.5,2.5))





# NOW LET US COMPARE THE ANALYSES : IF WE HAD TO GUESS AT THE DATES FOR EACH PLAY, USING ONLY INTERNAL EVIDENCE, WHAT WOULD OUR CHRONOLOGY LOOK LIKE?











