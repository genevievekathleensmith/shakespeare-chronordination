
matlab = read.csv('~/Documents/shakespeare-chronordination/MATLAB/CCA_rawpredictions.csv',header=F)
matlab

boot = read.csv('~/Documents/shakespeare-chronordination/MATLAB/CCA_bootpredictions.csv',header=F)
dim(boot)
hist(boot[,1])
plot(boot[,1],type='n')

for (i in 1:10) {
  points(boot[,i])
}

bootmeans = apply(boot,1,mean)
plot(matlab$V1,bootmeans)
plot(bootmeans,apply(boot,1,median))
plot(matlab$V1,apply(boot,1,median))
dim(matlab)
length(bootmeans)

plot(density(as.numeric(boot[1,])),xlim=c(1589,1614),ylim=c(0,1),type='n')
for (i in 1:length(bootmeans)) {
  lines(density(as.numeric(boot[i,]),bw=.25))
}






