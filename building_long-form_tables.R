
# For the up to date counts, including Arden of Faversham:
#counts = read.csv('~/Documents/shakespeare-chronordination/SH DATA all counts A minus C plus ARD.csv')
#verse = read.csv('~/Documents/shakespeare-chronordination/verse_line_counts plus ARD.csv')

# For the revised version of 1H6:
#counts = read.csv('~/Documents/shakespeare-chronordination/SH DATA red 1H6.csv')
#verse = read.csv('~/Documents/shakespeare-chronordination/verse_line_counts red 1H6.csv')

# For the revised version of Ed3:
#counts = read.csv('~/Documents/shakespeare-chronordination/SH DATA exp Ed3.csv')
#verse = read.csv('~/Documents/shakespeare-chronordination/verse_line_counts exp Ed3.csv')

# For both revisions, i.e. FULLY UP TO DATE FINAL COUNTS:
counts = read.csv('~/Documents/shakespeare-chronordination/SH DATA.csv')
verse = read.csv('~/Documents/shakespeare-chronordination/verse_line_counts.csv')




merged = merge(counts,verse)

data = merged[,c(11,2:10)]

first = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$first)))
first$pause = rep('first',length(first$abbrv))

second = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$second)))
second$pause = rep('second',length(second$abbrv))

third = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$third)))
third$pause = rep('third',length(third$abbrv))

fourth = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$fourth)))
fourth$pause = rep('fourth',length(fourth$abbrv))

fifth = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$fifth)))
fifth$pause = rep('fifth',length(fifth$abbrv))

sixth = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$sixth)))
sixth$pause = rep('sixth',length(sixth$abbrv))

seventh = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$seventh)))
seventh$pause = rep('seventh',length(seventh$abbrv))

eighth = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$eighth)))
eighth$pause = rep('eighth',length(eighth$abbrv))

ninth = as.data.frame(lapply(data[,1:2], function(x) rep(x, data$ninth)))
ninth$pause = rep('ninth',length(ninth$abbrv))

fulldata = rbind(first[,c(1,3)],second[,c(1,3)],third[,c(1,3)],fourth[,c(1,3)],fifth[,c(1,3)],sixth[,c(1,3)],seventh[,c(1,3)],eighth[,c(1,3)],ninth[,c(1,3)])

#table(fulldata)

#write.csv(fulldata,'~/Documents/shakespeare-chronordination/fulldata.csv',row.names=F)
#write.csv(table(fulldata)[,c(3,6,9,4,2,8,7,1,5)],'~/Documents/shakespeare-chronordination/tabledata.csv',row.names=F)
#write.csv(levels(fulldata$abbrv),'~/Documents/shakespeare-chronordination/playnames.csv',row.names=F)

#install.packages('R.matlab')
#library(R.matlab)

#plays = as.character(fulldata$abbrv)
#pauses = fulldata$pause
#writeMat('~/Documents/shakespeare-chronordination/MATLAB/plays.mat',plays=plays)
#writeMat('~/Documents/shakespeare-chronordination/MATLAB/pauses.mat',pauses=pauses)

A = as.character(fulldata$abbrv)
P = fulldata$pause
writeMat('~/Documents/shakespeare-chronordination/MATLAB/abbrvs_and_pauses.mat', A=A, P=P)

V = merged[,c(11,13)]
writeMat('~/Documents/shakespeare-chronordination/MATLAB/verse_lines.mat', V=V)

