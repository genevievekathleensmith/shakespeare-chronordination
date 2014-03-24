

dir()
dates = read.csv('alldates.csv')

head(dates)
levels(dates$abbrv)==levels(cca$Title)

levels(dates$abbrv)[which(levels(dates$abbrv)=='AC')] = 'ANT'
levels(dates$abbrv)[which(levels(dates$abbrv)=='ALL')] = 'AWW'
levels(dates$abbrv)[which(levels(dates$abbrv)=='KJ')] = 'JN'
levels(dates$abbrv)[which(levels(dates$abbrv)=='LEA')] = 'LR'
levels(dates$abbrv)[which(levels(dates$abbrv)=='RJ')] = 'ROM'
levels(dates$abbrv)[which(levels(dates$abbrv)=='TEM')] = 'TMP'
levels(dates$abbrv)[which(levels(dates$abbrv)=='TC')] = 'TRO'
levels(dates$abbrv)[which(levels(dates$abbrv)=='MW')] = 'WIV'
levels(dates$abbrv)[which(levels(dates$abbrv)=='WIN')] = 'WT'

write.csv(dates,'dates.csv')

dates = read.csv('dates.csv')
boot = read.csv('cca_bootstrap_confidence_intervals_march2014.csv')
head(boot)

names(boot)[1]='abbrv'
class(dates)
class(dates$abbrv)
class(boot$Title)
merged = merge(x=boot,y=dates,by.x=boot$Title,by.y=dates$abbrv)

levels(boot$Title)==levels(dates$abbrv)
