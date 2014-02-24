
# plotting heatmaps of scene-by-scene pause breakdowns
# data from hand-counts by Doug Bruster
# Feb 2014

filenames = list('allswell','dream','henry6pt2','henry6pt3','julius','merchant','tempest','titus','cornl_kyd','spanish_kyd','smt_midd')

for (i in 1:length(filenames)) {
  dat = read.csv((paste(filenames[i],sep='.','csv')))
  print(dim(dat))
  props = prop.table(as.matrix(na.omit(dat)),1)
  pdf(paste(filenames[i],sep='.','pdf'),height=4)
  par(mar=c(0,0,0,0))
  heatmap(t(props),Rowv=NA,Colv=NA,asp=1,col=colorRampPalette(colors=c('white','black'))(100),las=1,main=filenames[i],mar=c(10,2))
  dev.off()
}


