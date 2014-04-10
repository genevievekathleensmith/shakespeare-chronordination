

ca_boot_function = function(mydata) {
  data.ca <- ca(mydata)
  #  mydata = data[,2:10]
  #  data.ca <- ca(data[,2:10])
  
  
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
  
  #data.col <- t(t(data.rowsc) %*% mydata) / apply(mydata,2,sum) # set column label positions
  #data.row <- t(t(data.colsc) %*% t(mydata)) / apply(mydata,1,sum) # set column label positions
  #print(data.col)
  return(list(data.ca,data.rowsc,data.colsc,poly.row))
}
