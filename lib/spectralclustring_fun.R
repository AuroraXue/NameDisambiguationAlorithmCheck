#Spring 2017, Project 4, Group 10
#Spectral Cluster Function for paper 3

myspectralCluster<-function(x,centers){
  ##x--a matrix
  kernel = "rbfdot"
  myiter = 200
  mymod_s =  0.75

  #take na out of x matrix
  x <- na.omit(x)
  row_names <- rownames(x)
  x <- as.matrix(x)
  #number of dataset
  n_row <- nrow(x)
  #number of centers:
  num_center <- centers
  
  #################################

  mysample <- sample(1:n_row, floor(mymod_s*n_row))
  
  sx <- unique(x[mysample,])
  ns <- dim(sx)[1]
  dota <- rowSums(sx*sx)/2
  ktmp <- crossprod(t(sx))
  for (i in 1:ns)
    ktmp[i,]<- 2*(-ktmp[i,] + dota + rep(dota[i], ns))
  
  
  ## fix numerical prob.
  ktmp[ktmp<0] <- 0
  ktmp <- sqrt(ktmp)
  
  kmax <- max(ktmp)
  kmin <- min(ktmp + diag(rep(Inf,dim(ktmp)[1])))
  kmea <- mean(ktmp)
  lsmin <- log2(kmin)
  lsmax <- log2(kmax)
  midmax <- min(c(2*kmea, kmax))
  midmin <- max(c(kmea/2,kmin))
  rtmp <- c(seq(midmin,0.9*kmea,0.05*kmea), seq(kmea,midmax,0.08*kmea))
  if ((lsmax - (Re(log2(midmax))+0.5)) < 0.5){
    step <- (lsmax - (Re(log2(midmax))+0.5))}else
    {step <- 0.5}
  if (((Re(log2(midmin))-0.5)-lsmin) < 0.5 ) stepm <-  ((Re(log2(midmin))-0.5) - lsmin) else stepm <- 0.5
  
  tmpsig <- c(2^(seq(lsmin,(Re(log2(midmin))-0.5), stepm)), rtmp, 2^(seq(Re(log2(midmax))+0.5, lsmax,step)))
  diss <- matrix(rep(Inf,length(tmpsig)*num_center),ncol=num_center)
  
  for (i in 1:length(tmpsig)){
    ka <- exp((-(ktmp^2))/(2*(tmpsig[i]^2)))
    diag(ka) <- 0
    
    d <- 1/sqrt(rowSums(ka))
    
    if(!any(d==Inf) && !any(is.na(d))&& (max(d)[1]-min(d)[1] < 10^4))
    {
      l <- d * ka %*% diag(d)
      xi <- eigen(l,symmetric=TRUE)$vectors[,1:num_center]
      yi <- xi/sqrt(rowSums(xi^2))
      res <- kmeans(yi, centers, myiter)
      diss[i,] <- res$withinss
    }
  }
  
  ms <- which.min(rowSums(diss))
  kernel <- rbfdot((tmpsig[ms]^(-2))/2)
  
  ## Compute Affinity Matrix
  km <- kernelMatrix(kernel, x)
  
  d <- 1/sqrt(rowSums(km))
  l <- d * km %*% diag(d)
  xi <- eigen(l)$vectors[,1:num_center]
  yi <- xi/sqrt(rowSums(xi^2))
  res <- kmeans(yi, centers, myiter)
  
  ##return
  cent <- matrix(unlist(lapply(1:num_center,ll<- function(l){colMeans(x[which(res$cluster==l), ,drop=FALSE])})),ncol=dim(x)[2], byrow=TRUE)
  
  withss <- unlist(lapply(1:num_center,ll<- function(l){sum((x[which(res$cluster==l),, drop=FALSE] - cent[l,])^2)}))
  names(res$cluster) <- row_names
  mylist<-list(myData=res$cluster,clusters = res$size,withinss=withss)
  #myData contains the cluster group of each data point
  #clusters contains the number of points in each clusters
  return(mylist)
  
}


