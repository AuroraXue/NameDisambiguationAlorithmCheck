ini<-function(M_p)
{
tag<-rep(0,n)  #n=244
class<-1
index<- M_p[1,] >= 1 # length 244 T OR F
tag[index]<-class
class <- class + 1

for (row in 2:n)
{
  if (tag[row]!=0) {next}
  index<- M_p[row,] >= 1
  tag[index]<-class
  class <- class + 1
}



## adjust the number of clusters according to the actual researcher number k 

lambda <- length( unique(tag) )
#filename <- "AKumar"

## output : tag as a vector of length n

## need update y_h & tag in this step

## now y_h in a matrix in the essay:(dim) lambda * 666(ncol of x(dtm)) 
## need update as a matrix : k * 666(ncol of x(dtm)) 

## tag is a vector with lambda level
## need update to k level

ori.order<-seq(1,n)
if (lambda >= k){   ## need substract lambda - k
  
  while (lambda > k)
  {
    y_h<-matrix(NA,lambda,ncol(dtm))  ## y_h in a matrix in the essay:(dim) the number of clusters * 666(ncol of x(dtm))
    j=1
    
    for (k0 in unique(tag))   ## re-adjust for k clusters
    {
      index<-which(tag == k0)
      if (length(index)==1) {y_h[j,]<-dtm[index,]}
      else{y_h[j,]<-apply(dtm[index,],2,mean)}
      j<-j+1
    }
    
    dist<-matrix(NA,nrow(y_h),nrow(y_h))  ## 130*130 matrix : distance of each cluster
    for (i in 1:nrow(y_h))
    {
      for (j in i:nrow(y_h))
      {
        if (i==j) {dist[i,j]<-100}else
        {dist[i,j]<-dist(rbind(y_h[i,],y_h[j,]), method= "euclidian")}
      }
    }
    
    lambda <- length( unique(tag) )
    
    
    #### method 1 ####
    
    i<-1
    while (lambda > k)
    {
      tag_merge<-which.min(dist[i,])  ## choose the tag to merge with the ith tag
      index<- tag == unique(tag)[tag_merge]
      tag[index]<-i
      i=i+1
      lambda <- length( unique(tag) )
      if (i>lambda){break}
    }
    
    #### method 2 ####
    
    #   tag_merge<-which(dist == min(dist,na.rm = T), arr.ind = TRUE)[2]
    #   index<- tag == unique(tag)[tag_merge]
    #   tag[index]<-which(dist == min(dist,na.rm = T), arr.ind = TRUE)[1]
    #   lambda <- length( unique(tag) )
    
    ori.order<-ori.order[order(tag)]
    tag<-tag[order(tag)]
    M_p<-M_p[order(tag),]
    dtm<-dtm[order(tag),]
  }
  
  
}else
{  ## need add k - lambda
  index_du<-duplicated(tag)
  for (i in 1:k-lambda)
  {
    tag[index_du][i]<-max(tag)+i
  }

}


### simple check:should be 14
length(unique(tag))

return(tag)

}


em_test<-function(tag1)
{
  source('../lib/evaluation_measures.R')
  #truth<-AKumar$AuthorID
  #truth<-AKumar$AuthorID[1:10]
  matching_matrix_p6 <- matching_matrix(truth,tag1)
  performance_p6 <- performance_statistics(matching_matrix_p6)
  return(performance_p6)
}

#em_train<-function(M_p,tag)
#{
 
#}

