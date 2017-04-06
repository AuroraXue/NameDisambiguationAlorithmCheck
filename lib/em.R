### use input dtm_train_tfidf from main2 244 * 666
### just for AKumar.txt
### need input: M as a matrix of 244 * 244 ( use tao = 1 - coauthor constrain )
### 需要把filename定义

### 4.3 EM framework equation 1

#########
### 0 ### data & package preparation
#########

dtm<-as.matrix(dtm_train_tfidf)
n<-nrow(dtm)  ## number of papers 244
m<-ncol(dtm)  ## 666
A<-diag(rep(1,m))  ## 666 * 666

library("nnet")


#########
## 1.1 ## initialize parameters
#########

eta <- 0.01 ## need tune later

#########
## 1.2 ## initialize cluster
#########


#for test
M_p<-matrix(c(1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,1,1,0,0,1,1,1,1),ncol=6,nrow=6)
n<-6

M<-M %*% M
M_p<-M[1:n,1:n]

tag<-rep(0,n)  ## initialize tag for 244 papers
class<-1
index<- M_p[1,] == 1
tag[index]<-class
class <- class + 1

for (row in 2:n)
{
  if (tag[row]!=0) {next}
  index<- M_p[row,] == 1
  tag[index]<-class
  class <- class + 1
}

## adjust the number of clusters according to the actual researcher number k 

lambda <- length( unique(tag) )
#filename <- "AKumar"
k <- length(unique(AKumar$AuthorID))  
## output : tag as a vector of length n


## 待观察 k 和 lambda的关系 再决定怎么办


#########
### 2 ### E steps
######### update the tag of each paper

## 2.1 ## count D_xx : a matrix of distance of paper x_i and x_j

D_xx<-matrix(ncol=n,nrow=n)  ## 244 * 244
c<-M_p

for (i in 1:n){
  for (j in 1:n){
    D_xx[i,j]<- 1 - ( t(dtm[i,]) %*% A %*% dtm[j,] )/( sqrt(t(dtm[i,]) %*% A %*% dtm[i,]) * sqrt(t(dtm[j,]) %*% A %*% dtm[j,]) )  
  }}

## 2.2 ## count D_xy : a matrix of distance of paper x_i and researcher y_i(x_i's tag)

### D_xy is defined as average* distance of paper x_i and the papers written by paper x_i's "tag",
### that is, the average* distance of papers with the same current tag of x_i.
### average* can also be minimum & maximum

### keep summation version

#distance.xy <-function(tag.vec=tag,num=n)  ## tag.vec : 244 * 1 ; num : 244
#{
#  D_xy<-rep(NA,num)   ### 244 * 1
#  
#  for (i in 1:num){
#    index<-which(tag.vec == tag.vec[i])
#    for(j in index)
#    { d<-0
#    d<-d+D_xx[i,j]}
#    D_xy[i]<-d/length(index)
#  }
#  
#  D_xy
#}

### without summation version
distance.xy <-function(tag.vec=tag,paper_index)  ## tag.vec : 244 * 1
{
    index<-which(tag.vec == tag.vec[paper_index])
    for(j in index)
    { D_xy<-0
    D_xy<-D_xy+D_xx[paper_index,j]}
    return(D_xy/length(index))
}


## 2.3 ## assign tag for x to minimize f

### get f for x_i tagged y_h

f<-function(tagvec,paperindex)
{
  indicate<-matrix(0,length(tagvec),length(tagvec))
  for (i in 1:length(tagvec))
  {
    for (j in 1:length(tagvec))
    {
      indicate[i,j]<-ifelse(tagvec[i]==tagvec[j],1,0)
    }
  }
  f<-distance.xy(paper_index=paperindex,tag.vec=tagvec) + sum(D_xx* indicate * c) 
  return(f)
}

uni.tag<-unique(tag)

for (i in 1:n)  ## adjust for x_i's tag
{
  ## input : initialized cluster(tag)
  
  f.xi<-rep(NA,length(uni.tag))  ## f for trying x_i's all tags.
  j=1
  for (k in uni.tag)   ## substitute x_i's tag by tag "k"
  {
    newtag <- tag
    newtag[i]<-k
    f.xi[j]<-f(tagvec=newtag,paperindex=i)
    j<-j+1
  }
  tag[i]<-uni.tag[which.min(f.xi)]
}



#########
### 3 ### M steps
######### update A matrix

## 3.1 ## researcher representative

### 待写

uni.tag<-unique(tag)




for (k in uni.tag)   ## re-adjust for k clusters
{
  index<-which(tag == k)
  j=1
  newtag <- tag
  newtag[i]<-k
  f.xi[j]<-f(tagvec=newtag,paperindex=i)
  j<-j+1
}

for (i in 1:n)  
{
  
}

## 3.2 ## update a_mm

D_xx_deriv<-matrix(ncol=n,nrow=n)  ## 244 * 244

  
for (i in 1:n){
    for (j in 1:n){
      part<- (dtm[i,m]^2*(t(dtm[i,]) %*% A %*% dtm[i,])+dtm[j,m]^2*(t(dtm[j,]) %*% A %*% dtm[j,]))/2*( sqrt(t(dtm[i,]) %*% A %*% dtm[i,]) * sqrt(t(dtm[j,]) %*% A %*% dtm[j,]) )
      D_xx_deriv[i,j]<- dtm[i,m]*dtm[j,m]*( sqrt(t(dtm[i,]) %*% A %*% dtm[i,]) * sqrt(t(dtm[j,]) %*% A %*% dtm[j,]) )
      -t(dtm[i,]) %*% A %*% dtm[j,] * part
      D_xx_deriv[i,j]<-D_xx_deriv[i,j]/((t(dtm[i,]) %*% A %*% dtm[i,])*(t(dtm[j,]) %*% A %*% dtm[j,]))
}}

M<-nrow(A)
for (m in 1:M)
{
  deriv.f<-D_xx_deriv[m,m]
  A[m,m] <- A[m,m] + eta * deriv.f
}



