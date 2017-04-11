library(nnet)

#### used during initialization to count distance of two clusters

dist.cluster<-function(tag.mat,index)
{
  dist<-matrix(NA,nrow(tag.mat),nrow(tag.mat))  ## 130*130 matrix : distance of each cluster
  for (i in 1:nrow(tag.mat))
  {
    for (j in i:nrow(tag.mat))
    {
      if (i==j) {dist[i,j]<-100}else
      {dist[i,j]<-dist(rbind(tag.mat[i,],tag.mat[j,]), method= "euclidian")}
    }
  }
  return(dist[index,])
}


#### count D_xy : a matrix of distance of paper x_i and researcher y_i(x_i's tag)

distance.xy <-function(tag.vec=tag,paper_index)  ## tag.vec : 244 * 1
{
  index<-which(tag.vec == tag.vec[paper_index])
  D_xy<-0
  for(j in index)
  { 
  D_xy<-D_xy+D_xx[paper_index,j]}
  return(D_xy/(length(index))) #lead to trouble
  #return(D_xy)
}

#### objective function as f for x_i tagged y_h


### ver.1 (f)

#f<-function(tagvec,paperindex,n0,weight=1)
#{

### ver.1(for get indicate)
# indicate<- 1- class.ind(tagvec) %*% t(class.ind(tagvec))

### ver.2(for get indicate)
# indicate<-matrix(NA,length(tagvec),length(tagvec))
  
#  for (i in 1:length(tagvec))
#  {
#    for (j in 1:length(tagvec))
#    {
#      indicate[i,j]<-ifelse(tagvec[i]!=tagvec[j],1,0)
#    }
#  }

# row<-matrix(rep(tagvec),n,n,byrow=T)
# col<-matrix(rep(tagvec),n,n)
# indicate <- row != col
  
#f<-distance.xy(paper_index=paperindex,tag.vec=tagvec) + weight*sum(D_xx * indicate * c) 
#  return(f)
#}


### ver.2 (f)

f<-function(tagvec,paperindex,n0,weight=1)
{
  indicate<-ifelse(tagvec!=tagvec[paperindex],1,0)
  f<-distance.xy(paper_index=paperindex,tag.vec=tagvec) + weight*sum(D_xx[paperindex,] * indicate * c[paperindex,]) 
  return(f)
}

####build tao-coauthor constrain matric


#unique_Coauthors_list
#Coauthors_list

M_prod<-function(index,tao){
  source("../lib/Extract Coauthors.R")
  list<-Coauthors_list[[index]]
  coauthor<-unique_Coauthors_list[[index]]
  n<-length(list)
  len_coa<-length(coauthor)
  Mp<-diag(x=1,nrow = n, ncol = n)
  
  Mpa<-matrix(NA, n, len_coa)
  
  for(i in 1:len_coa){
    for(j in 1:n){
      Mpa[j,i]<-grepl(coauthor[i],list[j])
    }
  }
  
  Mpa<-ifelse(Mpa==TRUE,1,0)
  
  Ma<-diag(x=1,len_coa,len_coa)
  
  for(i in 1:len_coa){
    for(j in 1:len_coa){
      Ma[i,j]=sum(which(Mpa[,i]==1)%in%which(Mpa[,j]==1))
    }
  }  
  Ma<-ifelse(Ma>0,1,0)
  
  M1<-cbind(Mp,Mpa)
  M2<-cbind(t(Mpa),Ma)
  M0<-rbind(M1,M2)
  M<-M0
  for(j in 1: tao){
    M=M%*%M0
  }
  return(M[1:n,1:n])
}