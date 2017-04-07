#### count D_xy : a matrix of distance of paper x_i and researcher y_i(x_i's tag)

distance.xy <-function(tag.vec=tag,paper_index)  ## tag.vec : 244 * 1
{
  index<-which(tag.vec == tag.vec[paper_index])
  for(j in index)
  { D_xy<-0
  D_xy<-D_xy+D_xx[paper_index,j]}
  return(D_xy/length(index))
}

#### objective function as f for x_i tagged y_h

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

####build tao-coauthor constrain matric

constrain.mat<-function(feature_mat,tao)
{
  n<-nrow(feature_mat)[1]
  Mp<-diag(x=1,nrow = n, ncol = n)
  coauthor<-unlist(strsplit(feature_mat$Coauthor,split=";"))
  # GET CoAuthor
  coauthor<-unique(coauthor)[-1]
  
  # get name of author :
  coauthor<-ifelse(substr(coauthor,1,1)==" ",substr(coauthor,2,nchar(coauthor)),coauthor)
  coauthor<-ifelse(substr(coauthor,nchar(coauthor),nchar(coauthor))==" ",substr(coauthor,1,nchar(coauthor)-1),coauthor)
  len_coa<-length(coauthor)
  Mpa<-matrix(NA, n, len_coa)
  
  for(i in 1:len_coa){
    for(j in 1:n){
      Mpa[j,i]<-grepl(coauthor[i],feature_mat$Coauthor[j])
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
  M_1<-M0
  for (i in 1:tao)
  {M_1<-M_1 %*% M0}
  return(M_1)
}