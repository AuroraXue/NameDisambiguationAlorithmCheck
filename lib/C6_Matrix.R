source("lib/Extract Coauthors.R")
#unique_Coauthors_list
#Coauthors_list

M_prod<-function(list,coauthor){
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
  M<-rbind(M1,M2)
  
  return(M)
}

M_1 <- M_prod(Coauthors_list[[1]],unique_Coauthors_list[[1]])