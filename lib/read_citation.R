read_citation <- function(filename){
  #name <- deparse(substitute(filename))
  setwd("~/Spr2017-proj4-team10-master/data/nameset")
  citation <- read.csv(filename,
                       header = F,
                       sep = "\n")    
  #remove symbols disturbing split
  rule = "<([[:alpha:]]|[[:punct:]]){1,4}>"
  citation$V1 = gsub(rule,"",citation$V1)
  rule1 = ">([[:alpha:]]){1,5}:"
  citation$V1 = gsub(rule1,">",citation$V1)
  #change the environment so we can split the citation
  Sys.setlocale('LC_ALL','C')
  #split the rows by ">""
  L <- strsplit(citation$V1,split = ">")
  #create a vector
  citation$Coauthor = laply(L,function(t) t[1])
  citation$Paper = laply(L,function(t) t[2])
  citation$Journal = laply(L,function(t) t[3])
  
  # extract canonical author id befor "_"
  citation$AuthorID <- as.numeric(sub("_.*","",citation$Coauthor))
  # extract paper number under same author between "_" and first whitespace
  citation$PaperNO <- as.numeric(sub(".*_(\\w*)\\s.*", "\\1", citation$Coauthor))
  # delete "<" in AKumar$Coauthor
  citation$Coauthor <- gsub("<","",sub("^.*?\\s","", citation$Coauthor))
  # make firstname+last name continous to improve accuracy
  citation$Coauthor<-gsub(" ","",citation$Coauthor)
  # change the split to \t
  citation$Coauthor<-gsub(";"," ",citation$Coauthor)
  
  #citation$Coauthor<-gsub(pattern = "[[:punct:]]", replacement = "", citation$Coauthor)
  
  # delete "<" in AKumar$Paper
  citation$Paper <- gsub("<","",citation$Paper)
  # add PaperID for furthur use, you may want to combine all the nameset files and 
  # then assign the unique ID for all the citations
  citation$PaperID <- rownames(citation)
  return(citation)
}
