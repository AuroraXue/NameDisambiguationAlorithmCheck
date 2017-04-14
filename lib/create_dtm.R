vocabulary_Paper<-function(file){
  it_train <- itoken(file$Paper, 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = file$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on","for",
                                                     "at", "of", "above", "under"))
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- as.matrix(create_dtm(it_train, vectorizer))
  return(dtm_train)
}

vocabulary_Journal<-function(file){
  it_train <- itoken(file$Journal, 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = file$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on","for",
                                                     "at", "of", "above", "under"))
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- as.matrix(create_dtm(it_train, vectorizer))
  return(dtm_train)
}

vocabulary_Coauthor<-function(file){
  it_train <- itoken(file$Coauthor, 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = file$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on","for",
                                                     "at", "of", "above", "under"))
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- as.matrix(create_dtm(it_train, vectorizer))
  return(dtm_train)
}