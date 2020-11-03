source("config.R")

#This function cleans the data, removes unneeded or misleading information.
clean = function(text){
  text = tolower(text)
  text = gsub("@\\w+", "", text)
  text = gsub("[[:punct:]]", "", text)#remove punctuation
  text = gsub("http\\w+","", text)#remove links
  text = gsub("[ |\t]{2,}", "", text)#remove tabs
  text = gsub("^ ", "", text)#remove blank spaces
  text = gsub(" $", "", text)#remove blank spaces
  text = removeWords(text  ,  stopwords("en") )#remove stop words
  text = stemDocument(text)#Stems the document
  
  return(text)
}

#Sentiment Function------------------------------------------------------------- 
sentiment.info = function(text){ 
  
  text.sentiment = get_nrc_sentiment((text))
  sentiment.score = data.frame(colSums(text.sentiment[,]))
  names(sentiment.score) = 'Score'
  sentiment.score = cbind("sentiment" = rownames(sentiment.score), sentiment.score)
  rownames(sentiment.score) = NULL
  
  return(sentiment.score)
}

#This preprocesses the data and gets it into a format that we care about.
preprocess = function(corpus, sparsitivity){
  DTM <- DocumentTermMatrix(corpus)
  
  if (sparsitivity < 1){
    sparse_DTM <- removeSparseTerms(DTM, sparsitivity)
    tweets <- as.data.frame(as.matrix(sparse_DTM))
    tweets <- as.data.frame(as.matrix(tweets))
  }
  else{
    tweets <- as.data.frame(as.matrix(DTM))
  }
  colnames(tweets) <- make.names(colnames(tweets))
  
  return (tweets)
}

#This preps the model into a corpus and preprocesses it.
prep_Model_Data = function(df, sparsitivity){
  trump.text = df$tweet[df$person == 'trump']
  biden.text = df$tweet[df$person == 'biden']
  
  trump.corpus = Corpus(VectorSource(trump.text))
  biden.corpus = Corpus(VectorSource(biden.text))
  
  biden.trump.corpus = Corpus(VectorSource(c(biden.text,trump.text)))
  
  tweets = preprocess(biden.trump.corpus, sparsitivity)
  
  y = as.factor(c( rep(1, length(biden.corpus)) , rep(0, length(trump.corpus)) ))
  
  tweets = data.frame(y, tweets)
  
  return (tweets)
}

#This generates the Random Forest.
generate_RandomForest = function(df, train.index, trees){
  forest = randomForest(y ~ .-y , data = df, subset = train.index, ntree = trees, replace = TRUE)
  
  pred <- predict(forest, type = "response", newdata = df[-train.index ,])
  
  return (forest)
}

#This transforms all rows that are to be predicted to work with the model.
equalify_rows = function(df, da){
  for (name in colnames(da)){
    if (!name %in% colnames(df) & name != 'y')
    {
      print(name)
      df[,name] <- NA
    }
  }
  
  return (df[colnames(da)])
}

#Database connection object.
con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)