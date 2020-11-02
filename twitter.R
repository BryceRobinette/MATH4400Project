library(RMariaDB)
source("config.R")
library(tm)
library(syuzhet)
library(wordcloud)
library(randomForest)
library(plyr)

#Clean all data
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

generate_RandomForest = function(df, train.index, trees){
  forest = randomForest(y ~ .-y , data = df, subset = train.index, ntree = trees, replace = TRUE)

  pred <- predict(forest, type = "response", newdata = df[-train.index ,])
  print(table(pred, df[-train.index, 'y']))
  
  return (forest)
}

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

con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

query <- "SELECT DISTINCT tweet, person FROM candidates;"

df = dbGetQuery(con, query)

df$tweet = clean(df$tweet)
df$tweet = clean(df$tweet)

tweets1 = prep_Model_Data(df, 0.97)
train.index <- sample(c(1:dim(tweets1)[1]), floor( 0.7 * dim(tweets1)[1] ), replace = FALSE)

#Generally 500 - 1000 is perfect
trees = 1000
forest = generate_RandomForest(tweets1, train.index, trees)

query <- "SELECT DISTINCT tweet, person FROM tweets;"

df = dbGetQuery(con, query)

df$tweet = clean(df$tweet)
df$tweet = clean(df$tweet)

tweets2 = prep_Model_Data(df, 1)
tweets2 = equalify_rows(tweets2, tweets1)
pred <- predict(forest, type = "response", newdata = tweets2)

#factor 0 for trump, 1 for biden
print(table(pred, tweets2[,'y']))
highest = count(pred)
winner = highest[which.max(highest$freq),]
if (winner[1] == 0){
  print('Trump will win!')
}
if (winner[1] == 1){
  print('Biden will win!')
}

dbDisconnect(con)
