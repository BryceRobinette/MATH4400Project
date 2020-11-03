#You will need to change--------------------------------------------------------
setwd("C:/Users/User/Desktop/Fall2020/MATH 4400/TwitterProj")
install.packages('RMariaDB')
install.packages('ROAuth')
install.packages('tm')
install.packages('syuzhet')
install.packages('twitteR')
install.packages('ggplot2')
install.packages("SnowballC")
library(SnowballC)
library(ROAuth)
library(tm)
library(syuzhet)
library(twitteR)
library(RMariaDB)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
source("config.R")

#Connecting to Database---------------------------------------------------------
con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

query <- "SELECT DISTINCT * FROM tweets;"

df = dbGetQuery(con, query)

print(df)

#Separating Trump & Biden into two different datasets---------------------------

query.t <- "SELECT DISTINCT * FROM tweets WHERE person LIKE 'trump';"
trump.data = dbGetQuery(con, query.t)
print(trump.data)

query.b <- "SELECT DISTINCT * FROM tweets WHERE person LIKE 'biden';"
biden.data = dbGetQuery(con, query.b)
print(trump.data)

#Cleaning tweets----------------------------------------------------------------

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

trump.text = clean(trump.data$tweet)
biden.text = clean(biden.data$tweet)

#Sentiment Function------------------------------------------------------------- 
sentiment.info = function(text){ 
  
  text.sentiment = get_nrc_sentiment((text))
  sentiment.score = data.frame(colSums(text.sentiment[,]))
  names(sentiment.score) = 'Score'
  sentiment.score = cbind("sentiment" = rownames(sentiment.score), sentiment.score)
  rownames(sentiment.score) = NULL
  
  return(sentiment.score)
}

#Sentiment Graph for Trump------------------------------------------------------
sentiment.trump = sentiment.info(trump.text)

ggplot(data = sentiment.trump, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
  theme(legend.position = "none") +
  xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments of Trump")

#Sentiment Graph for Biden------------------------------------------------------
sentiment.biden = sentiment.info(biden.text)

ggplot(data = sentiment.biden, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
  theme(legend.position = "none") +
  xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments of Biden")

#Copy and Past Info for Sentiment Analysis on Excel Sheet
sentiment.trump
sentiment.biden

#Creating Corpus----------------------------------------------------------------

biden.corpus.clean = Corpus(VectorSource(biden.text))
trump.corpus.clean = Corpus(VectorSource(trump.text))

biden.trump.corpus.clean = Corpus(VectorSource(c(biden.text,trump.text)))

y= c( rep(1, length(biden.corpus.clean)) , rep(0, length(trump.corpus.clean)))

#WordCloud------------------------------------------------------------------------------------------
wordcloud(biden.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)
wordcloud(trump.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)

DTM <- DocumentTermMatrix(biden.trump.corpus.clean)
inspect(DTM)
sparse_DTM <- removeSparseTerms(DTM, 0.97)
tweets <- as.data.frame(as.matrix(sparse_DTM))
tweets <- as.data.frame(as.matrix(tweets))
colnames(tweets) <- make.names(colnames(tweets))

DTM.trump <- DocumentTermMatrix(trump.corpus.clean)
inspect(DTM.trump)
sparse_DTM.trump <- removeSparseTerms(DTM.trump, 0.97)
tweets.trump <- as.data.frame(as.matrix(sparse_DTM.trump))
tweets.trump <- as.data.frame(as.matrix(tweets.trump))
colnames(tweets.trump) <- make.names(colnames(tweets.trump))