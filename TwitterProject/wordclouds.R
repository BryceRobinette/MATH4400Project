#You will need to change--------------------------------------------------------
library(SnowballC)
library(ROAuth)
library(tm)
library(syuzhet)
library(twitteR)
library(RMariaDB)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(sys)

#Connecting to Database---------------------------------------------------------
generateWorldCloud = function(){
  source("helpers.R")
  query <- "SELECT DISTINCT * FROM tweets;"
  
  df = dbGetQuery(con, query)
  
  #Separating Trump & Biden into two different datasets---------------------------
  
  query.t <- "SELECT DISTINCT * FROM tweets WHERE person LIKE 'trump';"
  trump.data = dbGetQuery(con, query.t)
  
  query.b <- "SELECT DISTINCT * FROM tweets WHERE person LIKE 'biden';"
  biden.data = dbGetQuery(con, query.b)
  
  #Cleaning tweets----------------------------------------------------------------
  
  trump.text = clean(trump.data$tweet)
  biden.text = clean(biden.data$tweet)
  
  #Sentiment Graph for Trump------------------------------------------------------
  sentiment.trump = sentiment.info(trump.text)
  
  print('Sentiment of Trump')
  ggplot(data = sentiment.trump, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' ) +
    theme(legend.position = "none") +
    xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments of Trump")
  
  #Sentiment Graph for Biden------------------------------------------------------
  sentiment.biden = sentiment.info(biden.text)
  
  print('Sentiment of Biden')
  ggplot(data = sentiment.biden, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
    theme(legend.position = "none") +
    xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments of Biden")
  
  Sys.sleep(10)
  
  #Creating Corpus----------------------------------------------------------------
  
  biden.corpus.clean = Corpus(VectorSource(biden.text))
  trump.corpus.clean = Corpus(VectorSource(trump.text))
  
  biden.trump.corpus.clean = Corpus(VectorSource(c(biden.text,trump.text)))
  
  y = c( rep(1, length(biden.corpus.clean)) , rep(0, length(trump.corpus.clean)))
  
  #WordCloud------------------------------------------------------------------------------------------
  
  print('Wordcloud of Biden')
  wordcloud(biden.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)
  Sys.sleep(10)
  
  print('Wordcloud of Trump')
  wordcloud(trump.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)
  Sys.sleep(10)
  
  tweets = preprocess(biden.trump.corpus.clean, .97)
  
  tweets.trump = preprocess(trump.corpus.clean, .97)
}