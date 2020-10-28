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
source("config.R")

#Connecting to Database---------------------------------------------------------
con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

query <- "SELECT * FROM tweets;"

df = dbGetQuery(con, query)

print(df)

#Separating Trump & Biden into two different datasets---------------------------

query.t <- "SELECT * FROM tweets WHERE person LIKE 'trump';"
trump.data = dbGetQuery(con, query.t)
print(trump.data)

query.b <- "SELECT * FROM tweets WHERE person LIKE 'biden';"
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

#Creating Corpus----------------------------------------------------------------

biden.corpus.clean = Corpus(VectorSource(biden.text))
trump.corpus.clean = Corpus(VectorSource(trump.text))

biden.trump.corpus.clean = Corpus(VectorSource(c(biden.text,trump.text)))

y= c( rep(1, length(biden.corpus.clean)) , rep(0, length(trump.corpus.clean)))

#WordCloud------------------------------------------------------------------------------------------

library(wordcloud)
library(RColorBrewer)

wordcloud(biden.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)
wordcloud(trump.corpus.clean, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 50)




