library(RMariaDB)
library(tm)
library(syuzhet)
library(wordcloud)
library(randomForest)
library(plyr)

#Run random forest algorigthm.
Random_Forest = function(){
  source("helpers.R")
  query <- "SELECT DISTINCT tweet, person FROM candidates;"
  
  df = dbGetQuery(con, query)
  
  df$tweet = clean(df$tweet)
  df$tweet = clean(df$tweet)
  
  training_data = prep_Model_Data(df, 0.97)
  train.index <- sample(c(1:dim(training_data)[1]), floor( 0.7 * dim(training_data)[1] ), replace = FALSE)
  
  #Generally 500 - 1000 trees is perfect
  trees = 1000
  forest = generate_RandomForest(training_data, train.index, trees)
  
  query <- "SELECT DISTINCT tweet, person FROM tweets;"
  
  df = dbGetQuery(con, query)
  
  df$tweet = clean(df$tweet)
  df$tweet = clean(df$tweet)
  
  input_data = prep_Model_Data(df, 1)
  input_data = equalify_rows(input_data, training_data)
  pred <- predict(forest, type = "response", newdata = input_data)
  
  #factor 0 for trump, 1 for biden
  highest = count(pred)
  winner = highest[which.max(highest$freq),]
  if (winner[1] == 0){
    return('Trump')
  }
  
  if (winner[1] == 1){
    return('Biden')
  }
}
