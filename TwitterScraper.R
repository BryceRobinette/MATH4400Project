library(rtweet)
library(RMariaDB)
source("config.R")
library(sys)

insertTweets = function(df, person, table){
  for (count in c(1:dim(df)[1])){
    runthis <- tryCatch({
      query <- paste0("INSERT INTO ", table, " (tweet, dateTime, person) VALUES (\"", 
                      df$text[count], "\", \"", df$created_at[count], "\", \"", person, "\")")
      rsselect <- dbSendQuery(con, query)
    },
    error = function(e){
      return() 
    })
  }
}

twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

while (TRUE){
  df.trump <- as.data.frame(search_tweets("#trump OR #republican OR #donaldtrump OR #maga OR #teamtrump OR #trump2020", n = 500,
                                        include_rts = FALSE))

  df.biden <- as.data.frame(search_tweets("#biden OR #democrat OR #joebiden OR #teambiden OR #biden2020", n = 500,
                                        include_rts = FALSE))

  insertTweets(df.trump, "trump", "tweets")
  insertTweets(df.biden, "biden", "tweets")
  
  
  df.trump <- get_timelines("realdonaldtrump", n = 3200)
  df.biden <- get_timelines("JoeBiden", n = 3200)
  
  insertTweets(df.trump, "trump", "candidates")
  insertTweets(df.biden, "biden", "candidates")
  
  Sys.sleep(3600)
}
