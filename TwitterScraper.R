library(rtweet)
library(RMariaDB)
source("config.R")

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

df.trump <- as.data.frame(search_tweets("#trump", geocode = lookup_coords(address = "california", apikey = googleAPIkey), n = 500,
                               include_rts = FALSE))
print(df.trump)
df.biden <- as.data.frame(search_tweets("#biden", geocode = lookup_coords(address = "california"), n = 500,
                                     include_rts = FALSE))

for (count in c(1:dim(df.trump)[1])){
  query <- paste0("INSERT INTO tweets (tweet, dateTime, hashtag) VALUES (\"", 
                  df.trump$text[count], "\", \"", df.trump$created_at[count], "\", \"trump\")")
  rsselect <- dbSendQuery(con, query)
}

for (count in c(1:dim(df.biden)[1])){
  query <- paste0("INSERT INTO tweets (tweet, dateTime, hashtag) VALUES (\"",
                  df.biden$text[count], "\", \"", df.biden$created_at[count], "\", \"biden\")")
  rsselect <- dbSendQuery(con, query)
}

dbClearResult(rsselect)
print(rsselect)