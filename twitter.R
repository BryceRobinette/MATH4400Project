library(RMariaDB)
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "batestocks.cgrwtpcjkd6h.us-west-2.rds.amazonaws.com",
                 dbname = "TwitterFeed",
                 user = "school",
                 password = "password123")

query <- "SELECT * FROM tweets;"

df = dbGetQuery(con, query)

print(df)
