library(RMariaDB)
source("config.R")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

query <- "SELECT * FROM tweets;"

df = dbGetQuery(con, query)

print(df)
