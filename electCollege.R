#Getting electoral college data
#install.packages(c("dbplyr", "RSQLite"))
#install.packages('stringr')
library('stringr')
library(rtweet)
library(RMariaDB)
library(dplyr)
library(dbplyr)
library('MASS')
library('leaps')
library('class')
library(pROC)
library("RSQLite")

#####################################################################################
# IF YOU WANT TO WORK DIRECTLY WITH THE COMBINED DATA SET, SKIP DOWN TO "SECTION 2"
#####################################################################################

# set working directory
# read in data files
setwd("C:/Users/bryce/Desktop/ElectoralVoteData") # your dirctory
data1976  = read.csv("1976.csv",header=T,na.strings="?")
data1980  = read.csv("1980.csv",header=T,na.strings="?")
data1984  = read.csv("1984.csv",header=T,na.strings="?")
data1988  = read.csv("1988.csv",header=T,na.strings="?")
data1992  = read.csv("1992.csv",header=T,na.strings="?")
data1996  = read.csv("1996.csv",header=T,na.strings="?")
data2000  = read.csv("2000.csv",header=T,na.strings="?")
data2004  = read.csv("2004.csv",header=T,na.strings="?")
data2008  = read.csv("2008.csv",header=T,na.strings="?")
data2012  = read.csv("2012.csv",header=T,na.strings="?")
data2016  = read.csv("2016.csv",header=T,na.strings="?")
# data frames
df1976 = data.frame(data1976)
df1980 = data.frame(data1980)
df1984 = data.frame(data1984)
df1988 = data.frame(data1988)
df1992 = data.frame(data1992)
df1996 = data.frame(data1996)
df2000 = data.frame(data2000)
df2004 = data.frame(data2004)
df2008 = data.frame(data2008)
df2012 = data.frame(data2012)
df2016 = data.frame(data2016)
all.frames = c(df1976,df1980,df1984,df1988,df1992,df1996,df2000,df2004,df2008,df2012,df2016)
head(df1976)

# clean up by changing  "-" to 0s
df1980[-(1:2)] <- lapply(
  df1980[-(1:2)],
  function(x)(gsub("-",0,x))
)
#df1976


# get the outcome choice: red or blue
data.out = function(df){
  outcome = ifelse(df$blue > df$red, 'blue', 'red')
  #df <- df[-ncol(df)]
  df = data.frame(df, outcome)
  
  return(df)
}
df = data.out(df1980)


# add year to the datasets
year = (c(rep(1980, length(df$state))))

df = data.frame(df, year)
df

# Export the appended data sets
write.csv(df,"C:/Users/bryce/Desktop/MyData//1980dat.csv", row.names = FALSE)
  


makeDataGreatAgain = function(df, yr){
  # clean up by changing  "-" to 0s
  df[-(1:2)] <- lapply(
    df[-(1:2)],
    function(x)(gsub("-",0,x))
  )
  outcome = ifelse(df$blue > df$red, 'blue', 'red')
  df = data.frame(df, outcome)
  year = (c(rep(yr, length(df$state))))
  df = data.frame(df, year)
  df
}
mdga = makeDataGreatAgain(df2012, 2012)
mdga

write.csv(mdga,"C:/Users/bryce/Desktop/MyData//2012dat.csv", row.names = FALSE)


# read in new data
setwd("C:/Users/bryce/Desktop/MyData") # your dirctory
data.1976  = read.csv("1976dat.csv",header=T,na.strings="?")
data.1980  = read.csv("1980dat.csv",header=T,na.strings="?")
data.1984  = read.csv("1984dat.csv",header=T,na.strings="?")
data.1988  = read.csv("1988dat.csv",header=T,na.strings="?")
data.1992  = read.csv("1992dat.csv",header=T,na.strings="?")
data.1996  = read.csv("1996dat.csv",header=T,na.strings="?")
data.2000  = read.csv("2000dat.csv",header=T,na.strings="?")
data.2004  = read.csv("2004dat.csv",header=T,na.strings="?")
data.2008  = read.csv("2008dat.csv",header=T,na.strings="?")
data.2012  = read.csv("2012dat.csv",header=T,na.strings="?")
data.2016  = read.csv("2016dat.csv",header=T,na.strings="?")
#make new data frames
df.1976 = data.frame(data.1976)
df.1980 = data.frame(data.1980)
df.1984 = data.frame(data.1984)
df.1988 = data.frame(data.1988)
df.1992 = data.frame(data.1992)
df.1996 = data.frame(data.1996)
df.2000 = data.frame(data.2000)
df.2004 = data.frame(data.2004)
df.2008 = data.frame(data.2008)
df.2012 = data.frame(data.2012)
df.2016 = data.frame(data.2016)

# combine into one data set
#use rbind
df  <- rbind(df.1976, df.1980, df.1984, df.1988, df.1992, df.1996, df.2000, 
             df.2004, df.2008, df.2012, df.2016)
head(df)
tail(df)

# check factor levels on variables
(l = sapply(df, function(x) is.factor(x)))
(l = sapply(df, function(x) is.numeric(x)))
#ifelse(n <- sapply(df, function(x) length(levels(x))) == 1, "DROP", "NODROP")

# need outcome to be a factor
df$outcome = factor(gsub("\\.", "", df$outcome))

# create file
write.csv(df,"C:/Users/bryce/Desktop/MyData//electData.csv", row.names = FALSE)

# checked data and saw special chars
# remove special chars
str_replace_all(df$state, "[^[:alnum:]]", " ")
# note that I still had to curate the data set itself outside of R





##########################################################################################
# SECTION 2
##########################################################################################

elect.data  = read.csv("electData.csv",header=T,na.strings="?")
df = data.frame(elect.data)
df$outcome = factor(gsub("\\.", "", df$outcome))
(l = sapply(df, function(x) is.factor(x))) #checking to make sure outcome is a factor
#attach(df)
summary(df)

# This gives us the mode of our "outcome"
Mode = function(x){
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(df$outcome) 
# Its Red by the way. So historically, most states vote red.




# Below is me just briefly messing around to get errors and see what happens when we
# try to regress on the dataset.
library(RMariaDB)
source("config.R")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

for (count in c(1:dim(df)[1])){
  query <- paste0("INSERT INTO electoral (name, votes, party, year) values (
             \"", df$state[count], "\", 
                  ", df$totalvote[count], ",
                  \"", df$outcome[count],  "\",
                  \"", df$year[count],  "\"
  )")
  rsselect <- dbSendQuery(con, query)
}

query <- "SELECT  
            sum(votes) as votes, 
            party,
            year
          FROM electoral
          group by party, year ;"

df = dbGetQuery(con, query)

# Making incumbent information:
# Incumbent has value of 1 if the party had the incumbent and 0 if the party did not.
incumbent = c(0,0,0,1,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0)
win = c("yes","no","yes","no","yes","no","yes","no","no",
        "yes","no","yes","yes","no","yes","no","no","yes","no","yes","yes","no")

df = data.frame(df, win)
(l = sapply(df, function(x) is.factor(x)))
df$win = factor(gsub("\\.", "", df$win))
(l = sapply(df, function(x) is.factor(x))) #checking to make sure outcome is a factor
#attach(df)
summary(df)
#-------------------------------------------------------------------------------------------
# Training and testing data
#-------------------------------------------------------------------------------------------
test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)

train.X = df[-test.index,]
test.X = df[test.index,]
test.Y = df$outcome[test.index]
train.Y = df$outcome[-test.index]
#-------------------------------------------------------------------------------------------
# Linear Discriminant Analysis
#-------------------------------------------------------------------------------------------

lda.fit = lda(win~.-win, data = df, subset = -test.index)
lda.fit
lda.pred = predict(lda.fit,df[test.index,])
lda.pred$class
mytable = table(lda.pred$class,df[test.index,'win'])
print(mytable)
mean(lda.pred$class==df[test.index,'outcome'])



#-------------------------------------------------------------------
# OLD DATASET
#-------------------------------------------------------------------
# lda.fit = lda(outcome~.-outcome, data = df, subset = -test.index)
# lda.fit
# lda.pred = predict(lda.fit,df[test.index,])
# lda.pred$class
# mytable = table(lda.pred$class,df[test.index,'outcome'])
# print(mytable)
# mean(lda.pred$class==df[test.index,'outcome'])


#----------------------------------------------
# Quick Note: We sholdn't have to worry about
# QDA. We have too many replicate data. We
# get an error about "rank deficiency" when
# we try to fit a quadratic model.
#----------------------------------------------

















#-------------------------------------------------------------------------------------------
# Screwin around
#-------------------------------------------------------------------------------------------
#df$blue = gsub(" $", "", df$blue)#remove blank spaces
#df$red = gsub(" $", "", df$red)
#df$blue = as.numeric(gsub("\\.", "", df$blue))
#df$red = as.numeric(gsub("\\.", "", df$red))
#df$state = as.factor(gsub("\\.", "", df$state))
#df$totalvote = as.numeric(gsub("\\.", "", df$totalvote))
#df$outcome = as.factor(gsub("\\.", "", df$outcome))

#dumbvar1 = c(rnorm(length(df$state), 5, 5))
#dumbvar2 = c(rnorm(length(df$state), 2, 10))

#df = cbind(df, dumbvar1, dumbvar2)



# trainTest = function(df){
#   test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
#   
#   assign(train.X, df[-test.index,], envir = .GlobalEnv)
#   assign(test.X, df[test.index,], envir = .GlobalEnv)
#   assign(train.Y, df[-test.index, ncol(df)], envir = .GlobalEnv)
#   assign(test.Y, df[test.index, ncol(df)], envir = .GlobalEnv)
#   
# }

# for (count in c(1:dim(df.trump)[1])){
#   runthis <- tryCatch({
#     query <- paste0("INSERT INTO tweets (tweet, dateTime, person) VALUES (\"", 
#                     df.trump$text[count], "\", \"", df.trump$created_at[count], "\", \"trump\")")
#     rsselect <- dbSendQuery(con, query)
#   },
#   error = function(e){
#     return() 
#   })
# }

