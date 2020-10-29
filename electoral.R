##########################################################################################
# REGRESSION CHECKING
##########################################################################################
library('stringr')
library(RMariaDB)
library(dplyr)
library(dbplyr)
library('MASS')
library('leaps')
library('class')
library(pROC)
library("RSQLite")

###########################################################################################
# DATA BY STATE AND YEAR
##########################################################################################
# Uses the electData.csv file in the MyData folder on GitHub
# Download it and set your working directory

setwd("C:/Users/bryce/Desktop/TwitterProj") # your dirctory

elect.data  = read.csv("electData.csv",header=T,na.strings="?")
df.state = data.frame(elect.data)
df.state$outcome = factor(gsub("\\.", "", df.state$outcome)) #Makes the outcome a factor.
(l = sapply(df.state, function(x) is.factor(x))) #checking to make sure outcome is a factor

head(df.state) #look at what we got
summary(df.state)

# This gives us the mode of our "outcome"
Mode = function(x){
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(df.state$outcome) 
# Its Red by the way. So historically, most states vote red.

#------------------------------------------------------------------------------------------
# LDA (state data)
#------------------------------------------------------------------------------------------
# Test index for train/test data
test.index = sample(c(1:dim(df.state)[1]), size = floor(.3*dim(df.state)[1]), replace = FALSE)

lda.fit = lda(outcome~.-outcome, data = df.state, subset = -test.index)
lda.fit #What do we see here?
lda.pred = predict(lda.fit,df.state[test.index,])
lda.pred$class
mytable = table(lda.pred$class,df.state[test.index,'outcome'])
print(mytable)
mean(lda.pred$class==df.state[test.index,'outcome'])

# This gives a pretty good accuracy.


Mode(lda.pred$class) # The mode of our predicted outcome. Tells you what color most states
                     # voted for.


###########################################################################################
# There is a different database that we created on SQL. It has the total votes by year
# for each party. It is a much smaller data set which makes it difficult to work with.
###########################################################################################
#
# Get the data 
library(RMariaDB)
source("config.R")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = host,
                 dbname = dbname,
                 user = user,
                 password = password)

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

df = data.frame(df,incumbent,win)
#------------------------------------------------------------------------------------------
# LDA (database data)
#------------------------------------------------------------------------------------------
test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)

lda.fit = lda(win~., data = df, subset = -test.index)
lda.fit
lda.pred = predict(lda.fit,df[test.index,])
lda.pred$class
mytable = table(lda.pred$class, df[test.index,'win'])
print(mytable)
mean(lda.pred$class==df[test.index,'win'])


# Not sure if this helps with sentiment analysis or if we are just finding out how
# accurate lda could predict a win based on passed wins.

#Actually I think we may want to stick with state. it gives weights to those states.
# which means we are more likely to get something to work with. I may be wrong though.







