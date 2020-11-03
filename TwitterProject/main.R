source("forest.R")
source("wordclouds.R")

#You run the program from here.
#This predicts who will win the election.
winnerForest = Random_Forest()

#This generates all of our graphs and workclouds.
#Takes a loooooong time to run. Be prepared.
#Something to note, there is a bug with rstudio that you need to be aware of.
#In rstudio if you run a function and inside that function is a ggplot, the plot will not show up. This is an ongoing issue with RStudio.
#We have ggplots in this program, to see them you will need to go to our report or you can run all of the code by itself 
#directly from the file.
generateWorldCloud()

print(paste0('The winner is: ', winnerForest))
