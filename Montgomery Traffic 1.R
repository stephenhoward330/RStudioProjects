
# Class Participation Tree Models 1



# DATA:

# Montgomery County Traffic Violations

# Source file provided by Dr. Grimshaw
# https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
source('https://grimshawville.byu.edu/TrafficStops2019a.R')

# to get the names of all the variables
names(speed.last)

# to get the size of the dataset
dim(speed.last)



# EDA

# get a summary of the speed variable
summary(speed.last$speed)

# make a histogram to see the distribution of speed
hist(speed.last$speed,breaks=0:65,right=FALSE)



# Analysis

# Train & Test Data sets
set.seed(12)
n.speed.last<-dim(speed.last)[1]
# use 80% of the data for training
train.rows<-sample(n.speed.last, 6800)
speed.train<-speed.last[train.rows,]
# use the remaining 20% for testing
speed.test<-speed.last[-train.rows,]

# confirm train and test are similar
summary(speed.train$speed)
summary(speed.test$speed)

# Model: Random Forest
install.packages("randomForest")
library(randomForest)

# Predict Future Obs
speed.out<-randomForest(x=speed.train[,-24],y=speed.train$speed,
                        xtest=speed.test[,-24],ytest=speed.test$speed,
                        keep.forest=TRUE, # store trees for future prediction
                        replace=TRUE, # use bootstrap sampling
                        ntree=50, # number of trees
                        mtry=5, # rule of thumb is p/3 (round down)
                        nodesize=25)
# let's look at the result
speed.out

# approximate the prediction error with 
# train
sqrt(46.70558)
# test
sqrt(43.34)

# demonstrate prediction on a new observation
predict(speed.out, newdata=new.obs)
# speed prediction is 13.73317 for new.obs

# Does the model make sense?
varImpPlot(speed.out)
# Three most important explanatory variables are latitude, longitude, and hour



# Analysis Review

# Research Task: Predict Quantitative Response Variable

# Data Features:
# Tall & Wide Data
# Quantitative Response Variable
# Large Collection of Possible Explanatory Variables
#   Mix of Quantitative & Categorical Explanatory Variables
#   Choose Best Predictors

# Strengths:
# Predict future continuous response variable
# Explore a large number of possible explanatory
# variables and possible model effects

# Weaknesses:
# At some point tree is finding unique features of training dataset (overfit)
# Don't learn about phenomenon (black box)
# No statistical significance
# "Informal" approach to prediction interval

# Challenge:
# I would predict number of points an NBA team will score in a given game
#   based on explanatory variables such as season field goal percentage,
#   rebounds per game, assists per game, etc.
# Data available here: https://stats.nba.com/teams/


