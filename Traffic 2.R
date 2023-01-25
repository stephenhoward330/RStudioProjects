
# Class Participation Tree Models 2



# DATA:

# Montgomery County Traffic Violations

# Source file provided by Dr. Grimshaw
# https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
source('https://grimshawville.byu.edu/TrafficStops2019a.R')

# to get the names of all the variables
names(ticket.last)

# to get the size of the dataset
dim(ticket.last)



# EDA

# get a summary of the ticket variable
summary(ticket.last$Ticket)

# TRUE=Citation, FALSE=Warning
# frequency table
table(ticket.last$Ticket)
# proportion table
prop.table(table(ticket.last$Ticket))



# Analysis

set.seed(2319)

# construct model dataset (all bads/tickets, sample of goods/warnings)

all.bad<-subset(ticket.last,Ticket=="TRUE")
n.bads<-dim(all.bad)[1]
# SRS of goods
all.good<-subset(ticket.last,Ticket=="FALSE")
n.goods<-dim(all.good)[1]
rows.good<-sample(n.goods, n.bads)
sample.good<-all.good[rows.good,]

# smash all bad and sample of goods
ticket.model<-rbind(all.bad,sample.good)

# confirm ticket.model has half goods and half bads
table(ticket.model$Ticket)

# create train and test datasets
n.model<-dim(ticket.model)[1]
# 80% train, 20% test
0.8*n.model  # 108742.4
train.rows<-sample(n.model,100000)
ticket.train<-ticket.model[train.rows,]
ticket.test<-ticket.model[-train.rows,]

# confirm train and test are similar
table(ticket.train$Ticket)
table(ticket.test$Ticket)

# Random Forest
library(randomForest)

names(ticket.train)

ticket.out<-randomForest(x=ticket.train[,-24], y=ticket.train$Ticket,
                         xtest=ticket.test[,-24], ytest=ticket.test$Ticket,
                         keep.forest=TRUE, replace=TRUE, ntree=50, mtry=5,
                         nodesize=25)

# let's see how it went
ticket.out

# make a prediction on a new observation
new.obs
predict(ticket.out, newdata=new.obs)

# intuition of what matters in the model
varImpPlot(ticket.out)
# the three most important explanatory variables are 
#     longitude, latitude, and hour



# Analysis Review

# Research Task: predict a categorical response variable

# Data Features:
# Tall & Wide Data
# Categorical Response Variable
# Large Collection of Possible Explanatory Variables
#   Mix of Quantitative & Categorical Explanatory Variables
#   Choose Best Predictors

# Strengths:
# Explore all possible explanatory variables
# So good at finding predictions that we worry about "overfitting"
#   Note: "benchmark" for "best predicting"
# No assumptions

# Weaknesses:
# No statistical significance (trade off with "no assumptions")
# At some point tree is finding unique features of training dataset (overfit)
# Don't learn about phenomenon (black box)

# Challenge:
# I would predict if an NBA team would win a given game
#   based on explanatory variables such as season field goal percentage,
#   rebounds per game, assists per game, etc.
# Data available here: https://stats.nba.com/teams/




