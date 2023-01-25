
# Logistic Regression

# League of Legends



# DATA

library(httr)
library(jsonlite)

get1<-GET("https://s3-us-west-1.amazonaws.com/riot-developer-portal/seed-data/matches10.json")
alljson1<-fromJSON(content(get1, "text", encoding = "UTF-8"))

# get the 100 chunks and assemble into a large data frame
LOL.big<-NULL

for(i in 1:100) {
this.row.i<-data.frame(Win=ifelse(alljson1$matches$participants[[i]]$stats$win==TRUE,1,0),
                       Kills=alljson1$matches$participants[[i]]$stats$kills,
                       Deaths=alljson1$matches$participants[[i]]$stats$deaths,
                       Assists=alljson1$matches$participants[[i]]$stats$assists,
                       GoldEarned=alljson1$matches$participants[[i]]$stats$goldEarned,
                       LongestTimeSpentLiving=alljson1$matches$participants[[i]]$stats$longestTimeSpentLiving,
                       LargestMultiKill=alljson1$matches$participants[[i]]$stats$largestMultiKill)
LOL.big<-rbind(LOL.big,this.row.i)
}



# EDA

par(mfrow=c(3,2))
boxplot(Kills~Win,data=LOL.big)
boxplot(Deaths~Win,data=LOL.big)
boxplot(Assists~Win,data=LOL.big)
boxplot(GoldEarned~Win,data=LOL.big)
boxplot(LongestTimeSpentLiving~Win,data=LOL.big)
boxplot(LargestMultiKill~Win,data=LOL.big)
par(mfrow=c(1,1))


# Extended EDA

plot(Kills~Deaths,data=LOL.big)
plot(jitter(Kills)~jitter(Deaths),data=LOL.big)

# filtering the data set Kills>20 or Deaths>15 are bad influential

plot(GoldEarned~LongestTimeSpentLiving, data=LOL.big)
points(GoldEarned~LongestTimeSpentLiving, data=subset(LOL.big,Win==0), pch=19, col="gray")
points(GoldEarned~LongestTimeSpentLiving, data=subset(LOL.big,Win==1), pch=19, col="black")
# consider obs 175, 374, 322, 526 where they amazingly lost (bad influential)
points(GoldEarned~LongestTimeSpentLiving, data=LOL.big[c(175,322,374,526),], pch=19, col="red")
# consider obs 792 where they amazingly won (bad influential)
points(GoldEarned~LongestTimeSpentLiving, data=LOL.big[c(792),], pch=19, col="green")

# remove the influential and outliers
LOL.big<-LOL.big[ -c(175,322,374,526,792), ]
LOL.big<-subset(LOL.big, Kills<20 & Deaths<15)

dim(LOL.big)



# Analysis

# response variable: Win = 1 if win, 0 if lost
# explanatory variables: 
#   offense:     Kills, GoldEarned
#   errors:      Deaths
#   team play:   Assists
#   risk/reward: LongestTimeSpentLiving
#   hot hand:    LargestMultiKill

# create train and test
dim(LOL.big)

table(LOL.big$Win)
# naturally a 50-50 split of response variable, so we'll omit the stratified sampling step

set.seed(58)
train.ind<-sample(989,700)

LOL.train<-LOL.big[train.ind,]
LOL.test<-LOL.big[-train.ind,]

# confirm similarity between train and test
summary(LOL.train)
summary(LOL.test)

# fit model

# logit( P(Win = 1) ) = beta0 + beta1 Kills + beta2 GoldEarned
#                             + beta3 Deaths + beta4 Assists + beta5 LongestTimeSpentLiving
#                             + beta6 LargestMultiKill

LOL.out<-glm(Win ~ Kills + GoldEarned + Deaths + Assists + LongestTimeSpentLiving + LargestMultiKill,
             data=LOL.train, family="binomial")

# report estimates and std errors
summary(LOL.out)

# interpret estimated effects

# KILLS:
#   more kills increases the odds of winning holding all else constant

# logistic regression estimate
# for each additional kill we estimate the log odds of winning increases by 0.048 holding all else constant

# "untransform" for a better interpretation
exp(0.048)
# for each additional kill we estimate the odds of winning increase by 4.9% holding all else constant

# confidence interval (untransform)
exp(confint(LOL.out))[-1,]
# for each additional kill we estimate the odds of winning increase by 4.9% (95% CI: 4.6% decrease, 15.4% increase)
#   holding all else constant

# graphic of effect (log-odds interpretation)
x.star<-seq(0,10,length=100)
plot(x.star,coef(LOL.out)[2]*x.star, type="l",
     xlab="Kills",ylab="Partial Log Odds of Winning")

# graphic of effect from probability perspective
#  change Kills but hold all other explanatory variables at their median values
summary(LOL.train)

x.star<-data.frame(Kills=seq(0,10,length=100),
                   Deaths=5,Assists=7,GoldEarned=10868,LongestTimeSpentLiving=590,LargestMultiKill=1)
plot(x.star$Kills,predict(LOL.out,newdata=x.star,type="response"),type="l",
     xlab="Kills",ylab="P(Win), all other factors at median",ylim=c(0,1))

# GOLDEARNED

# interpretation for non-technical audience: 
#   more gold earned increases the odds of winning holding all else constant

# graphic
x.star<-data.frame(Kills=5,
                   Deaths=5,Assists=7,GoldEarned=seq(5000,18000,length=100),
                   LongestTimeSpentLiving=590,LargestMultiKill=1)
plot(x.star$GoldEarned,predict(LOL.out,newdata=x.star,type="response"),type="l",
     xlab="Gold Earned",ylab="P(Win), all other factors at median",ylim=c(0,1))

# ASSISTS

# interpretation for non-technical audience: 
#   more assists increases the odds of winning holding all else constant

# graphic
x.star<-data.frame(Kills=5,
                   Deaths=5,Assists=seq(0,30,length=100),GoldEarned=10868,
                   LongestTimeSpentLiving=590,LargestMultiKill=1)
plot(x.star$Assists,predict(LOL.out,newdata=x.star,type="response"),type="l",
     xlab="Assists",ylab="P(Win), all other factors at median",ylim=c(0,1))


# Inference:

# Is earning gold statistically significant?

# Wald Test Ho: beta2=0
summary(LOL.out)

# Gold Earned has a statistically significant effect on winning after adjusting for all other factors
#   (p-value < 0.0001)

# LRT test (full & reduced model comparison)
LOL.red<-glm(Win ~ Kills + Deaths + Assists + LongestTimeSpentLiving + LargestMultiKill,
             data=LOL.train, family="binomial")

anova( LOL.red, LOL.out, test="Chisq")

# Gold Earned has a statistically significant effect on winning after adjusting for all other factors
#   (p-value < 0.0001)

# Which test to report? Common practice is to report contrast (since it's a single betaj)


# Is there a hot hand effect? (Ben Cohen, "The Hot Hand")

# Wald Test Ho: beta6=0 (LargestMultiKill)

summary(LOL.out)
# there is no statistically significant hot hand in LOL after adjusting for all other factors (p-value=0.2116)

# LRT test (full & reduced model comparison)
LOL.red<-glm(Win ~ Kills + GoldEarned + Deaths + Assists + LongestTimeSpentLiving,
             data=LOL.train, family="binomial")

anova(LOL.red, LOL.out, test="Chisq")

# there is no statistically significant hot hand in LOL after adjusting for all other factors (p-value=0.2126)

# be careful interpreting "non-significant" ... haven't proved hot hand doesn't exist.
# if there is a hot hand effect it is very small compared to the effects we found statistically significant.


# Test if "being aggressive matters"
#   Ho: deaths, assists, longest time spent living have no effect

# LRT test (full & reduced model comparison)
LOL.red<-glm(Win ~ Kills + GoldEarned + LargestMultiKill,
             data=LOL.train, family="binomial")

anova(LOL.red,LOL.out,test="Chisq")

# playing aggessive has a statistically significant effect (p-value<0.0001)


# Prediction

# predict probability of winning for a player with Faker-like skills
predict(LOL.out, newdata=data.frame(Kills=2, GoldEarned=15000, Deaths=3, Assists=8,
                                    LongestTimeSpentLiving=600, LargestMultiKill=2), type="response")

# 95% CI on conditional probability
#  first, compute in log-odds
Faker.logit<-predict(LOL.out, newdata=data.frame(Kills=2, GoldEarned=15000, Deaths=3, Assists=8,
                                                 LongestTimeSpentLiving=600, LargestMultiKill=2), 
                     type="link", se.fit=TRUE)
logit.L<-Faker.logit$fit - 1.96 * Faker.logit$se.fit
logit.U<-Faker.logit$fit + 1.96 * Faker.logit$se.fit
#  second, compute in probability
Faker.phat.L<-1/(1+exp(-logit.L))
Faker.phat.U<-1/(1+exp(-logit.U))


# ROC curve (train & test)

library(ROCR)
train.pred<-prediction(predict(LOL.out,type="response"), LOL.train$Win)
train.perf<-performance(train.pred,measure="tpr",x.measure="fpr")
plot(train.perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
abline(0,1,col="gray")

performance(train.pred,measure="auc")

# test
test.pred<-prediction(predict(LOL.out,newdata=LOL.test,type="response"),LOL.test$Win)
test.perf<-performance(test.pred,measure="tpr",x.measure="fpr")
plot(test.perf,add=TRUE,col="royalblue")

performance(test.pred,measure="auc")



# Analysis Summary

# Research Task: Both inference and prediction on phenomenon

# Data Features:
# Response variable is binary (two possible outcomes)
# Six explanatory variables are continuous

# Strengths
# modeling conditional probability
# inference (statisitical significance)
# interpretable effects (must untransform ... and talk in odds)
# graphics of effects (probabilities, if we untransform)
# AUC as a parallel to R^2 (one number summary of prediction performance), ROC curve as a graphic
# Train and Test datasets

# Weaknesses
# not intuitive "log odds" model effects

# Challenge
# I'm a big fan of league of legends, and I play it almost every day. So, what I would like to do would be to
#   grab more information from the Riot API to learn more about other variables impact win percentage.
#   A few that I would like to test are vision score, crowd control score, damage dealt, and damage taken.
# Data is available here: https://developer.riotgames.com/




