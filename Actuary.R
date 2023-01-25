
# Simple Linear Logistic Regression

# Exam P and GPA



# DATA

examp=read.csv(header=TRUE,text="
Student,ExamP,GPA
2,No Pass,3.42
3,No Pass,3.93
4,No Pass,3.41
5,No Pass,3.75
6,Passed,3.18
7,No Pass,2.76
8,No Pass,3.68
9,No Pass,3.11
10,Passed,3.77
11,No Pass,3.06
12,No Pass,3.89
13,No Pass,3.28
14,Passed,3.82
15,No Pass,2.8
16,No Pass,3.45
17,No Pass,3.75
18,No Pass,3.7
19,No Pass,2.61
20,No Pass,3.17
21,Passed,3.93
22,No Pass,2.94
23,No Pass,2.49
24,No Pass,3.77
25,No Pass,2.7
26,Passed,3.4
27,No Pass,3.87
28,No Pass,2.55
29,Passed,3.81
30,No Pass,1.92
31,No Pass,2.79
32,No Pass,3.42
33,Passed,3.91
34,No Pass,3.94
35,Passed,3.6
36,Passed,3.89
37,No Pass,3.66
38,No Pass,3.02
39,Passed,3.87
40,No Pass,3.92
41,No Pass,3.27
42,Passed,3.32
43,Passed,3.52
44,No Pass,3.75
45,No Pass,3.87
46,No Pass,2.67
")



# EDA

# produces table of frequencies (counts)
table(examp$GPA>=3.5,examp$ExamP)

# Naively doing prop.table(table(examp$GPA>=3.5,examp$ExamP))
#  gives joint proportions, not the row marginals we want

# produces a table conditional on GPA (row marginal proportions):
prop.table(table(examp$GPA>=3.5,examp$ExamP),margin=1)

# boxplot showing students who pass Exam P have higher GPAs than those who don't
boxplot(GPA~ExamP,data=examp)



# Analysis

# Contingency Table (convert quantitative explanatory variable to factor)
chisq.test(table(examp$GPA>=3.5,examp$ExamP))

# Response Variable:
#   instead of using "ExamP" as the column name
# Pass or No Pass
# Pass = 1 if Pass
#      = 0 if No Pass

examp$Pass<-ifelse(examp$ExamP == "Passed", 1, 0)

# Explanatory Variable:
# GPA (quantitative)

# Model:
#   log ( P(Pass) / P(No Pass) )
#     = beta0 + beta1 GPA

# Fit Model
examp.out<-glm(Pass~GPA, data=examp, family="binomial")

# Report parameter estimates and standard errors
summary(examp.out)

# How do we interpret the 2.256 estimated coefficient on GPA?
# a one unit increase in GPA is estimated to increase the log of the odds ratio of passing Exam P by 2.256
# better:
exp(2.256)  # 9.54
# estimated odds of passing Exam P increase by almost 10 times for increasing GPA by 1 point
# (B to A)

# Is this statistically significant?
# Ho: beta1 = 0
# Wald test
# GPA has a statistically significant effect on passing Exam P (z=2.133, p-value=0.0329).

# Ho: no GPA effect
# Likelihood Ratio Test comparing full and reduced model
# reduced model assumes Ho is true
examp.red<-glm(Pass~+1, data=examp, family="binomial")
anova(examp.red,examp.out,test="Chisq")
# GPA has a statistically significant effect on passing Exam P (X2=6.5393, p-value=0.01055).

# 95% CI on beta1
confint(examp.out)
# since zero is not in the conf int we conclude GPA has a statistically significant effect
exp(confint(examp.out))[-1,]
# since one is not in the conf int we conclude GPA has a statistically significant effect

# predict probability of passing Exam P for two students: GPA 3.25, 3.85
predict(examp.out,newdata=data.frame(GPA=c(3.25, 3.85)), type="response")

# Create a graphic of the logistic regression model
# data
plot(Pass~GPA,data=examp,pch=19,xlab="GPA",ylab="P ( Pass Exam P  | GPA )")
# curve
xstar<-seq(2,4,length=100)
phat<-predict(examp.out,newdata=data.frame(GPA=xstar), type="response")
lines(xstar,phat,col="gray",lwd=2)

# add CI to curve
# first, compute CI in log-odds
logit.hat<-predict(examp.out,newdata=data.frame(GPA=xstar), type="link", se.fit=TRUE)
logit.L<-logit.hat$fit -1.96 * logit.hat$se.fit
logit.U<-logit.hat$fit +1.96 * logit.hat$se.fit
# second, untransform
phat.L<-1/(1+exp(-logit.L))
phat.U<-1/(1+exp(-logit.U))
# third, add to plot
lines(xstar,phat.L,col="gray",lty=3)
lines(xstar,phat.U,col="gray",lty=3)

# log-odds plot ... won't show data
plot(xstar,logit.hat$fit,type="l",col="gray",lwd=2,
     xlab="GPA", ylab="log Odds Ratio of Passing Exam P")
lines(xstar,logit.L,col="gray",lty=3)
lines(xstar,logit.U,col="gray",lty=3)

# create ROC curve (used to demonstrate prediction performance)
library(ROCR)
examp.pred<-prediction(predict(examp.out,type="response"),examp$Pass)
examp.perf<-performance(examp.pred, measure="tpr", x.measure="fpr")
plot(examp.perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
abline(0,1,col="gray")

# compute AUC (area under curve, its like R^2)
performance(examp.pred,measure="auc")  # 0.7209596



# Analysis Review

# Research Task: Both inference and prediction on phenomenon

# Data Features:
# Response variable is binary (two possible outcomes)
# Explanatory variable is continuous (could have been a factor ... also we are interested in lots of
#                                                  explanatory variables ... next case)

# Strengths
# modeling conditional probability
# inference (statisitical significance)
# interpretable effects (must untransform ... and talk in odds)
# graphics of effects (probabilities, if we untransform)
# AUC as a parallel to R^2 (one number summary of prediction performance), ROC curve as a graphic

# Weaknesses
# not intuitive "log odds" model effects

# Challenge
# I would predict the effect of NBA scoring (player-level) on wins. Some say that scoring points is all that
#   matters. So I want to see how the scoring of any one player effects their chance of winning. This has one
#   explanatory variable (points scored by a player) and one response variable (winning) which is either 0 or 1.
#   Data can be found here: https://stats.nba.com/players/




