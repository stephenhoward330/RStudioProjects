
# Class Participation Regression 4

# Orioles



# DATA

library(rvest)
url<-"https://www.espn.com/mlb/standings/_/season/2019"

mlb<-read_html(url)
standings<-html_table(mlb)[[2]]
colnames(standings)<-standings[1,]
standings<-standings[-c(1,7,13),]

#construct division from the table layout
standings$division<-c(rep("East",5), rep("Central",5), rep("West",5))

standings$W<-as.numeric(standings$W)
standings$run.diff<-as.numeric(substring(standings$DIFF,2)) * ifelse(substring(standings$DIFF,1,1)=="+",1,-1)

standings<-standings[,c("division","W","run.diff")]



# EDA

# summary statistics by division
by(standings$W,standings$division,mean)
by(standings$run.diff,standings$division,mean)

by(standings$W,standings$division,sd)
by(standings$run.diff,standings$division,sd)

# scatterplot
plot(W~run.diff,data=standings,main="Run Differential and Wins by Division",
     xlab="Run Differential",ylab="Wins",type="n")
points(W~run.diff,data=subset(standings,division=="East"),pch=19,col="orange")
points(W~run.diff,data=subset(standings,division=="Central"),pch=19,col="black")
points(W~run.diff,data=subset(standings,division=="West"),pch=19,col="red")



# Analysis

# What's different?

# explanatory variable is factor / qualitative

# goal is to identify statistically significant difference in divisions
#   ideally a BF(3) ... randomized experiment ... require all else constant

# we will make the comparison fair by adjusting for team quality

# Analysis of covariance (ANACOVA)

# response variable is winning (W)
# explanatory of interest: factor divisions
# explanatory variable to adjust for team quality: run.diff

# naive:
mlb.out<-lm(W~division+run.diff,data=standings)

# make division a factor
standings$division<-factor(standings$division)
# to make the comparisons of factor levels to "East"
standings$division<-relevel(standings$division, "East")

# fit model
mlb.out<-lm(W~division+run.diff,data=standings,x=TRUE,y=TRUE)

mlb.out$x
# there isn't a column for divisionEast since divisionWest and divisionCentral are the difference related to
#   divisionEast.

# If East:    W = beta0 +         beta3 * run.diff (since divisionCentral=0 and divisionWest=0 if division="East")
# If Central: W = beta0 + beta1 + beta3 * run.diff
# If West:    W = beta0 + beta2 + beta3 * run.diff

# table of estimated coefficients and std errors
summary(mlb.out)
confint(mlb.out)

# interpret coefficient on run.diff:
# Scoring or preventing runs has a statistically significant effect on winning (pvalue<0.001).
# For each run scored or prevented we estimate a 0.10 (95% CI: 0.93, 0.112) increase in the number of wins
#   keeping teams in the same division.

# why is the interpretation on "division effect" complicated?
#   there are pairs of effects between factor levels
#   the way we have expressed the model (using relevel function), the pairs of comparisons we are interested in are:
#     Central compared to East, West compared to East

# we have really 3 models!
# estimated models:

# If East:    hat(W) = 80.062         + 0.103 * run.diff
# If Central: hat(W) = 80.062 + 1.271 + 0.103 * run.diff
# If West:    hat(W) = 80.062 + 1.405 + 0.103 * run.diff

# create a plot showing models
# scatterplot
plot(W~run.diff,data=standings,main="Run Differential and Wins by Division",
     xlab="Run Differential",ylab="Wins",type="n")
points(W~run.diff,data=subset(standings,division=="East"),pch=19,col="orange")
points(W~run.diff,data=subset(standings,division=="Central"),pch=19,col="black")
points(W~run.diff,data=subset(standings,division=="West"),pch=19,col="red")
# overlay lines
abline(80.062, 0.103, col="orange")
abline(80.062+1.271, 0.103, col="black")
abline(80.062+1.405, 0.103, col="red")

# Central division teams and West division teams are different from East division teams after adjusting for
#   team quality (run differential).

# statistical test

# Naive:
#   manufacture the conclusion from pairwise comparisons in the table

# ANOVA test for Ho: no division effect

# reduced model assuming Ho is true
mlb.reduced<-lm(W~run.diff,data=standings)

# compute ANOVA F-test
anova(mlb.reduced, mlb.out)
# F-stat = 0.3438, pvalue = 0.7164

# There is no statistically significant effect of division on wins after adjusting for team quality (pvalue=0.7164).



# Analysis Review

# Research Task: Inference on Phenomenon ("statistically significant")

# Data Features:
# Columns:
#   continuous response variable
#   explanatory variable is a factor (k levels) ... would be a BF(1) if this was explored as a randomized experiment
#       with identical experimental units ... but since there wasn't identical experimental units there is an
#       explanatory variable (continuous) that adjusts for the fact experimental units are different.
# Rows:
#   observations from randomized experiment or observational study

# Strengths:
# Analysis of Covariance generalizes the spirit of randomized experiments to observational studies
# Test if factor has a statistically significant effect on response variable
# If that's significant then answer which pair of levels differ

# Weaknesses:
# Correct Model specification (critical in EDA to identify if interaction or not)
# Model assumptions that must be satisfied (look during EDA ... additive model/linear, constant variance, outcome
#   of one observation doesn't effect the outcome of another, variation is normally distributed).

# Was it a weakness that we compared to "East" division (relevel function)? -- Not really, it was our question

# Challenge:
# In the NBA, there has recently been talk about if they should get rid of the conferences (some fans often say that 
#   the west is better that the East). We could do an analysis like this for the NBA data, splitting teams up into
#   the two conferences (East and West) and using point differential to measure team quality. Our goal would be to
#   see if there is an actual difference between the conferences.
#   NBA team stats are available here:  https://stats.nba.com/teams/traditional/?sort=W_PCT&dir=-1








