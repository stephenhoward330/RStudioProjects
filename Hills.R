
# Class Participation Regression 2

# Scottish hill races



# DATA:

# data at http://www.statsci.org/data/general/hills.txt
hills<-read.table("http://www.statsci.org/data/general/hills.txt", header=TRUE)



# EDA:

# plot scatterplots
# Distance to Time
plot(hills$Distance,hills$Time)
# identify points
#identify(hills$Distance,hills$Time,hills$Race)
# I see that the races KnockHill and BensofJura are unexpected on this plot

# Climb to Time
plot(hills$Climb,hills$Time)
#identify(hills$Climb,hills$Time,hills$Race)
# I see that the races KnockHill, SevenHills, LairigGhru, MoffatChase,
#     TwoBreweries, and BensofJura are unexpected on this plot

# Extended EDA (Data Diagnostics):

hills.out<-lm(Time~Distance+Climb,data=hills)

# assess normality assumption
# (concerned about shape, outliers)

# compute R-studentized residuals
hills.R<-rstudent(hills.out)

subset(hills.R,hills$Race=="CairnTable")
# R-studentized residual for Cairn Table is 0.7146

# graphic
hist(hills.R)

# something prettier
hist(hills.R, freq=FALSE)
my.z<-seq(-3,3,length=100)
lines(my.z,dnorm(my.z,0,1),lty=2,col="royalblue")
# or
plot(density(hills.R))
my.z<-seq(-3,3,length=100)
lines(my.z,dnorm(my.z,0,1),lty=2,col="royalblue")

# Shapiro-Wilk Test of normality
# Ho: dist is normal
# Ha: dist is not normal
shapiro.test(hills.R)
# p-value < 0.001 ... so non-normal ... due to outliers

# hunt for outliers

# Kildcon Hill?
subset(resid(hills.out),hills$Race=="KildconHill")
# this isn't a good way to look at it ... try the R-studentized residual
#   because it has a normal dist
subset(hills.R,hills$Race=="KildconHill")
# we get 0.21 so we conclude that this is not an outlier

# Knock Hill?
subset(hills.R,hills$Race=="KnockHill")
# we get 7.6 so we say this is an outlier

# identify all outliers (will likely filter these from analysis)
subset(hills,abs(hills.R)>3)

# Influential Observations (unusual in the horizontal direction)

# good influential (happy to have something!)

# bad influential (changing the fit elsewhere)

# leverage (weight an observation has in predicting itself)
#   0 (small weight, influence) to 1 (large weight, influence)
hills.leverage<-lm.influence(hills.out)$hat
# Ben Nevis
subset(hills.leverage,hills$Race=="BenNevis")
# leverage for Ben Nevis is 0.1216

# Cook's Distance
hills.cd<-cooks.distance(hills.out)
# CD for Moffat Chase race
subset(hills.cd,hills$Race=="MoffatChase")
# CD for Moffat Chase is 0.0524

# Leverage Rule of Thumb: 2 * # of parameters estimated / # of obs
2 * 3 / 35  # Climb + Distance + y-intercept
# 0.1216 < 0.1714 so Ben Nevis is not influential

# Cook's Distance Rule of Thumb: 4 / (# of obs - # of parameters estimated)
4 / (35-3)
# 0.0524 < 0.125 so Moffat Chase is not influential

# Identify Influential Observations
subset(hills$Race, hills.leverage > 2*length(coef(hills.out))/dim(hills)[1])
subset(hills$Race, hills.cd > 4/(dim(hills)[1]-length(coef(hills.out))))

# Investigate Lairig Ghru race
par(mfrow=c(1,2))
plot(Time~Distance,data=hills)
points(Time~Distance, data=subset(hills,Race=="LairigGhru"),
       col="red",pch=19)
plot(Time~Climb,data=hills)
points(Time~Climb, data=subset(hills,Race=="LairigGhru"),
       col="red",pch=19)
par(mfrow=c(1,1))
# it appears to be bad influential

# EDA for the truly lazy
plot(hills.out)

# filter the dataset to races that our model predicts
#   Knock Hill (think it was misrecorded)
#   Lairig Ghru (bad influential ... short climb for such a long race)
#   Bens of Jura (outlier and bad influential ... 
#      ... terrain is boggy and rocky)

# remove the above points from our dataset
hills1<-hills[c(-18,-11,-7),]



# Analysis

# Response Variable: Time
# Explanatory Variables: Distance and Climb

# Model: Time = beta0 + (beta1 * Distance) + (beta2 * Climb) + epsilon,
#         where epsilon ~ N(0, sigma2)
# model requires additive data, constant mean

# fit model
hills.out<-lm(Time~Distance+Climb, data=hills1)
# table of estimates and std errors
summary(hills.out)

# Interpret beta1hat = 6.8377
#   Distance effect on Time holding Climb constant
#   for a one unit (km?) increase in Distance we estimate an expected increase
#     of 6.8377 minutes in race time holding Climb constant
# Interpret beta2hat = 0.007694
#   Climb effect on Time holding Distance constant
#   for a one unit increase in Climb we estimate an expected increase
#     of 0.007694 minutes in race time holding Distance constant

# Ho: no Distance or Climb effect <===> Ho: Beta1 = Beta2 = 0
#   Ha: Distance or Climb effect <===> Ha: at least one Betaj != 0
# ANOVA F-test
# F-statistic: 544.1 on 2 and 29 DF,  p-value: < 2.2e-16
# conclude there is a Distance and/or Climb effect,
#   either just Distance, just Climb, or both

# Distance:   t-value = 14.647, p-value = 6.20e-15
# conclude there is a statistically significant effect of Distance on Time
# Climb:   t-value = 5.374, p-value = 8.97e-06
# conclude there is a statistically significant effect of Climb on Time

# 95% confidence intervals
confint(hills.out)[-1,]
#               2.5 %     97.5 %
# Distance 5.882856894 7.79244834
# Climb    0.004766207 0.01062255
# neither CI includes 0, so we reject Ho. Both have a significant effect.



# Analysis Review

# Research Task: Inference on the phenomenon
# Research Question: Do Distance and Climb have a statistically significant
#   effect on Time in Scottish Hill Races?  **Inference**

# Data Features:
# Not-too-tall & Skinny
# continuous response variable
# more than one continuous explanatory variable
# rows of observations (either randomized experiment or observational study)

# Strengths:
# model with interpretable estimates of model elements
# graphics that support inference conclusions
# statistical significance of effect of explanatory variables on the 
#   response variable
# estimate of effect with confidence interval
# statistical significance of each explanatory variable effect on response var

# Weaknesses:
# explanatory variables must be continuous
# model assumptions must be satisfied for both variables: additive model 
#   (straight line), constant variation after adjusting for effect of variable, 
#   outcome of one observation doesn't effect outcome of another observation, 
#   variation is normally distributed

# Challenge:
# I would model the effect of age and salary on a MLB baseball player's batting
#   average. I believe that both of these may have an effect.
#   (for example, players may perform better when getting paid more)
# This data is available here: http://m.mlb.com/stats




