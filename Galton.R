
# Class Participation Simple Linear Regression 1

# Galton's Pea Experiment

# Data:

peas<-read.table(header=TRUE,text="
parent offspring
0.21 0.1726
0.20 0.1707
0.19 0.1637
0.18 0.1640
0.17 0.1613
0.16 0.1617
0.15 0.1598
")



# EDA:

# find the correlation coefficient
cor(peas$parent,peas$offspring)  # 0.9249
# Very strong positive correlation that diameter is a trait 
# inherited from parents

# to designate what we hypothesize as the casual relationship,
# parent is on the horizontal axis & offspring is on the vertical axis
plot(offspring~parent,data=peas,
     main="Pea Scatter Plot",
     xlab="Diameter of Parent Pea",
     ylab="Diameter of Offspring Pea")

# better-looking plot
install.packages("ggplot2")
library(ggplot2)
qplot(parent,offspring,data=peas,
      geom="point",
      main="Pea Scatter Plot",
      xlab="Diameter of Parent Pea",
      ylab="Diameter of Offsprint Pea")



# Analysis:

# response variable: Offspring Diameter (in)
# explanatory variable: Parent Diameter (in)

# Model: Offspring Diameter = beta0 + beta1 * Parent Diameter + epsilon
#         where epsilon ~ N( 0, sigma^2)

# practically, this means:
#   additive model (straight line)
#   constant variance (same variance for all X)
#   after adjusting for the effect of X, a normal distribution
#   outcome of one obs has no effect on the outcome of another

# fit/estimate model
peas.out<-lm(offspring~parent, data=peas)
# report estimates and standard errors
summary(peas.out)
# Interpret \hat(\beta)_1 = 0.21
#   the slope
#   how much we estimate the mean offspring diameter to increase
#     if the parent diameter increases by 1 inch
#   the inheritance effect


# Is there a statistically significant inheritance effect?

# t-test
#   Ho: no inheritance effect <====> Ho: beta1=0
# test statistic: 5.438
# p-value: 0.0029 (chance of getting Galton's data without inheritance)
# formal conclusion: We reject Ho in favor of Ha: beta1 does not equal zero
#   at the 0.01 significance level
# informal conlusion: There is a statistically significant inheritance effect

# ANOVA F-test
#   Ho: no inheritance effect <====> Ho: beta1=0
anova(peas.out)
# test statistic: 29.58
# p-value: 00029
# formal conclusion: We reject Ho in favor of Ha: beta1 does not equal zero
#   at the 0.01 significance level
# informal conlusion: There is a statistically significant inheritance effect

# 95% confidence interval on beta1
confint(peas.out)
# 95% Confidence Interval for beta1 is (0.111, 0.309) in
#                                       0.210 +/- 0.099 in
# formal conclusion: since 0 is not in the interval, we would reject 
#   Ho: beta1=0 in favor of Ha: beta1 not equal 0 at the 0.05 significance level
# informal conlusion: There is a statistically significant inheritance effect

# There is a statistically significant inheritance effect
#   (95% CI: 0.111, 0.309; p-value=0.0029).


# graphic demonstrating the uncertainty associated with the inheritance effect
library(ggplot2)
qplot(parent,offspring,data=peas,
      geom="smooth",formula=y~x,method="lm",se=TRUE,
      xlab="Diameter of Parent Pea (in)",ylab="Diameter of Offspring Pea (in)")

# 95% CI E(offspring diameter | parent diameter = .20 )
predict(peas.out, newdata=data.frame(parent=0.20), interval="confidence")
#         fit       lwr       upr
#    1 0.1690286 0.1662211 0.1718361

# Suppose we are also interested in prediction
# predicted/estimated model: 
#   predicted offspring diameter = 0.127 + 0.210 * parent diameter

# 95% Prediction Interval for offspring diameter for a parent with 0.18 diameter
predict(peas.out, newdata=data.frame(parent=0.18), interval="prediction")
#   fit       lwr       upr
# 0.1648286 0.1592136 0.1704436

# graphic of uncertainty associated with predictions
plot.df<-cbind(peas, predict(peas.out,interval="prediction"))
ggplot(plot.df, aes(parent,offspring)) + 
  xlab("Diameter of Parent Pea") +
  ylab("Diameter of Offspring Pea") +
  geom_point() +
  geom_line(aes(y=fit),color="royalblue") +
  geom_line(aes(y=lwr),color="red",linetype="dashed") +
  geom_line(aes(y=upr),color="red",linetype="dashed")

summary(peas.out)
# report R^2 = 0.8554
# summarizes prediction performance (larger is better)
# % of variation in Y that is explained by model



# Analysis Review:

# Research Task: Inference on the Phenomenon

# Data Features: 
# Two columns (one a continuous response variable and one a 
#   continuous explanatory variable) and rows of observations 
#   (either a randomized experiment or an observational study)

# Strengths:
# model with interpretable estimates of model elements
# statistical significance of effect of explanatory variable on the 
#   response variable
# estimate of effect with confidence interval
# predictions with prediction interval 
# graphics that support inference conclusions and prediction performance

# Weaknesses:
# only one explanatory variable
# explanatory variable must be continuous
# model assumptions must be satisfied: additive model (straight line), 
#   constant variation after adjusting for effect of X, outcome of 
#   one observation doesn't effect outcome of another observation, 
#   variation is normally distributed
# few data points

# Challenge:
# I would see if there is a statistically significant effect on field goal
#   percentage from minutes played in an NBA game. In other words, if being
#   in the game longer will improve your shooting. We assume Ho: that there
#   is no effect.
# Data would have to be a random sample, data can be found here:
#       https://stats.nba.com/


