
# Class Participation Simple Linear Regression 2

# Diamonds

# Data:

# Data at http://www.amstat.org/publications/jse/v9n2/4Cdata.txt
diamonds<-read.table(header=FALSE,
                  file="http://www.amstat.org/publications/jse/v9n2/4Cdata.txt")
# Add headers to columns
names(diamonds)<-c("Carat","Color","Clarity","Cert","Price")



# EDA:

# Plot Carat to Price
plot(Price~Carat,data=diamonds,
     main="Diamond Scatter Plot",
     xlab="Diamond Size (Carat)",
     ylab="Diamond Price (Singapore $)")

# Plot ln(Carat) to ln(Price)
plot(x=log(diamonds$Carat), y=log(diamonds$Price),
     main="Diamond Scatter Plot (ln)",
     xlab="ln Diamond Size (Carat)",
     ylab="ln Diamond Price (Singapore $)")



# Analysis:

# response variable: log diamond price ($Singapore in 2000)
# explanatory variable: log diamond carat

# model: log diamond price = beta0 + beta1 * log diamond carat + epsilon
#         where epsilon ~ N( 0, sigma^2)

# fit model
diamonds.out<-lm(log(Price)~log(Carat),data=diamonds)

# table of estimates and standard errors
summary(diamonds.out)
# How do we interpret beta1 in a log-transformed model?
# for a 1% increase in carat we estimate an expected 1.53% increase in price

# does diamond size have a statistically significant effect on price?
# t-test
summary(diamonds.out)
# ANOVA F-test
anova(diamonds.out)

# 95% confidence interval on beta1
#   Ho: no inheritance effect <====> Ho: beta1=0
confint(diamonds.out)
# 95% Confidence Interval for beta1 is (1.501, 1.574) in
# formal conclusion: since 0 is not in the interval, we would reject
#   Ho: beta1=0 in favor of Ha: beta1 not equal 0 at the 0.05 significance level
# informal conlusion: There is a statistically significant effect

# Estimated model for estimated price
#   Estimated Price = e ^ (9.13 + 1.54 * ln(Carat))

# create a graphic demonstrating the uncertainty associated with estimated model
library(ggplot2)
qplot(x=log(Carat),y=log(Price),data=diamonds,
      geom="smooth",formula=y~x,method="lm",se=TRUE,
      xlab="Diamond Size (Carat)",
      ylab="Diamond Price (Singapore $)")

# a newly engaged couple is shopping for a one carat diamond
#   predict the price
# predict log Price (Singapore $)
predict(diamonds.out,newdata=data.frame(Carat=1), interval="prediction")
# predict Price (Singapore $)
exp(predict(diamonds.out,newdata=data.frame(Carat=1), interval="prediction"))
#        fit      lwr      upr
#     9207.307 6602.699 12839.37

# graphic of uncertainty associated with predictions
plot.df<-cbind(diamonds, exp(predict(diamonds.out,interval="prediction")))
ggplot(plot.df, aes(Carat,Price)) + 
        xlab("Diamond Size (Carat)") +
        ylab("Diamond Price (Singapore $ in 2000)") +
        geom_point() +
        geom_line(aes(y=fit),color="royalblue") +
        geom_line(aes(y=lwr),color="red",linetype="dashed") +
        geom_line(aes(y=upr),color="red",linetype="dashed")

# R-squared using ln Price
summary(diamonds.out)
# 0.9574
# 95.74% of the variation in ln Price is accounted for 
#   using a model with ln Carat

# R-squared using Price in the log-transformed model
1-sum((diamonds$Price-exp(predict(diamonds.out)))^2)/sum((diamonds$Price-mean(diamonds$Price))^2)
# 0.9053

# what if we didn't transform?
diamonds.out.untransformed<-lm(Price~Carat, data=diamonds)
summary(diamonds.out.untransformed)

# Summarize Prediction Performance
summary(abs(diamonds$Price-exp(predict(diamonds.out))))

# graph to demonstrate the nature of prediction errors
plot(abs(diamonds$Price-exp(predict(diamonds.out)))~exp(predict(diamonds.out)))
# predicts very well for small diamonds, worse for bigger diamonds
# bigger diamonds have other features that affect the price, like cut or color



# Analysis Review:

# Research Task: Inference on the Phenomenon

# Data Features: 
# Two columns (one a continuous response variable and one a 
#   continuous explanatory variable) and rows of observations 
#   (either a randomized experiment or an observational study)
# Requires a log transform

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
# We use a log transformation to match the model assumptions

# Challenge:
# I would see if there is a statistically significant effect on free throw
#   percentage from minutes played in an NBA game. In other words, if being
#   in the game longer will improve your free throw shooting. 
#   We assume Ho: that there is no effect.
# Data would have to be a random sample, data can be found here:
#       https://stats.nba.com/
# This data may require a log transformation




