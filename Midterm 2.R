
# Midterm 2

# Data of All-Star Game TV Viewership in Detroit

# Data from handout
baseball<-read.table(header=TRUE, sep="", stringsAsFactors=FALSE, text="
                     Year TVaud Tigers AtBreak DNP Bullpen
                     2004 235.6 2 0.483 1 10
                     2005 436.6 1 0.488 4 10
                     2006 290.9 3 0.670 7 10
                     2007 305.1 5 0.605 2 10
                     2008 223.3 1 0.500 4 11
                     2009 302.1 5 0.552 19 13
                     2010 260.8 3 0.558 10 16
                     2011 201.9 5 0.533 15 13
                     2012 296.2 3 0.512 6 9
                     2013 374.7 6 0.553 8 14
                     2014 279.9 4 0.582 17 12
                     2015 190.4 4 0.500 8 12
                     2016 122.1 1 0.517 11 14
                     2017 111.2 2 0.448 7 11
                     2018 87.3 1 0.418 9 14
                     ") # Note: TVaud is in 1,000 households

# means, standard deviations, and correlations with TVaud
mean(baseball$TVaud)
mean(baseball$Tigers)
mean(baseball$AtBreak)
mean(baseball$DNP)
mean(baseball$Bullpen)

sd(baseball$TVaud)
sd(baseball$Tigers)
sd(baseball$AtBreak)
sd(baseball$DNP)
sd(baseball$Bullpen)

cor(baseball$TVaud,baseball$TVaud)
cor(baseball$Tigers,baseball$TVaud)
cor(baseball$AtBreak,baseball$TVaud)
cor(baseball$DNP,baseball$TVaud)
cor(baseball$Bullpen,baseball$TVaud)

# scatterplot of the # of tigers and TV audience
plot(baseball$Tigers,baseball$TVaud,
     xlab="# of Tigers Players in All-Star Game",
     ylab="TV Viewership (per 1000 households)",
     main="TV Audience in Relation to Number of Tiger Players in All-Star Game")

# remove 2005 from the dataset since it is an outlier
#   (Detroit hosted the ASG that year)
baseball<-subset(baseball, Year != 2005)


# create the linear regression model with 4 explanatory variables
baseball.out<-lm(TVaud~Tigers+AtBreak+DNP+Bullpen,data=baseball)

# report parameter esimates and standard errors
summary(baseball.out)

# Interpret estimates for Tigers: For each additional Tigers all star, we 
#   estimate an increase of 27.925 households per 1000 watching the all star game
#   holding all other variables constant
confint(baseball.out)
# CI: (2.882317, 52.968367)

# graphic demonstrating the uncertainty of above estimate
library(ggplot2)
qplot(x=Tigers,y=TVaud,data=baseball,
      geom="smooth",formula=y~x,method="lm",se=TRUE,
      xlab="# of Tigers in All-Star Game",
      ylab="TV Audience per 1000 Households",
      main="Estimate and Uncertainty of the Effect of # of Tigers in All-Star Game and Detroit TV Viewership")


# Interpret estimates for Bullpen: For each additional pitcher in the bullpen, 
#   we estimate a decrease of 1.902 households per 1000 watching the all star 
#   game, holding all other variables constant
confint(baseball.out)
# CI: (-22.273974, 18.470407)

# graphic demonstrating the uncertainty of above estimate
library(ggplot2)
qplot(x=Bullpen,y=TVaud,data=baseball,
      geom="smooth",formula=y~x,method="lm",se=TRUE,
      xlab="# of Pitchers in Bullpen in All-Star Game",
      ylab="TV Audience per 1000 Households",
      main="Estimate and Uncertainty of the Effect of Size of Bullpen in All-Star Game and Detroit TV Viewership")


# prediction for successful team (5 all-stars, 0.6 winning proportion)
predict(baseball.out,newdata=data.frame(Tigers=5, AtBreak=0.6, DNP=7, 
                                        Bullpen=11),interval = "confidence")
# prediction for tanking team (1 all-star, 0.4 winning proportion)
predict(baseball.out,newdata=data.frame(Tigers=1, AtBreak=0.4, DNP=7, 
                                        Bullpen=11),interval = "confidence")



