
# Class Participation Regression 1

# Climate Change



# DATA:

# global climate (1880 - 2018)
nasatemp <- readLines('http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt', warn=FALSE)[-c(1:7, 160:167)]
nasatemp<-read.table(textConnection(nasatemp),header=TRUE,blank.lines.skip=TRUE,stringsAsFactors=FALSE,na.strings="****")
nasatemp<-nasatemp[!nasatemp$Year=="Year",]
# code to reshape to each row is a month
nasatemp<-data.matrix(nasatemp[,1:13])

globaltemp<-data.frame(temp=matrix(t(nasatemp[,2:13]),ncol=1))
# add month and year
#dim(nasatemp)
globaltemp$month<-rep(1:12,dim(nasatemp)[1])
globaltemp$year<-rep(min(nasatemp[,1]):max(nasatemp[,1]),each=12)

#CO2
CO2<-read.table("ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/co2_mlo_surface-flask_1_ccgg_month.txt")
colnames(CO2)<-c('site','year','month','co2') #CO2 measured in ppm
#head(CO2)

# CH4
CH4<-read.table("ftp://aftp.cmdl.noaa.gov/data/trace_gases/ch4/flask/surface/ch4_mlo_surface-flask_1_ccgg_month.txt")
colnames(CH4)<-c('site','year','month','methane') #CH4 measured in ppb

# merge datasets and clean up
climate<-merge(merge(globaltemp,CO2,by=c("year","month"),all.x=TRUE),CH4,by=c("year","month"),all.x=TRUE)
climate<-subset(climate,!is.na(site.x) & !is.na(site.y) )
climate<-climate[,c("year","month","temp","co2","methane")]



# EDA:

# correlation coefficients
cor(climate$temp,climate$co2)
cor(climate$temp,climate$methane)

# Scatter plots
plot(temp~co2, data=climate,
     main="Scatter Plot of Temp and CO2",
     xlab="Atmosphere Carbon Dioxide (ppm)",
     ylab="Global Surface Temperature Anamoly (0.01 degrees C)")
plot(temp~methane, data=climate,
     main="Scatter Plot of Temp and Methane",
     xlab="Atmosphere Methane (ppb)",
     ylab="Global Surface Temperature Anamoly (0.01 degrees C)")



# Analysis

# response variable: global temperature (0.01 degrees Celsius)
# explanatory variables: greenhouse gases (CO2 and CH4)

# Model: temp = beta0 + (beta1 * CO2) + (beta2 * CH4) + epsilon,
#         where epsilon ~ N(0, sigma2)

# fit model
climate.out<-lm(temp~co2+methane, data=climate)
# table of estimates and std errors
summary(climate.out)

# Interpret beta1hat = 0.7365
#   CO2 effect on global temp holding methane constant
#   for a one ppm increase in CO2 we estimate an expected increase
#     of 0.74 (0.01 C) in global temp holding methane constant
# Interpret beta2hat = 0.0975
#   methane effect on global temp holding CO2 constant
#   for a one ppb increase in methane we estimate an expected increase
#     of 0.10 (0.01 C) in global temp holding CO2 constant
# We call this a "partial slope" when we hold other variables constant

# graphics to demonstrate the regression coefficients
#install.packages("car")
library(car)
# partial regression plot
avPlots(climate.out)
# component plus residual plot
crPlots(climate.out)

# Ho: no greenhouse gas effect <===> Ho: Beta1 = Beta2 = 0
#   Ha: greenhouse gas effect <===> Ha: at least one Betaj != 0
# ANOVA F-test
# F-statistic: 427.5 on 2 and 425 DF,  p-value: < 2.2e-16
# conclude there is a greenhouse gas effect,
#   either just co2, just methane, or both co2 and methane

# co2:   t-value = 7.745, p-value = 7.07e-14
# conclude there is a statistically significant effect of co2 on temp
# ch4:   t-value = 3.045, p-value = 0.00247
# conclude there is a statistically significant effect of ch4 on temp

# 95% confidence intervals
confint(climate.out)[-1,]
#              2.5 %    97.5 %
#  co2     0.54961265 0.9234529
#  methane 0.03455957 0.1604108
# neither CI includes 0, so we reject Ho. Both have a significant effect.

# prediction validation
# create train (up to 2017) & test (2018)
climate.train<-subset(climate,year<=2017)
climate.test<-subset(climate,year==2018)

# fit model to train
climate.train.out<-lm(temp~co2+methane, data=climate.train)
# predict for 2018
cbind(predict(climate.train.out,newdata=climate.test),climate.test$temp)
# plot of prediction and actual
plot(cbind(predict(climate.train.out,newdata=climate.test),climate.test$temp))
abline(0,1,col="gray")



# Analysis Review

# Research Task: Inference on the phenomenon
# Research Question: Do greenhouse gases have a statistically significant effect
#   on global temperature?  **Inference**
#   (also want prediction performance to demonstrate practical significance)

# Data Features:
# Tall & Skinny
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
# prediction with prediction interval
# validation of future prediction

# Weaknesses:
# explanatory variables must be continuous
# model assumptions must be satisfied for both variables: additive model 
#   (straight line), constant variation after adjusting for effect of variable, 
#   outcome of one observation doesn't effect outcome of another observation, 
#   variation is normally distributed

# Challenge:
# I would model the effect of height and weight on a person's vertical leap
# I would have to run an experiment and make measurements to get this data
# This dataset would have one continuous response variable (vertical leap),
#   and two continuous explanatory variables (height and weight)




