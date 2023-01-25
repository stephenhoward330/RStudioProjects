
# Class Participation Time Series 2

# DATA:

# US Residential Energy Consumption

# http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption
data1<-read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")

# subset to TERCBUS Total Energy Consumed by the Residential Sector
data2<-subset(data1,MSN=="TERCBUS")

# subset to "your lifetime"
data3<-subset(data2,data2$YYYYMM>199100)

# remove yearly total (coded "month 13", every 13th obs)
data4<-subset(data3,data3$YYYYMM%%100 != 13)

# we're only interested in the value column
energy<-data4$Value
T<-length(energy)


# EDA

# Notice that in addition to a slight increasing trend, there
# is also a seasonal trend (12 month cycle)
plot(1:T,energy,type="b",main="US Residential Energy Consumption",
     ylab="US Resid. Ener. Cons. (trillion Btu)",xlab="Month")

# to see the monthly pattern more clearly
plot(1:12,energy[1:12],ylim=c(1000,3000),type="b",
     main="US Residential Energy Consumption (by Month)",
     ylab="US Resid. Ener. Cons. (trillion Btu)",xlab="Month")
for(i in 1:27){
  lines(1:12,energy[12*i + (1:12)],type="b")
}


# Analysis

# Model: ARIMA(1,1,1) x (1,1,1)_12

library(astsa)
energy.out<-sarima(energy, 1,1,1, 1,1,1, 12)

energy.out$ttable

# Predict next 2 years
future.month<-c(10,11,12,rep(1:12,2))
future.year<-c(2019,2019,2019,rep(2020:2021,each=12))

# predictons
energy.future<-sarima.for(energy, 1,1,1, 1,1,1, 12, n.ahead=length(future.year))

# compute 95% prediction intervals
L=energy.future$pred - 2*energy.future$se
U=energy.future$pred + 2*energy.future$se

# table of predictions
cbind(future.month,future.year,energy.future$pred,L,U)


# Analysis Review

# Research Task: Prediction

# Data Features:
# Seasonal Time-Series (monthly data)
# Slightly yearly and seasonal Trend

# Strengths:
# More data (more frequent)
# Incorporate seasonal pattern
# Predict with prediction intervals
# Prediction matches intuition
# Long & Short Memory

# Weaknesses:
# We form seasonality
# "Overall trend" is really "every month has a trend"
# No idea "why"
# Assumes past predicts future


# Challenge:
# I would forecast the number of car crashes that will happen per month next year
# Some data can be found here: https://www.statista.com/statistics/193018/number-of-us-crashes-by-month/





