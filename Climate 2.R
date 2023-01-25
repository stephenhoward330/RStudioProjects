
# Class Participation Regression 3

# Climate Change 2



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
CO2<-read.table("ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/co2_mlo_surface-flask_1_ccgg_month.txt",skip=70)
colnames(CO2)<-c('site','year','month','co2') #CO2 measured in ppm
#head(CO2)

# CH4
CH4<-read.table("ftp://aftp.cmdl.noaa.gov/data/trace_gases/ch4/flask/surface/ch4_mlo_surface-flask_1_ccgg_month.txt",skip=70)
colnames(CH4)<-c('site','year','month','methane') #CH4 measured in ppb

# N2O
N2O<-read.table("ftp://aftp.cmdl.noaa.gov/data/hats/n2o/insituGCs/CATS/monthly/mlo_N2O_MM.dat", skip=50)
colnames(N2O)<-c('year','month','n2o','ignore1','ignore2')
N2O<-N2O[,1:3]

# HcFC
HCFC<-read.table("ftp://aftp.cmdl.noaa.gov/data/hats/hcfcs/hcfc142b/insituGCs/CATS/monthly/mlo_HCFC142b_MM.dat", skip=50)
colnames(HCFC)<-c('year','month','hcfc','ignore1','ignore2')
HCFC<-HCFC[,1:3]

# SF6
SF6<-read.table("ftp://aftp.cmdl.noaa.gov/data/hats/sf6/insituGCs/CATS/monthly/mlo_SF6_MM.dat", skip=50)
colnames(SF6)<-c('year','month','sf6','ignore1','ignore2')
SF6<-SF6[,1:3]

# merge additional all datasets and clean up
climate<-merge(merge(globaltemp,CO2,by=c("year","month"),all.x=TRUE),CH4,by=c("year","month"),all.x=TRUE) 
climate<-subset(climate,!is.na(site.x) & !is.na(site.y) )
climate<-climate[,c("year","month","temp","co2","methane")] 
climate<-merge(climate,N2O,by=c("year","month"),all.x=TRUE) 
climate<-subset(climate,!is.na(n2o))
climate<-merge(climate,HCFC,by=c("year","month"),all.x=TRUE) 
climate<-subset(climate,!is.na(hcfc)) 
climate<-merge(climate,SF6,by=c("year","month"),all.x=TRUE) 
climate<-subset(climate,!is.na(sf6)) 
# with so many observations removed for missing data, renumber the rows 
rownames(climate)<-1:dim(climate)[1]



# EDA:

# correlation coefficients for all explanatory variables to temp
cor(climate$temp,climate[,4:8])

# Extended EDA

climate.out<-lm(temp~co2+methane+n2o+hcfc+sf6,data=climate)

# studentized-residuals
climate.R<-rstudent(climate.out)

# test normalness
shapiro.test(climate.R)  # p-value 0.027

# identify all outliers
subset(climate.R,abs(climate.R)>3)  # 76 and 182



# Analysis

# Response Variable: Global Temperature (in 0.01 degree Celsius)
# Explanatory Variables: Greenhouse Gases
#   CO2 (in ppm)
#   Methane (in ppb)
#   N2O (in ppb)
#   HCFC (in ppt)
#   SF6 (in ppt)

# Model:
#   temp = beta0 + beta1 * CO2 + beta2 * Methane + beta3 * N2O 
#     + beta4 * HCFC + beta5 * SF6 + epsilon where epsilon ~ N(0, sigma2)
climate.out<-lm(temp~co2+methane+n2o+hcfc+sf6,data=climate)
# table of estimates and std errors
summary(climate.out)
# co2, hcfc, and sf6 all have negative regression coefficients...

# notice the consequences of collinearity
# all the greenhouse gases go up with one another

# 1. some of the estimates have opposite sign from what we expected 
#       from science & plots
# 2. standard errors are inflated from what we would see in a designed experiment
# 3. lack of significance for obvious practically significant effects

# plot the explanatory variables against one another
plot(~co2+methane+n2o+hcfc+sf6,data=climate)

# identify all outliers
subset(climate.R,abs(climate.R)>3)  # 76 and 182

# Identify Influential Observations


# what do we want?

# If prediction performance is most important ...
# ... interpretation suffers.
# best model uses all the explanatory variables
climate.out<-lm(temp~co2+methane+n2o+hcfc+sf6,data=climate)
# table of estimates and std errors
summary(climate.out)
# explain the non-sensical coefficients
library(car)
avPlots(climate.out)

# If interpretation is most important ...
# ... prediction suffers.
climate.out1<-lm(temp~co2+methane+sf6,data=climate)  # throw out n2o and hcfc
# table of estimates and std errors
summary(climate.out1)
# Note: this is the result of eliminating n2o and hcfc explanatory variables
#       in order to achieve a model whose effects have the expected interpretation
#       where increasing levels of the greenhouse gas have an average
#       increasing effect on the global temperature.



# Analysis Review

# Research Task: understanding or prediction?
#   In our case understanding is most important

# Data Features: 
#   continuous response variable
#   multiple continuous explanatory variables
#   observational study or randomized experiment

# Analysis Strengths:
#   if interpretation is important:
#     model with interpretable elements
#   graphics that support the conclusions
#     (correlations of greenhouse gases show trend)
#   statistical significance
#   if predictions were important:
#     predictions with prediction interval

# Analysis Weaknesses:
#   assumptions are met (validated with data diagnostics)
#   if interpretation is important:
#     we deleted important explanatory variables to achieve interpretability
#   if prediction is important:
#     we used a model that has parts that are nonsensical

# Challenge:
# I would model the effect of age, height, weight and salary on a MLB baseball
#   player's batting average. I believe that both of these may have an effect.
# This data is available here: http://m.mlb.com/stats




