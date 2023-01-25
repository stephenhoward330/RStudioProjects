
# Midterm 1

# DATA:

# webscraper for Q1 data 
# from "https://www.boxofficemojo.com/quarter/q1/?grossesOption=calendarGrosses 

# data from webpage 
install.packages("rvest")
library(rvest) 
# Q1 box office url 
i<-1
q1boxoffice.url<-paste(paste("https://www.boxofficemojo.com/quarter/q",i,sep=""),"/?grossesOption=calendarGrosses",sep="") 
# read webpage and store in memory 
q1boxoffice.webpage<-read_html(q1boxoffice.url) 
# create R dataset from webpage contents (first table has the info we want) 
q1boxoffice<-html_table(q4boxoffice.webpage)[[1]] 
# keep only year and gross 
q1boxoffice<-q1boxoffice[,1:2] 
# add Qtr
q1boxoffice$Qtr<-i 
# convert "accounting number" (form $xxx,xxx,xxx.xx) to R number 
q1boxoffice$Gross<-as.numeric(gsub(",", "", gsub("[:$:]", "", q1boxoffice[,"Cumulative Gross"] ) ) ) 
# convert to billions 
q1boxoffice$Gross<-q1boxoffice$Gross/10^9 
# remove "accounting number" column 
q1boxoffice<-q1boxoffice[,-2]

# Q2 box office url 
i<-2
q2boxoffice.url<-paste(paste("https://www.boxofficemojo.com/quarter/q",i,sep=""),"/?grossesOption=calendarGrosses",sep="") 
# read webpage and store in memory 
q2boxoffice.webpage<-read_html(q2boxoffice.url) 
# create R dataset from webpage contents (first table has the info we want) 
q2boxoffice<-html_table(q2boxoffice.webpage)[[1]] 
# keep only year and gross 
q2boxoffice<-q2boxoffice[,1:2] 
# add Qtr
q2boxoffice$Qtr<-i 
# convert "accounting number" (form $xxx,xxx,xxx.xx) to R number 
q2boxoffice$Gross<-as.numeric(gsub(",", "", gsub("[:$:]", "", q2boxoffice[,"Cumulative Gross"] ) ) ) 
# convert to billions 
q2boxoffice$Gross<-q2boxoffice$Gross/10^9 
# remove "accounting number" column 
q2boxoffice<-q2boxoffice[,-2]

# Q3 box office url 
i<-3
q3boxoffice.url<-paste(paste("https://www.boxofficemojo.com/quarter/q",i,sep=""),"/?grossesOption=calendarGrosses",sep="") 
# read webpage and store in memory 
q3boxoffice.webpage<-read_html(q3boxoffice.url) 
# create R dataset from webpage contents (first table has the info we want) 
q3boxoffice<-html_table(q3boxoffice.webpage)[[1]] 
# keep only year and gross 
q3boxoffice<-q3boxoffice[,1:2] 
# add Qtr
q3boxoffice$Qtr<-i 
# convert "accounting number" (form $xxx,xxx,xxx.xx) to R number 
q3boxoffice$Gross<-as.numeric(gsub(",", "", gsub("[:$:]", "", q3boxoffice[,"Cumulative Gross"] ) ) ) 
# convert to billions 
q3boxoffice$Gross<-q3boxoffice$Gross/10^9 
# remove "accounting number" column 
q3boxoffice<-q3boxoffice[,-2]

# Q4 box office url 
i<-4
q4boxoffice.url<-paste(paste("https://www.boxofficemojo.com/quarter/q",i,sep=""),"/?grossesOption=calendarGrosses",sep="") 
# read webpage and store in memory 
q4boxoffice.webpage<-read_html(q4boxoffice.url) 
# create R dataset from webpage contents (first table has the info we want) 
q4boxoffice<-html_table(q4boxoffice.webpage)[[1]] 
# keep only year and gross 
q4boxoffice<-q4boxoffice[,1:2] 
# add Qtr
q4boxoffice$Qtr<-i 
# convert "accounting number" (form $xxx,xxx,xxx.xx) to R number 
q4boxoffice$Gross<-as.numeric(gsub(",", "", gsub("[:$:]", "", q4boxoffice[,"Cumulative Gross"] ) ) ) 
# convert to billions 
q4boxoffice$Gross<-q4boxoffice$Gross/10^9 
# remove "accounting number" column 
q4boxoffice<-q4boxoffice[,-2]

# put the 4 quarter dataframes together
boxoffice<-rbind(q1boxoffice,q2boxoffice,q3boxoffice,q4boxoffice)

# to sort the dataframe
boxoffice<-boxoffice[order(boxoffice$Year,boxoffice$Qtr),]

# Remove the 2020 observation
boxoffice<-boxoffice[-180,]



# EDA:

# Plot by year and quarter
plot(boxoffice$Year+.25*boxoffice$Qtr,boxoffice$Gross, # type="b",
     main="Yearly Box Office Gross Revenue",ylab="Box Office Gross (in $ billions)",xlab="Year")

# We'll cut out all years before 1978 since they don't have data for each quarter
boxoffice1978<-subset(boxoffice,Year>1977)

boxoffice2000<-subset(boxoffice,Year>1999)

# to see the monthly pattern more clearly
plot(1:4,boxoffice2000$Gross[1:4],ylim=c(0.0,4.0),xlim=c(1,4),type="b",
     main="Quarterly Box Office Gross Revenue",
     ylab="Box Office Gross (in $ billions)",xlab="Quarter")
for(i in 1:20){
  lines(1:4,boxoffice2000$Gross[4*i + (1:4)],type="b")
}

# get a column for gross in millions
boxoffice1978$Millions<-boxoffice1978$Gross * 10^3



# Analysis:

# Model: ARIMA(1,1,1) x (1,1,1)_4

library(astsa)
boxoffice.out<-sarima(boxoffice1978$Millions, 1,1,1, 1,1,1, 4)

# report parameter estimates and standard errors
boxoffice.out$ttable

# Predict next 3 years
future.quarter<-c(rep(1:4,3))
future.year<-c(rep(2020:2022,each=4))

# predictons
boxoffice.future<-sarima.for(boxoffice1978$Millions, 1,1,1, 1,1,1, 4, 
                             n.ahead=length(future.year))

# compute 95% prediction intervals
L=boxoffice.future$pred - 2*boxoffice.future$se
U=boxoffice.future$pred + 2*boxoffice.future$se

# table of predictions
cbind(future.quarter,future.year,boxoffice.future$pred,L,U)

# get indeces for the quarters
boxoffice1978$index<-1:168

# create a publication quality graphic
plot(boxoffice1978$index,boxoffice1978$Millions,type="b",
     main="Yearly Box Office Gross Revenue (with Predictions)",
     ylab="Box Office Gross (in $ millions)",xlab="Quarter",
     xlim=c(100,180),ylim=c(2000,3750))
lines(169:180, boxoffice.future$pred, type="b", pch=19, col="darkorange2")
lines(169:180, L, lty=2, col="darkorange2")
lines(169:180, U, lty=2, col="darkorange2")



