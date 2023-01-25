
# Simple Linear Regression HW

# data from playbill.csv
playbill<-read.csv(file="http://gattonweb.uky.edu/sheather/book/docs/datasets/playbill.csv")

# plot data
plot(playbill$LastWeek, playbill$CurrentWeek)

# make the regression model
playbill.out<-lm(CurrentWeek~LastWeek,data=playbill)

# predict new obs if LastWeek=400,000
predict(playbill.out,newdata=data.frame(LastWeek=400000), interval="prediction")


# data from indicators.txt
indicators<-read.table(header=TRUE,file="http://gattonweb.uky.edu/sheather/book/docs/datasets/indicators.txt")

# plot the data
plot(indicators$LoanPaymentsOverdue,indicators$PriceChange)

# make the regression model
indicators.out<-lm(PriceChange~LoanPaymentsOverdue,data=indicators)

# create a graphic demonstrating the uncertainty associated with estimated model
library(ggplot2)
qplot(x=LoanPaymentsOverdue,y=PriceChange,data=indicators,
      geom="smooth",formula=y~x,method="lm",se=TRUE,
      main="Regression Model",
      xlab="% of Loan Payments Overdue",
      ylab="% Change in Price")

# predict new obs if LoanPaymentsOverdue=4
predict(indicators.out,newdata=data.frame(LoanPaymentsOverdue=4), interval="prediction")


# data from invoices.txt
invoices<-read.table(header=TRUE,file="http://gattonweb.uky.edu/sheather/book/docs/datasets/invoices.txt")

# plot the data
plot(invoices$Invoices, invoices$Time)

# make the regression model
invoices.out<-lm(Time~Invoices,data=invoices)

# predict new obs if Invoices=0
predict(invoices.out,newdata=data.frame(Invoices=0), interval="prediction")

# predict new obs if Invoices=130
predict(invoices.out,newdata=data.frame(Invoices=130), interval="prediction")



# data from AdRevenue.csv
revenue<-read.csv(file="http://gattonweb.uky.edu/sheather/book/docs/datasets/AdRevenue.csv")

# plot the data
plot(revenue$Circulation,revenue$AdRevenue,
     xlab="Circulation (in millions)",
     ylab="Revenue (in thousands of dollars)")

# make the regression model
revenue.out<-lm(AdRevenue~Circulation,data=revenue)
# or log regression model
revenue.out.ln<-lm(log(AdRevenue)~log(Circulation),data=revenue)

# predict new obs if Circulation=0.5
predict(revenue.out,newdata=data.frame(Circulation=0.5), interval="prediction")

# predict new obs if Circulation=20
predict(revenue.out,newdata=data.frame(Circulation=20), interval="prediction")


# data from cars04.csv
cars<-read.csv(file="http://gattonweb.uky.edu/sheather/book/docs/datasets/cars04.csv")

# plot data
plot(cars$DealerCost,cars$SuggestedRetailPrice)

# make the regression model
cars.out<-lm(SuggestedRetailPrice~DealerCost,data=cars)
# or log regression model
cars.out.ln<-lm(log(SuggestedRetailPrice)~log(DealerCost),data=cars)

1-sum((cars$SuggestedRetailPrice-exp(predict(cars.out.ln)))^2)/sum((cars$SuggestedRetailPrice-mean(cars$SuggestedRetailPrice))^2)

coef(cars.out.ln)
