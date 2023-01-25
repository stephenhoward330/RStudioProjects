
# Silverman's Motorcycle Data in MASS R-library as mcycle
# accel = head acceleration in a simulated motorcycle accident (g)
# times = time after impact (millisconds)
library(MASS)
head(mcycle)

# plot data (for our own use)
plot(accel~times,data=mcycle)

# polynomial approximation
out1<-lm(accel~times+I(times^2)+I(times^3)+I(times^4)+I(times^4)+I(times^5),
         data=mcycle, x=TRUE)

# notice the X matrix is
head(out1$x)

# judge the fit from plot
xstar<-seq(0,60,length=100)
yhat1<-predict(out1,newdata=data.frame(times=xstar))

lines(yhat1~xstar,col="red")

# median absolute prediction error
median(abs(resid(out1)))

# Test Ho: no wiggly effect
# if Ho is true then line would be good
out1.red<-lm(accel~times,data=mcycle)
# compute ANOVA F-test statistic
anova(out1.red,out1)
# reject Ho and conclude it is wiggly

# but we need to be careful with the I(x^p) approach becuase of the
#   collinearity problem
plot(~times+I(times^2)+I(times^3)+I(times^4)+I(times^4)+I(times^5),
     data=mcycle)

# a better way to fit a polynomial is use "orthonormal polynomial basis functions"
out2<-lm(accel~poly(times,5),data=mcycle,x=TRUE)

head(out2$x)

plot(~out2$x[,2]+out2$x[,3]+out2$x[,4]+out2$x[,5]+out2$x[,6])

round(t(out2$x)%*%out2$x,3)

# judge the fit from plot
xstar<-seq(0,60,length=100)
yhat2<-predict(out2,newdata=data.frame(times=xstar))

plot(accel~times,data=mcycle)
lines(yhat2~xstar,col="red")

# median absolute prediction error
median(abs(resid(out2)))


# a simple approximation is using piecewise linear effects at the changes
# (hockey stick)

out3<-lm(accel~times + I((times>=14)*(times-14))
         + I((times>=21)*(times-21))
         + I((times>=30)*(times-30))
         + I((times>=40)*(times-40)), data=mcycle,x=TRUE)
head(out3$x)

# judge the fit from plot
plot(accel~times,data=mcycle)

xstar<-seq(0,60,length=100)
yhat3<-predict(out3,newdata=data.frame(times=xstar))
lines(yhat3~xstar,col="forestgreen")

median(abs(resid(out3)))


# natural splines, cubic with continuous 1st & 2nd deriv at knots 
#                  linear beyond boundary
library(splines)

out4<-lm(accel~ns(times,10),data=mcycle) # 10 is degs of freedom

# judge the fit from plot
#plot(accel~times,data=mcycle)
plot(accel~times,data=mcycle,xlim=c(-5,65))

#xstar<-seq(0,60,length=100)
xstar<-seq(-5,65,length=100)
yhat4<-predict(out4,newdata=data.frame(times=xstar))
lines(yhat4~xstar,col="royalblue")

median(abs(resid(out4)))



# 1
hay<-read.table(header=TRUE,text="
                Yield Handlines 
                4 3
                1 2
                8 5
                3 3
                3 1
                5 4
                6 5
                3 2
                2 1
                3 4
                ")
# a
hay.out<-lm(Yield~Handlines+I(Handlines^2),data=hay,x=TRUE)
hay.out$x
summary(hay.out)

# b
plot(Yield~Handlines,data=hay,main="Plot with SLR line and curved line")
xstar<-seq(1,5,length=100)
yhat1<-predict(hay.out,newdata=data.frame(Handlines=xstar))
lines(yhat1~xstar,col="red")

hay.out.red<-lm(Yield~Handlines,data=hay,x=TRUE)
yhat1.red<-predict(hay.out.red,newdata=data.frame(Handlines=xstar))
lines(yhat1.red~xstar,col="blue")

# c
# Test Ho: no curvature effect
# if Ho is true then line would be good
# compute ANOVA F-test statistic
anova(hay.out.red,hay.out)
# fail to reject Ho


# 2
fox<-read.table(file="http://grimshawville.byu.edu/NFLinSLC.txt",header=TRUE,sep=",")
plot(Audience~TotalPoints,data=fox)

# a
fox.out1<-lm(Audience~TotalPoints+I(TotalPoints^2)+I(TotalPoints^3)+I(TotalPoints^4)+I(TotalPoints^4)
             +I(TotalPoints^5)+I(TotalPoints^6),data=fox,x=TRUE)
# median absolute prediction error
median(abs(resid(fox.out1)))

# b
fox.out2<-lm(Audience~TotalPoints + I((TotalPoints>=42.5)*(TotalPoints-42.5)),data=fox,x=TRUE)
# median absolute prediction error
median(abs(resid(fox.out2)))

# c
plot(Audience~TotalPoints,data=fox,main="Plot with 6th Order Line and Hockey Line")

xstar<-seq(30,60,length=100)
yhat1<-predict(fox.out1,newdata=data.frame(TotalPoints=xstar))
lines(yhat1~xstar,col="red")
xstar<-seq(30,60,length=100)
yhat2<-predict(fox.out2,newdata=data.frame(TotalPoints=xstar))
lines(yhat2~xstar,col="black")







