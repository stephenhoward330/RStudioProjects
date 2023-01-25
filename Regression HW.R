
# ch6 5 (like diamonds)

pga<-read.csv(file="http://gattonweb.uky.edu/sheather/book/docs/datasets/pgatour2006.csv")

# a
plot(PrizeMoney~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, data=pga)
plot(log(PrizeMoney)~log(DrivingAccuracy)+log(GIR)+log(PuttingAverage)+log(BirdieConversion)+log(SandSaves)+
       log(Scrambling)+log(PuttsPerRound), data=pga)
plot(log(PrizeMoney)~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, data=pga)


# 5b (like Scottish Hill Races)
pga.out<-lm(log(PrizeMoney)~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, 
            data=pga)
summary(pga.out)

pga.R<-rstudent(pga.out)

hist(pga.R, freq=FALSE)
my.z<-seq(-3,3,length=100)
lines(my.z,dnorm(my.z,0,1),lty=2,col="royalblue")
# or
plot(density(pga.R))
my.z<-seq(-3,3,length=100)
lines(my.z,dnorm(my.z,0,1),lty=2,col="royalblue")


# 5c (outliers... bad influential, good influential... why?)

# identify all outliers (will likely filter these from analysis)
subset(pga,abs(pga.R)>3)

# leverage (weight an observation has in predicting itself)
pga.leverage<-lm.influence(pga.out)$hat

# Cook's Distance
pga.cd<-cooks.distance(pga.out)

# Leverage Rule of Thumb: 2 * # of parameters estimated / # of obs
# Cook's Distance Rule of Thumb: 4 / (# of obs - # of parameters estimated)
# Identify All Influential Observations
subset(pga$Name, pga.leverage > 2*length(coef(pga.out))/dim(pga)[1])
subset(pga$Name, pga.cd > 4/(dim(pga)[1]-length(coef(pga.out))))



# ch5  big interaction ":"
# reduced model, remove both main effect and interaction
bill<-read.table(file="http://gattonweb.uky.edu/sheather/book/docs/datasets/overdue.txt",header=TRUE)
bill$TYPE<-c(rep("Residential",48), rep("Commercial",48))
plot(LATE~BILL,data=bill)
plot(LATE~BILL,data=subset(bill,bill$TYPE=="Residential"),main="Residential Plot")
plot(LATE~BILL,data=subset(bill,bill$TYPE=="Commercial"),main="Commercial Plot")

bill.out<-lm(LATE~BILL + TYPE + BILL:TYPE,data=bill)
residential.out<-lm(LATE~BILL,data=subset(bill,bill$TYPE=="Residential"))
commercial.out<-lm(LATE~BILL,data=subset(bill,bill$TYPE=="Commercial"))
summary(bill.out)
summary(residential.out)
summary(commercial.out)



