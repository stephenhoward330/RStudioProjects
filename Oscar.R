

# Academy Award Best Picture Winners

# Data:
oscar<-read.csv('http://grimshawville.byu.edu/bestpicture.txt',header=TRUE)
names(oscar)<-c("Movie","Win","RT","Rating")  # RT is Rotten Tomatoes score

# create factor
oscar$Rating<-factor(oscar$Rating)
oscar$Rating<-relevel(oscar$Rating," R")

# Model
oscar.out<-glm(Win~RT+Rating,data=oscar,family=binomial)
summary(oscar.out)

coef(oscar.out)

exp(coef(oscar.out)[-1])

exp(confint(oscar.out)[-1,])

# predict Black Panther (2019 nominee)
predict(oscar.out,newdata=data.frame(RT=97,Rating=" PG-13"),type="response")

# predict 2020
tail(predict(oscar.out,type="response"),9)
# Little Women & Marriage Story have the same RT ... so R matters more than one heigher RT











