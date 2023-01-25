
# Risk factors for Cardiovascular Disease
# Data (Framingham Data)
# Data Description
#
# Response Variable: CVD=1 if Myocardial Infarction, Fatal Coronary Heart Disease,
#                             Atherothrombotic Infarction, Cerebral Embolism,
#                             Intracerebral Hemorrhage, or Subarachnoid Hemorrhage
#                             or Fatal Cerebrovascular Disease
#                       =0 otherwise
#
# Explanatory Variables:
# SEX =1 if Men, =2 if Women
# TOTCHOL serum total cholesterol (mg/dL)
# AGE (years)
# SYSBP systolic blood pressure (mmHg)
# DIABP diastolic blood pressure (mmHg)
# CURSMOKE = 1 current smoker, 0 if not current smoker
# CIGPDay number of cigarettes smoked each day
# BMI body mass index (kg/m^2)
# DIABETES =1 if diabetic (glucose>200), 0 if not diabetic
# BPMEDS =1 if currently using anti-hypertensive medication, =0 otherwise
# HEARTRATE ventrical heart rate (beats/min)
# GLUCOSE casual serum glucose (mg/dL)
# educ = 1 if < HS, =2 if HS, =3 if some college, =4 if college

fram<-read.csv("http://grimshawville.byu.edu/frmgham2.csv",header=TRUE)

# subset to participants that didn't have CHD
fram<-subset(fram,PERIOD==1 & PREVCHD==0)

# subset to risk factors under study
fram<-fram[,c(2:14,30)]

# filter missing data (assume missing at random)
fram<-subset(fram,!is.na(TOTCHOL))
fram<-subset(fram,!is.na(CIGPDAY))
fram<-subset(fram,!is.na(BMI))
fram<-subset(fram,!is.na(BPMEDS))
fram<-subset(fram,!is.na(HEARTRTE))
fram<-subset(fram,!is.na(GLUCOSE))
fram<-subset(fram,!is.na(educ))

# easier intepretation of coded variables, make them, educ as factors
fram$Gender<-"Male"
fram$Gender[fram$SEX==2]<-"Female"
fram$Gender<-factor(fram$Gender)
fram$Smoker<-"Yes"
fram$Smoker[fram$CURSMOKE==0]<-"No"
fram$Smoker<-factor(fram$Smoker)
fram$DIAB<-"No"
fram$DIAB[fram$DIABETES==1]<-"Yes"
fram$DIAB<-factor(fram$DIAB)
fram$Meds<-"No"
fram$Meds[fram$BPMEDS==1]<-"Yes"
fram$Meds<-factor(fram$Meds)
fram$educ<-factor(fram$educ,levels=4:1)

# delete original coded variable columns
fram<-fram[,c(2:5,7,8,11:18)]

# create train and test
set.seed(330)
sample.ind<-sample(dim(fram)[1],3000)
fram.train<-fram[sample.ind,]
fram.test<-fram[-sample.ind,]

# EDA
boxplot(HEARTRTE~CVD,data=fram.train)
boxplot(AGE~CVD,data=fram.train)
prop.table(table(fram.train$Smoker,fram.train$CVD),margin=1)

# Stepwise selection

# uses AIC, which combines prediction performance and penalty for complexity

min.model<-glm(CVD ~ +1,data=fram.train,family="binomial")
biggest<-formula( glm(CVD ~ .,data=fram.train,family="binomial") )
fram.out<-step(min.model,direction="both",scope=biggest)
summary(fram.out)


# Training ROC
library(ROCR)
train.pred<-prediction(predict(fram.out,type="response"),fram.train$CVD)
train.perf<-performance(train.pred,measure="tpr",x.measure="fpr")

performance(train.pred,measure="auc")

# Test ROC
test.pred<-prediction(predict(fram.out,type="response",newdata=fram.test),fram.test$CVD)
test.perf<-performance(test.pred,measure="tpr",x.measure="fpr")

performance(test.pred,measure="auc")

# compare ROC for train and test
plot(train.perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
plot(test.perf,add=TRUE,col="royalblue")
abline(0,1,col="gray")






