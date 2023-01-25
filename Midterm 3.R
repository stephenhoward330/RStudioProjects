
# Midterm 3

# start at 11:25
# finish by 2:25
# finished at 1:10

# Read in data
stem<-read.csv("http://grimshawville.byu.edu/STEMswitchers.csv")

# Relevel
stem$Switcher<-as.factor(stem$Switcher)
stem$Gender<-as.factor(stem$Gender)
stem$Major<-as.factor(stem$Major)
stem$PrevCalc<-as.factor(stem$PrevCalc)
stem$Gender<-relevel(stem$Gender, "1")
stem$Major<-relevel(stem$Major, "1")
stem$PrevCalc<-relevel(stem$PrevCalc, "5")

# Fit Model
stem.out<-glm(Switcher~PrevCalc+StandardizedTest+Major+Teacher+ClassPractice+Gender,data=stem, family="binomial")
summary(stem.out)

# Interpret StandardizedTest
exp(-0.007816)
exp(confint(stem.out))
# for each additional percentile point in a student's standardized test scores,
#   we estimate the odds of switching out of calculus decrease by 0.99% (95% CI: 0.98% decrease, 1.00% decrease)
#   holding all else constant

# Is PrevCalc significant?
stem.red<-glm(Switcher~StandardizedTest+Major+Teacher+ClassPractice+Gender,data=stem, family="binomial")
anova(stem.red,stem.out,test="Chisq")

# Is Major significant?
stem.red<-glm(Switcher~PrevCalc+StandardizedTest+Teacher+ClassPractice+Gender,data=stem, family="binomial")
anova(stem.red,stem.out,test="Chisq")

# Interpret Gender
exp(0.356231)
exp(confint(stem.out))
# If the student is female,
#   we estimate the odds of switching out of calculus increase by 1.43% (95% CI: 1.13% increase, 1.81% increase)
#   holding all else constant

# Make ROC curve
library(ROCR)
stem.pred<-prediction(predict(stem.out,type="response"), stem$Switcher)
stem.perf<-performance(stem.pred,measure="tpr",x.measure="fpr")
plot(stem.perf,xlab="1-Specificity",ylab="Sensitivity",main="ROC Curve")
abline(0,1,col="gray")

# Find Area Under Curve
performance(stem.pred,measure="auc")




