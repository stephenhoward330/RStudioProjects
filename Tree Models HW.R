
# Tree Models Homework


# Download the data
temp<-tempfile()
download.file("https://www.tandfonline.com/doi/suppl/10.1080/10691898.2018.1434342/suppl_file/ujse_a_1434342_sm1547.zip",temp)
default<-read.csv(unz(temp, "SBAcase.11.13.17.csv"))
unlink(temp)

# Remove columns for MIS_Status, ChgOffDate, ChgOffPrinGr, Selected, Name
#   Recession, daysterm, City, State, Bank, xx
default<-subset(default,select=-c(MIS_Status,ChgOffDate))
default<-subset(default,select=-c(ChgOffPrinGr,ï..Selected,Name,Recession))
default<-subset(default,select=-c(daysterm,City,State,Bank,xx))

# Add columns for first 2 and 3 digits of ZIP and first 3 and 4 digits of NAICS
default$Zip_2<-floor(default$Zip/1000)
default$Zip_3<-floor(default$Zip/100)
default$NAICS_3<-floor(default$NAICS/1000)
default$NAICS_4<-floor(default$NAICS/100)

# Filter if value of NewExist isn't 1 or 2
default<-subset(default, NewExist==1 | NewExist==2)

# Filter if DisbursementDate is missing
default<-default[complete.cases(default$DisbursementDate), ]

# Convert Default from numeric (0, 1) to factor (no, yes)
default$Default<-ifelse(default$Default==1, "Yes", "No")
default$Default<-as.factor(default$Default)

# Report dim and a table of the possible values for Default
dim(default)
table(default$Default)

# get a list of the variables
names(default)



# modeling dataset with half goods (No) and half bads (Yes)
set.seed(2319)

all.bad<-subset(default,Default=="Yes")
n.bads<-dim(all.bad)[1]
# SRS of goods
all.good<-subset(default,Default=="No")
n.goods<-dim(all.good)[1]
rows.good<-sample(n.goods, n.bads)
sample.good<-all.good[rows.good,]

# smash all bad and sample of goods
default.model<-rbind(all.bad,sample.good)

# confirm default.model has half goods and half bads
dim(default.model)
table(default.model$Default)


# create train and test datasets
n.model<-dim(default.model)[1]
# 80% train, 20% test
0.8*n.model  # 1097.6
train.rows<-sample(n.model,1100)
default.train<-default.model[train.rows,]
default.test<-default.model[-train.rows,]

# confirm train and test are similar
table(default.train$Default)
table(default.test$Default)


# Random Forest

# find response variable
names(ticket.train)

library(randomForest)

default.out<-randomForest(x=default.train[,-24], y=default.train$Default,
                         xtest=default.test[,-24], ytest=default.test$Default,
                         keep.forest=TRUE, replace=TRUE, ntree=50, mtry=10,
                         nodesize=20)

# let's see how it went
default.out

# intuition of what matters in the model
varImpPlot(default.out)
# the three most important explanatory variables are 
#     Term, SPA_Appv, and BankState



