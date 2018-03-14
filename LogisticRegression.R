library(dplyr)
library(irr)
library(rpart)
library(caret)
library(ROCR)
library(gains)

setwd("C:\\Jig12051\\Logistic Regression")
#Reading the csv file
good <- read.csv("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 10 - Logistic Regression\\goodforu-class12.csv")

#Taking back up of the actual file
good_bk <- good

summary(good)
str(good)
dim(good)
#No missing values in the data

good$X1 <- as.factor(good$X1)
good$X2 <- as.factor(good$X2)
good$X3 <- as.factor(good$X3)
good$X4 <- as.factor(good$X4)
good$X5 <- as.factor(good$X5)
good$X6 <- as.factor(good$X6)
good$X7 <- as.factor(good$X7)
good$X8 <- as.factor(good$X8)
good$X9 <- as.factor(good$X9)
good$X10 <- as.factor(good$X10)
good$X11 <- as.factor(good$X11)
good$X12 <- as.factor(good$X12)
good$X13 <- as.factor(good$X13)
good$X14 <- as.factor(good$X14)
good$X15 <- as.factor(good$X15)
good$X16 <- as.factor(good$X16)
good$X17 <- as.factor(good$X17)
good$X18 <- as.factor(good$X18)
good$X19 <- as.factor(good$X19)
good$X20 <- as.factor(good$X20)
good$X21 <- as.factor(good$X21)



#Creating dependent variable for Brand A using X23, 1= Good, 0= Bad
good$X23_Target <- ifelse(good$X23>=6,1,0)
good$X23_Target <- as.factor(good$X23_Target)

table(good$X23_Cat,good_bk$X23)

#Removing X23 from data
good <- select(good, -X23)
summary(good)
str(good)


#Checking ratios between DV and IDV

lapply(good[,c("X2","X9","X16")],table)
table(good$X2, good$X23_Target)
table(good$X9, good$X23_Target)
table(good$X16, good$X23_Target)
table(good$X30, good$X23_Target)

plot(good$X23_Target)
           
#Splitting into test and train samples(30%-70%)
set.seed(200)
index <- sample(nrow(good),0.70*nrow(good),replace = F)
train <- good[index,]
test <- good[-index,]

nrow(train)
nrow(test)

table(train$X23_Target)
table(test$X23_Target)

table(train$X23_Target)/nrow(train)
table(test$X23_Target)/nrow(test)


#Model building
mod1 <- glm(X23_Target~X2+X9+X16+X30+X38+X41+X42+X43+X45+X46+X47+X48+X49+
                  X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62
                , family = "binomial", data=train)
summary(mod1)
#step(mod1,direction="backward")

mod2 <- glm(X23_Target~ X2 + X9 + X16 + X30 + X38 + X41 + X42 + X43 + X45 + 
    X51 + X56 + X57 + X58 + X59 + X61 + X62
                , family = "binomial", data=train)
summary(mod2)

mod3 <- glm(X23_Target~ X2 + X9 + X16 + X30 + X38 + X41 + X51 + X57, 
            family = "binomial", data=train)
summary(mod3)

mod <- glm(X23_Target~ X2 + X9 + X16 + X30,family = "binomial", data=train)
summary(mod)

#Final model - mod 

#Predicting the values from model on train 
predicted1 <- as.data.frame(predict(mod, type = "response", data = train))
predicted1 <- ifelse(predicted1>=0.254,1,0)

#Predicting the values from model on est
predicted2 <- as.data.frame(predict(mod, type = "response", newdata = test))
predicted2 <- ifelse(predicted2>=0.254,1,0)

#Validations

#Confusion Matrix
confusionMatrix(predicted1,train$X23_Target, positive = "1")
confusionMatrix(predicted2,test$X23_Target, positive = "1")

#Gains Chart
GainsChart <- gains(as.numeric(test$X23_Target),predict(mod,type="response",newdata=test),groups = 10)


pred<-prediction(predicted2,test$X23_Target)

#ROCR Curve
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

saveRDS(mod,"C:/Jig12051/Logistic Regression/model.rds")
saveRDS(GainsChart,"C:/Jig12051/Logistic Regression/gains.rds")

