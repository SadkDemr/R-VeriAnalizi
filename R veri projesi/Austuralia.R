
library(SVMMaj)
lirary(bknitr)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)


library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(verification)



data=data("AusCredit")
summary(data)
AusCredit$y<-factor(AusCredit$y,
                    levels = c("Rejected","Accepted"),
                    labels = c(0,1))


data=cbind(AusCredit$X, AusCredit$y)
head(data)
data

##veri sütunlarýnýn ismini deðiþtirmek için 
names(data)[15] <- c("X15")
head(data)

##veride kategorik deðiþkenleri nümerik yapma
data$X1<- factor(data$X1,
                 levels = c("TRUE","FALSE"),
                 labels = c(0,1))


data$X8<- factor(data$X8,
                 levels = c("TRUE","FALSE"),
                 labels = c(0,1))

data$X9<- factor(data$X9,
                 levels = c("TRUE","FALSE"),
                 labels = c(0,1))
data$X11<- factor(data$X11,
                  levels = c("TRUE","FALSE"),
                  labels = c(0,1))

head(data)
data

##veri ??zetleme ??rnekleri grafikler
head(data)
table(data$X15)
summary(data$X13)
plot(data$X15)

table(data$X15,data$X1)
boxplot(data$X2,data$X15)
boxplot(data$X5,data$X15)
boxplot(data$X13,data$X15)
boxplot(data$X8,data$X15)

ggplot(melt(data[,c(2,15)]), aes(x = variable, y = value, fill = X15)) + geom_boxplot() + xlab("X15") + ylab("X2")
ggplot(melt(data[,c(2,15)]), aes(x = variable, y = value, fill = X15)) + geom_boxplot() + xlab("X15") + ylab("X4")


ggplot(data, aes(factor(X5), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Egitim Durumu")+ylab("Kisi Sayisi")

ggplot(data, aes(factor(X13), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Posta Kodu")+ylab("Kisi Sayisi")



ggplot(data, aes(factor(X9), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Issizlik")+ylab("Kisi Sayisi")
ggplot(data, aes(factor(X8), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Kredi Alip Almadiði")+ylab("Kisi Sayisi")
ggplot(data, aes(factor(X13), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Posta kodu")+ylab("Kisi Sayisi")







##kullan??lan baz?? paketler
library(neuralnet)
library(MASS)

##veriyi test train diye ay??rma
index<-sample(1:nrow(data), round(0.80*nrow(data)))
data.test <- data[-index,]
data.train<-data[index,]


##lojistik regresyon modeli

model<-glm(formula = X15 ~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14 , family = binomial, data =data.train)
summary(model)
model2<- step(model, direction = "backward")
summary(model2)
model3<-step(model,direction = "forward")
summary(model3)
n
glm(formula = X15 ~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14 , family = binomial, data =data.train)


credit.glm.final <- glm(X15 ~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14 , family = binomial, data =data.train)

summary(credit.glm.final)
##******************************************
model<-glm(formula = X15 ~ X1 + X2 + X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14 , family = binomial, data =data.train)
summary(model)
model2<- step(model, direction = "backward")
summary(model2)
model3<- step(model, direction = "forward")
summary(model3)

prob.glm1.insample <- predict(model3, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.1667
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
mean(ifelse(data.train$X15 != predicted.glm1.insample, 1, 0))

table(data.train$X15, predicted.glm1.insample, dnn = c("Truth", "Predicted"))

roc.plot(data.train$X15 == "1", prob.glm1.insample)
roc.plot(data.train$X15 == "1", prob.glm1.insample)$roc.vol$Area








##BU DE??????KEBLERDEN MODEL KUR EN SON HAL?? (chk_acct, duration, credit_his, amount, saving_acct,installment_rate,other_install)


glm(formula = X15 ~ X4 + X5 + X7 + X8 + X10 + X11 + X12 + X14, 
    family = binomial, data = data.train)
summary(credit.glm.final)

##tahmin kýsmý
prob.glm1.insample <- predict(model2, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.1667
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
mean(ifelse(data.train$X15 != predicted.glm1.insample, 1, 0))


##tahmin edilenler ger??ekte olanlar tablosu
table(data.train$X15, predicted.glm1.insample, dnn = c("Truth", "Predicted"))

##ROC E??R??S?? (paket y??kle)

library(verification)


roc.plot(data.train$X15 == "1", prob.glm1.insample)
roc.plot(data.train$X15 == "1", prob.glm1.insample)$roc.vol$Area



###KARAR AGACI DENEME tum degýskenlerle (paket yuklemek gerekli)

library(rpart)
library(rpart.plot)

tree<- rpart(X15~., data = data.train, method = 'class')
rpart.plot(tree)

##sadece modeldeki degýskenlerle
tree<- rpart(model2, data = data.train, method = 'class')
rpart.plot(tree)


##TEST iciN
prob.glm1.outsample <- predict(model2, data.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.1667
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(data.test$X15, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))

mean(ifelse(data.test$X15 != predicted.glm1.outsample, 1, 0))

roc.plot(data.test$X15 == "1", prob.glm1.outsample)
roc.plot(data.test$X15 == "1", prob.glm1.outsample)$roc.vol$Area
##Forward için test

prob.glm1.outsample <- predict(model3, data.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.1667
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(data.test$X15, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))

mean(ifelse(data.test$X15 != predicted.glm1.outsample, 1, 0))


roc.plot(data.test$X15 == "1", prob.glm1.outsample)
roc.plot(data.test$X15 == "1", prob.glm1.outsample)$roc.vol$Area






ggplot(data.test, aes(factor(X5), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Egitim Durumu")+ylab("Kisi Sayisi")
ggplot(data.test, aes(factor(X9), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Issizlik")+ylab("Kisi Sayisi")
ggplot(data.test, aes(factor(X8), ..count..)) +
  geom_bar(aes(fill = X15), position = "dodge") + xlab("Kredi Alip Almadiði")+ylab("Kisi Sayisi")



##korelasyon bakma
library(ellipse)
library(corrplot)
c<-data.frame(data$X2,data$X3,data$X7,data$X10,data$X13,data$X14)
cor(c)
corrplot(cor(c))

##Yapay Sinir Aðlarý

library(neuralnet)
nn=neuralnet(X15~X2+X3+X7+X13+X14,data=data.train, hidden=2)
plot(nn)

#Random Forest
data$X15 <- as.factor(data$X15)
table(data$X15)

library(randomForest)

set.seed(31)
index<-sample(1:nrow(data), round(0.80*nrow(data)))
data.test <- data[-index,]
data.train<-data[index,]
table(data.train$X15)

#one of regression, classification, or unsupervised
r <- randomForest(X15 ~ ., data=data.train, importance=TRUE, do.trace=10)

#The original call to randomForest
r$call
#one of regression, classification, or unsupervised
r$type
#MeanDecrease Accuracy Column is the mean decrease in accuracy over all classes
#MeanDecreaseGini is the mean decrease in Gini Index
r$importance
round(importance(r), 2)
#Standard Error permutation based importance measure
r$importanceSD
#Number of Tree grown
r$ntree
#Number of predictors sampled for spliting at each node
r$mtry
#A list that contains the entire forest
r$forest
#Obtain an individual tree
getTree(r,k=2)

#vector error rates of the prediction on the input data
head(r$err.rate)
#Confusion Matrics
r$confusion
#predicted values
r$predicted
table(r$predicted)
r$test
r$votes
#Out of Bag (OOB) data
r$oob.times

print(r)

library(ggplot2)
class_1_importance <- data.frame(feature=names(r$importance[,1]), importance=r$importance[,1])
ggplot(class_1_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

class_2_importance <- data.frame(feature=names(r$importance[,2]), importance=r$importance[,2])
ggplot(class_2_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

##truth predicted table
table(r$predicted,data.train$X15)



data1<- read.csv("AusCredit", header = FALSE)
head(data1)
summary(data1$X15)
data<-na.omit(data1)
str(data)

data$X15<- as.factor(data$X15)
table(data$X15)

library(randomForest)

set.seed(31)
index<-sample(1:nrow(data), round(0.80*nrow(data)))
data.test <- data[-index,]
data.train<-data[index,]
table(data.train$X15)

#one of regression, classification, or unsupervised
r <- randomForest(X15 ~ ., data=data.test, importance=TRUE, do.trace=10)

#The original call to randomForest
r$call
#one of regression, classification, or unsupervised
r$type
#MeanDecrease Accuracy Column is the mean decrease in accuracy over all classes
#MeanDecreaseGini is the mean decrease in Gini Index
r$importance
round(importance(r), 2)
#Standard Error permutation based importance measure
r$importanceSD
#Number of Tree grown
r$ntree
#Number of predictors sampled for spliting at each node
r$mtry
#A list that contains the entire forest
r$forest
#Obtain an individual tree
getTree(r,k=2)

#vector error rates of the prediction on the input data
head(r$err.rate)
#Confusion Matrics
r$confusion
#predicted values
r$predicted
table(r$predicted)
r$test
r$votes
#Out of Bag (OOB) data
r$oob.times

print(r)

library(ggplot2)
class_1_importance <- data.frame(feature=names(r$importance[,1]), importance=r$importance[,1])
ggplot(class_1_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

class_2_importance <- data.frame(feature=names(r$importance[,2]), importance=r$importance[,2])
ggplot(class_2_importance, aes(x=feature, y=importance)) + geom_bar(stat="identity")

##truth predicted table
table(r$predicted,data.test$X15)


plot(r, type="1" ,main=deparse(substitute(r)))







