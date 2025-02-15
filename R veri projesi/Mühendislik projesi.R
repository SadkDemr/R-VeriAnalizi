install.packages("CreditRisk")
library(CreditRisk)
install.packages("klaR")
library(klaR)
data$V19

library(caret)

library(pls)

library(MASS)

library(Matrix)

library(ISLR)

library(glmnet)

library(bromm)

library(tidyverse)
library(glmnet)

library(elasticnet)

library(PerformanceAnalytics)

library(funModeling)

library(AppliedPredictiveModeling)



german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
german_credit



names(german_credit)[1:21]<-c("MEVCUT �EK HESABI DURUMU","AYLIK S�RE","KRED� GE�M���","AMA�","KRED� TUTARI","TASARRUF HESABI","MEVCUT �ST�HDAM","HARCANAB�L�R GEL�R Y�ZDES� TAKS�T ORANI","K���SEL DURUM VE C�NS�YET","D��ER BOR�LULAR","MEVCUT �KAMETGAH","�ZELL�K","YILLAR ���NDE YA�","D��ER TAKS�T PLANLARI","KONUT","BU BANKADA MEVCUT KRED� SAYISI","��","BAKIM SA�LAMA ���N SORUMLU OLAN K��� SAYISI","TELEFON","YABANCI ����")
plot(german_data$V21)

german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
data<-german_credit
data
str(data)

##head komutu verinin sadece ilk 6 sat�r�n� g�sterir
head(data)

##kategorik de�i�keni n�merik hale getirme
data$V1<- factor(data$V1,
                 levels = c('A11', 'A12','A13', 'A14'),
                 labels = c(1, 2, 3, '4'))
data
str(data)
seedstrain<- sample(1:210,147)
seedstest <- setdiff(1:210,seedstrain)
ideal <- class.ind(seeds$Class)
seedsANN = nnet(irisdata[seedstrain,-8], ideal[seedstrain,], size=10, softmax=TRUE)
predict(seedsANN, seeds[seedstrain,-8], type="class")
table(predict(seedsANN, seeds[seedstest,-8], type="class"),seeds[seedstest,]$Class)
boxplot(german_credit$V21)

