url<-"https://online.stat.psu.edu/onlinecourses/sites/stat508/files/german_credit.csv"
german_data<-read.csv(url,header = T,sep = ",")
set.seed(100)
nall=nrow(data)
ntrain= floor(0.7*nall)
ntest= floor(0.3*nall)
index=seq(1:nall)
trainIndex=sample(index,ntrain)
testIndex=index[-trainIndex]
train=german_data[trainIndex,]
test=german_data[testIndex,]
str(german_data)

