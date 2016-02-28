# all the required packages having the desired functionality are imported
library("AppliedPredictiveModeling", lib.loc="~/R/win-library/3.2")
library("caret", lib.loc="~/R/win-library/3.2")
library("doParallel", lib.loc="~/R/win-library/3.2")
library("knitr", lib.loc="~/R/win-library/3.2")
library("randomForest", lib.loc="~/R/win-library/3.2")
#removed all previous objects to have good memory space
rm(list=ls())
#size of the memory is also extened using memory.limit
# the given data set is loaded into the data frame ,unncessary string are removed while laoding the data into data frame
dat_tr <- read.csv(file = "D:/pml-training.csv", na.strings=c("#DIV/0!"))
dim(dat_tr)
# the gaiven data set has 19622 objecta and 160 variables
# the test set data is also loaded in the same way
dat_ts <- read.csv(file = "D:/pml-testing.csv",na.string=c("#DIV/0!"))
# the test  data set is having 20 objects abd 160 variables
#removing the variales/predicotrs having more than 50% NULL Values in trainig data set
dat_na1<- dat_tr[, colMeans(is.na(dat_tr)) <= .50]
#removing the variales/predicotrs having more than 50% NULL Values in testing data set
dat_na2<- dat_ts[, colMeans(is.na(dat_tr)) <= .50]
#Identifying & removing predicotrs having the near to zero variance in training dataset
nz1<-nearZeroVar(dat_na1)
#Identifying & removing predicotrs having the near to zero variance in test dataset
nz2<-nearZeroVar(dat_na2)
dat_nz1<-dat_na1[,-nz1]
dat_nz2<-dat_na2[,-nz2]
#removing predictors which are acting as indicators in training and test datasets
dat_r<-dat_nz1[,-c(1:6)]
dat_s<-dat_nz2[,-c(1:6)]
# create the environmnt for parlle computing , i am using 3 core preocessor
cl <- makeCluster(3)
registerDoParallel(cl)
# pratition the given training data into trainning and validatin sets
tr<- createDataPartition(dat_r$classe, p = .75, list=FALSE)
tr_tr<-dat_nz1[tr,]
tr_ts<-dat_nz1[-tr,]
# train the algorithm using the random forest technique
x <- tr_tr[,-53]
y <- tr_tr[,53]
#tune the model 
fitControl <- trainControl(method = "cv", number = 5,allowParallel = TRUE)
fit <- train(x,y, method="rf",data=tr_tr,trControl = fitControl, ntree=100)
#dislpay the model
print(fit)
# validate the model by using the validation set
pred1 <- predict(fit, newdata=tr_ts)
cm<-confusionMatrix(pred1,tr_ts$classe)
cm
# use the model for predicting the objects in test data set
dat_s1<-dat_s[,-53]
pred1 <- predict(fit, newdata=dat_s1)
pred1