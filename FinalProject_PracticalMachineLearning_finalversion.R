library(MASS)
library(caret)
library(rattle)
library(foreach)
library(rpart)
library(randomForest)
library(ppcor)

##### 1) Getting data #####

setwd("C:/Users/Dani/Documents/Coursera/Machine Learning")
trainData <- read.csv("pml-training.csv", header = T, na.strings = c("NA", "#DIV/0!", ""))[,-(1:7)]
testData <- read.csv("pml-testing.csv", header = T, na.strings = c("NA", "#DIV/0!", ""))[,-(1:7)]


##### 2) Cleaning data #####

## getting rid of the columns that with more than 40% NA

data_NA <- apply(is.na(trainData), 2, sum) > 0.4*nrow(trainData)
train <- (trainData[,!data_NA])
test <- (testData[,!data_NA])


##### 3) Building model #####

### checking correlation ###
train_matrix <- as.matrix(train[,-53])
correlation <- pcor(train_matrix, method="pearson")$estim
diag(correlation) <- 0
which((abs(correlation))>0.80, arr.ind=TRUE)
## we eliminate gyros_arm_x, magnet_arm_z, gyros_dumbbell_x and magnet_forearm_x
train_noncorr <- train[,-c(18,26,31,47)]

### splitting data ###
set.seed(123)
inTrain <- createDataPartition(train_noncorr$classe, p=0.6, list=F)
training  <- train_noncorr[inTrain,]
testing  <- train_noncorr[-inTrain,]
dim(training); dim(testing)

### eliminating predictors ###
set.seed(123)
RF <- randomForest(classe~., data=training, ntree=500)
varImpPlot(RF)
## we choose 12 most important predictors
df <- as.data.frame(RF$importance)
order(df$MeanDecreaseGini, decreasing = T)[1:13]
## get 12 most important 
TRAINING <- (training[,c(order(df$MeanDecreaseGini, decreasing = T)[1:13], 49)])


save.image("C:/Users/Dani/Documents/Coursera/Machine Learning/FINALwspace.RData")

load("C:/Users/Dani/Documents/Coursera/Machine Learning/FINALwspace.RData")

### Random Forest with Bagging ###

## bootstrapped sample size 1/p of the original number of observations, iter is number of iterations
bagg_RF <- function(training_data,testing_data,p,iter)
{
  predictions <- foreach(m=1:iter,.combine=cbind) %do% {
    sampled_positions <- sample(nrow(training_data), size=floor((nrow(training_data)/p)))
    training_positions <- 1:nrow(training_data) %in% sampled_positions
    set.seed(123)
    RF_fit <- randomForest(classe~., data=training_data[training_positions,], ntree=500)
    predict(RF_fit,newdata=testing_data)
  }
  Predictions_num <- round(rowMeans(predictions))
  Predictions <- as.vector(length(Predictions_num))
  for(i in 1:length(Predictions_num)){
    if(Predictions_num[i]==1) Predictions[i]="A"
    if(Predictions_num[i]==2) Predictions[i]="B"
    if(Predictions_num[i]==3) Predictions[i]="C"
    if(Predictions_num[i]==4) Predictions[i]="D"
    if(Predictions_num[i]==5) Predictions[i]="E"
  }
  Predictions
}

testing_pred <- bagg_RF(TRAINING,testing,p=4,iter=100)
confusionMatrix(testing$classe, testing_pred)

bagg_RF(TRAINING,testData,p=4,iter=100)


## we have chosen randomForest because it is quicker and we get slightly better results
RF_fit2 <- randomForest(classe~., data=TRAINING, ntree=500)
RF_fit2
fitModel_trainRF <- train(classe~., data=TRAINING, method="rf", trControl=trainControl(method="cv",number=3), allowParallel=TRUE)
fitModel_trainRF$finalModel

