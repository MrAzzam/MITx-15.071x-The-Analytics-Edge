setwd("/Users/chicheongweng/Downloads")

census = read.csv("census.csv")
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

#logistic regression
censusglm = glm( over50k ~ . , family="binomial", data = train)
summary(censusglm)
predictTest = predict(censusglm, newdata = test, type = "response")
table(test$over50k, predictTest >= 0.5)
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

#cart model
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
train_rpart = rpart(over50k ~ ., data=train, method="class")
prp(train_rpart)
library(ROCR)
predict_test_rpart = predict(train_rpart, newdata = test)
predict_test_rpart = predict_test_rpart[,2]
rp = prediction(predict_test_rpart, test$over50k)
perf = performance(rp, "tpr", "fpr")
plot(perf)
table(test$over50k, predict_test_rpart)

#random forest model
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
train_randomforest = randomForest(over50k ~ ., data=trainSmall)
PREDICTb = predict(train_randomforest, newdata=test)
table(test$over50k, PREDICTb)

vu = varUsed(train_randomforest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(train_randomforest$forest$xlevels[vusorted$ix]))
varImpPlot(train_randomforest)

#selecting cp by cross validation
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
# Perform the cross validation
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
#create a new cart model
train_model = rpart(over50k ~ ., data=train, method="class", cp = 0.002)
predict_test = predict(train_model, newdata=test, type="class")
table(test$over50k, predict_test)
