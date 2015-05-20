Airlines = read.csv("AirlineDelay.csv")

set.seed(15071)
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]

delaymodel = lm(TotalDelay ~ ., data=AirlinesTrain)
tempPredict = predict(delaymodel, newdata=AirlinesTest)

SSE = sum((tempPredict - AirlinesTest$TotalDelay)^2)
SST = sum((mean(AirlinesTrain$TotalDelay)-AirlinesTest$TotalDelay)^2)
R2 = 1 - SSE/SST

Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
Airlines$TotalDelay = NULL

library(caTools)
set.seed(15071)
spl = sample.split(Airlines$DelayClass, SplitRatio = 0.7)
train = subset(Airlines, spl==TRUE)
test = subset(Airlines, spl==FALSE)

library(rpart)
library(rpart.plot)

model = rpart(DelayClass ~ ., data=train, method="class")
prp(model)

predictTrain = predict(model, newdata=train, type="class")
table(train$DelayClass, predictTrain)

predictTest = predict(model, newdata=test, type="class")
table(test$DelayClass, predictTest)