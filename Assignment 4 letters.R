setwd("/Users/chicheongweng/Downloads")
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
CARTb = rpart(isB ~.-letter, data=train, method="class")
test = subset(letters, spl == FALSE)
#baseline model
table(test$isB)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
PREDICTb = predict(CARTb, newdata = test, type = "class")
table(test$isB, PREDICTb)

set.seed(1000)

library(randomForest)

# Build random forest model
FORESTb = randomForest(isB ~ . - letter, data = train, method="class")
PREDICTb = predict(FORESTb, newdata=test, type ="class")
table(test$isB, PREDICTb)

letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

CARTletter = rpart(letter ~ ., data=train, method="class")
PREDICTletter = predict(CARTletter, newdata = test, type = "class")
table(test$letter, PREDICTletter)

set.seed(1000)
FORESTletter = randomForest(letter ~ ., data = train, method="class")
PREDICTletter = predict(FORESTletter, newdata=test, type ="class")
table(test$letter, PREDICTletter)
