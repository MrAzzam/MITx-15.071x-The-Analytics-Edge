ebay = read.csv("ebay.csv")
ebay$sold = as.factor(ebay$sold)
ebay$condition = as.factor(ebay$condition)
ebay$heel = as.factor(ebay$heel)
ebay$style = as.factor(ebay$style)
ebay$color = as.factor(ebay$color)
ebay$material = as.factor(ebay$material)

set.seed(144)
library(caTools)
spl = sample.split(ebay$sold, 0.7)

training = subset(ebay, spl==TRUE)
testing = subset(ebay, spl==FALSE)

model = glm(sold ~ biddable+startprice+condition+heel+style+color+material, data=training, family="binomial")
predictTest = predict(model, newdata = testing, type = "response")

library(caret)
library(e1071)
set.seed(144)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,by=0.001)) 
# Perform the cross validation
train(sold ~ biddable+startprice+condition+heel+style+color+material, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(ebay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.9)
descriptionText = as.data.frame(as.matrix(dtm))
colSums(descriptionText)

names(descriptionText) = paste0("D", names(descriptionText))

descriptionText$sold=ebay$sold
descriptionText$biddable=ebay$biddable
descriptionText$startprice=ebay$startprice
descriptionText$condition=ebay$condition
descriptionText$heel=ebay$heel
descriptionText$style=ebay$style
descriptionText$color=ebay$color
descriptionText$material=ebay$material

trainText = subset(descriptionText, spl==TRUE)
testText = subset(descriptionText, spl==FALSE)

library(ROCR)

glmText = glm(sold ~ ., data=trainText, family="binomial")

predictTrain = predict(glmText, newdata=trainText, type="response")
ROCRpred = prediction(predictTrain, trainText$sold)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

predictTest = predict(glmText, newdata=testText, type="response")
ROCRpred = prediction(predictTest, testText$sold)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

