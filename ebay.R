ebay = read.csv("ebay.csv", stringsAsFactors=FALSE)
ebay$sold = as.factor(ebay$sold)
ebay$condition = as.factor(ebay$condition)
ebay$heel = as.factor(ebay$heel)
ebay$style = as.factor(ebay$style)
ebay$color = as.factor(ebay$color)
ebay$material = as.factor(ebay$material)

set.seed(144)
library(caTools)
spl = sample.split(ebay$sold, 0.7)

train = subset(ebay, spl==TRUE)
test = subset(ebay, spl==FALSE)

model = glm(sold ~ biddable+startprice+condition+heel+style+color+material, data=train, family=binomial)
newdata=data.frame(biddable=0, startprice=100, condition="Pre-owned", heel="High", style="Open Toe", color="Black", material="Satin")

predict(model, newdata, type="response")
predicttest = predict(model, newdata=test, type="response")

table(test$sold, predicttest >= 0.5)

library(ROCR)
ROCRpred = prediction(predicttest, test$sold)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf, colorize=TRUE)
as.numeric(performance(ROCRpred, "auc")@y.values)

#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

set.seed(144)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001, 0.05, by=0.001)) 
# Perform the cross validation
train(sold ~ biddable+startprice+condition+heel+style+color+material, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
#create a new cart model
train_model = rpart(sold ~ biddable+startprice+condition+heel+style+color+material, data=train, method="class", cp = 0.005)
prp(train_model)
predict_test = predict(train_model, newdata=test, type="class")
table(test$sold, predict_test)


#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
set.seed(144)
corpus = Corpus(VectorSource(ebay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.9)
spdtm
descriptionText = as.data.frame(as.matrix(spdtm, unique=TRUE))
#descriptionText = as.data.frame(make.names(as.matrix(spdtm, unique=TRUE)))
descriptionText
