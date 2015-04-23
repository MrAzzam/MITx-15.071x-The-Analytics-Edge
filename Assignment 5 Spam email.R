# Unit 5 - Spam

# Read in the data
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
# Convert to a data frame
emailsSparse = as.data.frame(as.matrix(spdtm))

# Make all variable names R-friendly
colnames(emailsSparse) = make.names(colnames(emailsSparse))

# Add dependent variable
emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))
# Split the data

library(caTools)
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

# Logistic Regression
spamLog = glm(spam ~ ., data=train, family="binomial")
summary(spamLog)
predictLog = predict(spamLog, type="response")
library(ROCR)
ROCRpred = prediction(predictLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

table(predictLog<0.00001)
table(predictLog>0.99999)
table(predictLog>=0.00001&predictLog<=0.99999)
table(train$spam, predictLog>0.5)

# CART
library(rpart)
library(rpart.plot)
set.seed(123)
spamCART = rpart(spam ~ ., data=train, method="class")
#predCART = predict(spamCART, type = "class")
predCART = predict(spamCART)[,2]
table(train$spam, predCART>0.5)
library(ROCR)
ROCRpred = prediction(predCART, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# RANDOM FOREST
library(randomForest)
# Build random forest model
set.seed(123)
spamRF = randomForest(spam ~ ., data=train, method="class")
spamRF = randomForest(spam~., data=train)
#predRF = predict(spamRF, type ="class")
predRF = predict(spamRF, type="prob")[,2]
table(train$spam, predRF>0.5)
library(ROCR)
ROCRpred = prediction(predRF, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Logistic Regression over test
predictLog = predict(spamLog, newdata=test, type="response")
table(test$spam, predictLog>0.5)
library(ROCR)
ROCRpred = prediction(predictLog, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# CART over test
set.seed(123)
spamCART = rpart(spam ~ ., data=train, method="class")
predictCART = predict(spamCART, newdata=test, type="prob")[,2]
table(test$spam, predictCART>0.5)
library(ROCR)
ROCRpred = prediction(predictCART, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# RF over test
set.seed(123)
spamRF = randomForest(spam~., data=train)
#predRF = predict(spamRF, type ="class")
predRF = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, predRF>0.5)
library(ROCR)
ROCRpred = prediction(predictCART, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)


# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

emailCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(emailCART)

# Evaluate the performance of the model
predictCART = predict(emailCART, newdata=testSparse, type="class")

table(testSparse$spam, predictCART)

# Compute accuracy

#(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$spam)

#300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)
emailRF = randomForest(Negative ~ ., data=trainSparse)
# Make predictions:
predictRF = predict(emailRF, newdata=testSparse)
table(testSparse$spam, predictRF)
# Accuracy:
#(293+21)/(293+7+34+21)

set.seed(123)
emaillm = glm(Negative ~ ., data=trainSparse, family="binomial")
predictlm = predict(emaillm, newdata=testSparse, type="response")
table(testSparse$spam, predictlm>=0.5)

