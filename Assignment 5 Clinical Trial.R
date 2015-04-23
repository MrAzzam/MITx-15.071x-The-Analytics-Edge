# Assignment 5 Clinical Trial
# Read in the data

clinical_trial = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(clinical_trial)
max(nchar(clinical_trial$abstract))
which.min(nchar(clinical_trial$title))

# Install new packages
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# TITLE
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))

# ABSTRACT
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusTitle, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial


# Split the data

library(caTools)

set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)

trainSparse = subset(dtm, split==TRUE)
testSparse = subset(dtm, split==FALSE)

table(trainSparse$trial)

# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=trainSparse, method="class")
prp(trialCART)

predTrain = predict(trialCART)[,2]
summary(predTrain)
table(trainSparse$trial, predTrain>=0.5)

# Evaluate the performance of the model
predictCART = predict(trialCART, newdata=testSparse)[,2]
table(testSparse$trial, predictCART>=0.5)

# Compute accuracy

#(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

#300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
#(293+21)/(293+7+34+21)

set.seed(123)
tweetlm = glm(Negative ~ ., data=trainSparse, family="binomial")
predictlm = predict(tweetlm, newdata=testSparse, type="response")
table(testSparse$Negative, predictlm>=0.5)

