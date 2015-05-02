# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
set.seed(1000)

# Libraries
library(tm)
library(randomForest)
library(ROCR)
library(mice)

# Utility routines
getwords <- function(vector, ratio) {
  corpus = Corpus(VectorSource(vector))
  
  # Preprocessing
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stemDocument)
  
  # convert our corpus to a DocumentTermMatrix,
  # remove sparse terms, and turn it into a data frame. 
  dtm = DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(dtm, ratio)
  bagofwords = as.data.frame(as.matrix(sparse))
  bagofwords
}

printf <- function(...) cat(sprintf(...))
model_randomforest<-function(nytdata)
{
  nytRF = randomForest(Popular ~ ., data=nytdata, method="class", ntree=200, nodesize=25) 
  predictRF = predict(nytRF, newdata=nytdata)
  predROCR = prediction(predictRF, nytdata$Popular)
  perfROCR = performance(predROCR, "tpr", "fpr")
  plot(perfROCR, colorize=TRUE)
  auc = performance(predROCR, "auc")@y.values
  printf("auc train %f", auc)
  nytRF
}

# Load input
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="National")
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="Sports")
NewsTrain = subset(NewsTrain, NewsTrain$SectionName!="Style")
NewsTrain$WordCount = as.integer(NewsTrain$WordCount/100)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTest$WordCount = as.integer(NewsTest$WordCount/100)

# TRAINING DATA
NewsTrain$Snippet = paste(NewsTrain$Headline, NewsTrain$Snippet)
NewsTest$Snippet  = paste(NewsTest$Headline,  NewsTest$Snippet)

bagofwords = getwords(c(NewsTrain$Snippet, NewsTest$Snippet), ratio = 0.999)

colnames(bagofwords) = make.names(colnames(bagofwords))

# Packing Independent Variables
Train = head(bagofwords, nrow(NewsTrain))
Test = tail(bagofwords, nrow(NewsTest))

Train$NewsDesk = as.factor(NewsTrain$NewsDesk)
Train$SectionName = as.factor(NewsTrain$SectionName)
Train$SubsectionName = as.factor(NewsTrain$SubsectionName)
Date = strptime(NewsTrain$PubDate, format="%Y-%m-%d %H:%M:%S")
Train$Weekday = as.factor(weekdays(Date))
Train$Hour = as.factor(Date$hour)
Train$WordCount = NewsTrain$WordCount
Train$Popular = NewsTrain$Popular

Test$NewsDesk  = as.factor(NewsTest$NewsDesk)
Test$SectionName = as.factor(NewsTest$SectionName)
Test$SubsectionName = as.factor(NewsTest$SubsectionName)
Date = strptime(NewsTest$PubDate, format="%Y-%m-%d %H:%M:%S")
Test$Weekday = as.factor(weekdays(Date))
Test$Hour = as.factor(Date$hour)
Test$WordCount = NewsTest$WordCount

# Random Forest Model
nytRF = model_randomforest(Train)

# Test Data
PredTest = predict(nytRF, newdata=Test)
PredTest[PredTest<0] = 0

# Generate Submission Data
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

