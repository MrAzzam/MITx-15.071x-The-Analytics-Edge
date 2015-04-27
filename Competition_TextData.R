# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
set.seed(1000)

# Libraries
library(tm)
library(randomForest)
library(ROCR)

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
modelrf<-function(nytdata)
{  
  nytRF = randomForest(Popular ~ . - UniqueID, data=nytdata, method="class", ntree=200, nodesize=25) 
  predictRF = predict(nytRF, newdata=nytdata)
  
  predROCR = prediction(predictRF, nytdata$Popular)
  perfROCR = performance(predROCR, "tpr", "fpr")
  plot(perfROCR, colorize=TRUE)
  auc = performance(predROCR, "auc")@y.values
  printf("auc train %f\n", auc)
  nytRF
}

getsubsetsbydeskname<-function(input) {
  list(subset(input, NewsDesk=="Business"),
       subset(input, NewsDesk=="Culture"),
       subset(input, NewsDesk=="Foreign"),
       subset(input, NewsDesk=="Magazine"),
       subset(input, NewsDesk=="Metro"),
       subset(input, NewsDesk=="OpEd"),
       subset(input, NewsDesk=="Science"),
       subset(input, NewsDesk=="Styles"),
       subset(input, NewsDesk=="TStyle"),
       subset(input, NewsDesk=="Travel"),
       subset(input, NewsDesk==""))
}
# total 11 levels
newsdesk = c("Business", "Culture", "Foreign", "Magazine", "Metro", "OpEd", "Science", "Styles", "TStyle","Travel", "" )

# Load input
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="National")
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="Sports")
NewsTrain = subset(NewsTrain, NewsTrain$SectionName!="Style")
NewsTrain$WordCount = as.integer(NewsTrain$WordCount/100)

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTest$WordCount = as.integer(NewsTest$WordCount/100)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)
NewsTest$SubsectionName = as.factor(NewsTest$SubsectionName)

# TRAINING DATA
bagofwords = getwords(c(NewsTrain$Snippet, NewsTest$Snippet), ratio = 0.99)

colnames(bagofwords) = make.names(colnames(bagofwords))

# Packing Independent Variables
Train = head(bagofwords, nrow(NewsTrain))
Test = tail(bagofwords, nrow(NewsTest))

Train$UniqueID = NewsTrain$UniqueID
Train$NewsDesk = NewsTrain$NewsDesk
Train$SectionName = NewsTrain$SectionName
Train$SubsectionName = NewsTrain$SubsectionName
Date = strptime(NewsTrain$PubDate, format="%Y-%m-%d %H:%M:%S")
Train$Weekday = as.factor(weekdays(Date))
Train$Hour = as.factor(Date$hour)
Train$WordCount = NewsTrain$WordCount
Train$Popular = NewsTrain$Popular
#remove Magazine column
#Train = subset(Train, Train$NewsDesk!="Magazine")

trainsubsets = split(Train, Train$NewsDesk)

Test$UniqueID = NewsTest$UniqueID
Test$NewsDesk  = NewsTest$NewsDesk
Test$SectionName = NewsTest$SectionName
Test$SubsectionName = NewsTest$SubsectionName
Date = strptime(NewsTest$PubDate, format="%Y-%m-%d %H:%M:%S")
Test$Weekday = as.factor(weekdays(Date))
Test$Hour = as.factor(Date$hour)
Test$WordCount = NewsTest$WordCount
#remove Magazine column
#Test = subset(Train, Test$NewsDesk!="Magazine")

testsubsets = split(Test, Test$NewsDesk)

# Random Forest Model
#nytRF = modelrf(Train)

#models = list()
#for (i in 1:11) {
#   if (i!=5) {
#   list[[i]] = modelrf(trainsubsets[[i]])
#  }
#}

predtests = list()
for (i in 1:11) {
  if (i!=5) {
    predtests[[i]] = predict(modelrf(trainsubsets[[i]]), newdata=as.data.frame(testsubsets[[i]]))
    for (j in 1:length(predtests[[i]])) {
      if (predtests[[i]][j]<0) {
        predtests[[i]][j] = 0
      }
    }
  }
  else
    predtests[[5]] = rep(0.0,nrow(testsubsets[[5]]))
}

# Test Data

#PredTest = predict(nytRF, newdata=Test)
#PredTest[PredTest<0] = 0
# Generate Submission Data
df = list()
for (i in 1:11) {
  printf("i = %d\n",i)
  df[[i]] = data.frame(UniqueID = testsubsets[[i]]$UniqueID, Probability1 = predtests[[i]])
}

MySubmission = Reduce(function(x, y) merge(x, y, all=TRUE),
                      list(df[[1]],
                           df[[2]],
                           df[[3]],
                           df[[4]],
                           df[[5]],
                           df[[6]],
                           df[[7]],
                           df[[8]],
                           df[[9]],
                           df[[10]],
                           df[[11]]))
#MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

