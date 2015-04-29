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
  nytRF = randomForest(Popular ~ . - UniqueID, data=nytdata, method="class") 
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

trains = split(NewsTrain, NewsTrain$NewsDesk)
tests = split(NewsTest, NewsTest$NewsDesk)

trains<-list()
tests<-list()

total = 1

trains[[1]] = subset(NewsTrain, NewsTrain$NewsDesk == "Business" | NewsTrain$NewsDesk == "Foreign" | NewsTrain$NewsDesk == "OpEd")
trains[[2]] = subset(NewsTrain, !(NewsTrain$NewsDesk == "Business" | NewsTrain$NewsDesk == "Foreign" | NewsTrain$NewsDesk == "OpEd"))
trains[[1]] = NewsTrain

tests[[1]] = subset(NewsTest, NewsTest$NewsDesk == "Business" | NewsTest$NewsDesk == "Foreign" | NewsTest$NewsDesk == "OpEd")
tests[[2]] = subset(NewsTest, !(NewsTest$NewsDesk == "Business" | NewsTest$NewsDesk == "Foreign" | NewsTest$NewsDesk == "OpEd"))
tests[[1]] = NewsTest

trainsubsets = list()
testsubsets = list()

# TRAINING DATA
for (i in 1:total) {
  bagofwords = getwords(c(trains[[i]]$Snippet, tests[[i]]$Snippet), ratio = 0.995)
  colnames(bagofwords) = make.names(colnames(bagofwords))
  trainsubsets[[i]] = head(bagofwords, nrow(trains[[i]]))
  testsubsets[[i]] = tail(bagofwords, nrow(tests[[i]]))
}

date1 = list()
date2 = list()

for (i in 1:total) {
  printf("i = %d\n",i)
  trainsubsets[[i]]$UniqueID = trains[[i]]$UniqueID
  trainsubsets[[i]]$Popular = trains[[i]]$Popular
  trainsubsets[[i]]$NewsDesk = trains[[i]]$NewsDesk
  trainsubsets[[i]]$SectionName = trains[[i]]$SectionName
  date1[[i]] = strptime(trains[[i]]$PubDate, format="%Y-%m-%d %H:%M:%S")
  trainsubsets[[i]]$Weekday = as.factor(weekdays(date1[[i]]))
  trainsubsets[[i]]$Hour = as.factor(date1[[i]]$hour)
  trainsubsets[[i]]$WordCount = trains[[i]]$WordCount
  
  testsubsets[[i]]$UniqueID = tests[[i]]$UniqueID
  testsubsets[[i]]$NewsDesk = tests[[i]]$NewsDesk
  testsubsets[[i]]$SectionName = tests[[i]]$SectionName
  date2[[i]] = strptime(tests[[i]]$PubDate, format="%Y-%m-%d %H:%M:%S")
  testsubsets[[i]]$Weekday = as.factor(weekdays(date2[[i]]))
  testsubsets[[i]]$Hour = as.factor(date2[[i]]$hour)
  testsubsets[[i]]$WordCount = tests[[i]]$WordCount

}

predtests = list()
for (i in 1:total) {
  # if (i != 5)
  {
    printf("predicting i %d\n",i)
    predtests[[i]] = predict(modelrf(trainsubsets[[i]]), newdata=as.data.frame(testsubsets[[i]]), type="class")
    for (j in 1:length(predtests[[i]])) {
      if (predtests[[i]][j]<0) {
       predtests[[i]][j] = 0
      }
    }
  }
  #else
  #  predtests[[5]] = rep(0.0,nrow(testsubsets[[5]]))
}

df = list()
for (i in 1:total) {
  printf("i = %d\n",i)
  df[[i]] = data.frame(UniqueID = testsubsets[[i]]$UniqueID, Probability1 = predtests[[i]])
}

MySubmission = Reduce(function(x, y) merge(x, y, all=TRUE),
                      list(df[[1]]                         
                           #,df[[2]]
                           #,df[[3]],df[[4]],df[[5]],df[[6]],df[[7]],df[[8]],df[[9]],df[[10]],df[[11]]
                           ))

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

