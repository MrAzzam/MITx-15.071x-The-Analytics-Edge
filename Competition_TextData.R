# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

set.seed(1000)
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="National")
NewsTrain = subset(NewsTrain, NewsTrain$NewsDesk!="Sports")
NewsTrain = subset(NewsTrain, NewsTrain$SectionName!="Style")
NewsTrain$WordCount = as.integer(NewsTrain$WordCount/100)
NewsTrain$LessThan2000 = NewsTrain$WordCount<=20

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTest$WordCount = as.integer(NewsTest$WordCount/100)
NewsTest$LessThan2000 = NewsTest$WordCount<=20

library(tm)

# TRAINING DATA

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))

# Preprocessing

CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# convert our corpus to a DocumentTermMatrix,
# remove sparse terms, and turn it into a data frame. 

dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.98)
HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# -------------

# Note that this split of HeadlineWords works to properly put the observations back 
# into the training and testing sets, because of how we combined them together when 
# we first made our corpus. Before building models, we want to add back the original
# variables from our datasets. We'll add back the dependent variable to the training
# set, and the WordCount variable to both datasets. You might want to add back more
# variables to use in your model - we'll leave this up to you!

HeadlineWordsTest$NewsDesk  = as.factor(NewsTest$NewsDesk)
HeadlineWordsTest$SectionName = as.factor(NewsTest$SectionName)
HeadlineWordsTest$SubsectionName = as.factor(NewsTest$SubsectionName)
Date = strptime(NewsTest$PubDate, format="%Y-%m-%d %H:%M:%S")
HeadlineWordsTest$Weekday = as.factor(weekdays(Date))
HeadlineWordsTest$Hour = as.factor(Date$hour)
HeadlineWordsTest$WordCount = NewsTest$WordCount

HeadlineWordsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
HeadlineWordsTrain$SectionName = as.factor(NewsTrain$SectionName)
HeadlineWordsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
Date = strptime(NewsTrain$PubDate, format="%Y-%m-%d %H:%M:%S")
HeadlineWordsTrain$Weekday = as.factor(weekdays(Date))
HeadlineWordsTrain$Hour = as.factor(Date$hour)
HeadlineWordsTrain$WordCount = NewsTrain$WordCount

HeadlineWordsTrain$Popular = NewsTrain$Popular

# Random Forest

library(randomForest)
library(ROCR)

printf <- function(...) cat(sprintf(...))

model_randomforest<-function(HeadlineWordsTrain, HeadlineWordsTest)
{
nytRF = randomForest(Popular ~ . - WordCount, data=HeadlineWordsTrain)

predictRF = predict(nytRF, newdata=HeadlineWordsTrain)
predROCR = prediction(predictRF, HeadlineWordsTrain$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc = performance(predROCR, "auc")@y.values
printf("auc train %f", auc)
nytRF
}

nytRF = model_randomforest(HeadlineWordsTrain, HeadlineWordsTest)


# Test Data
PredTest = predict(nytRF, newdata=HeadlineWordsTest)
PredTest[PredTest<0] = 0

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

