# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

set.seed(1000)
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

normalize <- function(input) {
  input$IsBusiness = input$NewsDesk == "Business"
  input$IsCulture = input$NewsDesk == "Culture"
  input$IsScience = input$NewsDesk == "Science"
  input$IsStyles = input$NewsDesk == "Styles"
  input$IsMetro = input$NewsDesk == "Metro"
  input$IsOpEd = input$NewsDesk == "OpEd"
  
  input$IsOpinion = input$SectionName == "Opinion"
  input$IsDay     = input$SectionName == "Day"
  input$IsArts    = input$SectionName == "Arts"
  input$IsGames   = input$SectionName == "Games"
  input$ISHealth  = input$SectionName == "Health"
  input$IsTechnology = input$SectionName == "Technology"
  input$IsUS         = input$SectionName == "U.S."
  
  input$IsDealbook = input$SubsectionName == "Dealbook"
  
  input$Date = strptime(input$PubDate, format="%Y-%m-%d %H:%M:%S")
  input$Weekday = weekdays(input$Date)
  input$Hour = input$Date$hour
  
  input$WordCount = as.integer(input$WordCount/100)
  input$LessThan2000 = input$WordCount<=20
  
  input$is6to22 = input$Hour>=6 & input$Hour<=22
  input$isSat = input$Weekday == "Saturday"
  input$isSun = input$Weekday == "Sunday"
  
  input
}

NewsTrain = normalize(NewsTrain)
NewsTest = normalize(NewsTest)

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

updatevar <- function(v1, v2) {
  v1$LessThan2000 = v2$LessThan2000
  v1$IsOpEd = v2$IsOpEd

  v1$IsBusiness = v2$IsBusiness
  v1$IsCulture = v2$IsCulture
  v1$IsScience = v2$IsScience
  v1$IsStyles = v2$IsStyles
  v1$IsMetro = v2$IsMetro
  
  v1$IsOpinion = v2$IsOpinion
  v1$IsDay     = v2$IsDay
  v1$IsArts    = v2$IsArts
  v1$IsGames   = v2$IsGames
  v1$ISHealth  = v2$IsHealth
  v1$IsTechnology = v2$IsTechnology
  v1$IsUS         = v2$IsUS
  
  v1$IsDealbook = v2$IsDealbook
  
  v1$Weekday = as.factor(v2$Weekday)
  v1$Hour = as.factor(v2$Hour)
  
  v1
}

HeadlineWordsTest  = updatevar(HeadlineWordsTest, NewsTest)
HeadlineWordsTrain = updatevar(HeadlineWordsTrain, NewsTrain)

HeadlineWordsTrain$Popular = NewsTrain$Popular
# Random Forest

library(randomForest)
library(ROCR)

printf <- function(...) cat(sprintf(...))

model_randomforest<-function(HeadlineWordsTrain, HeadlineWordsTest)
{
nytRF = randomForest(Popular ~ ., data=HeadlineWordsTrain)

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

