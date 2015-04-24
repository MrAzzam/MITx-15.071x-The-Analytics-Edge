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
  
  input$WordCount = as.integer(input$WordCount/100)
  input$LessThan2000 = input$WordCount<=20
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

HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$LessThan2000 = NewsTrain$LessThan2000
HeadlineWordsTrain$IsOpEd = NewsTrain$IsOpEd
HeadlineWordsTrain$IsOpinion = NewsTrain$IsOpinion

HeadlineWordsTest$LessThan2000 = NewsTest$LessThan2000
HeadlineWordsTest$IsOpEd = NewsTest$IsOpEd
HeadlineWordsTest$IsOpinion = NewsTest$IsOpinion

# Random Forest

library(randomForest)
library(ROCR)

printf <- function(...) cat(sprintf(...))
nytRF = randomForest(Popular ~ ., data=HeadlineWordsTrain)

predictRF = predict(nytRF, newdata=HeadlineWordsTrain)
predROCR = prediction(predictRF, HeadlineWordsTrain$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc = performance(predROCR, "auc")@y.values

printf("auc train %f", auc)

# Test Data
PredTest = predict(nytRF, newdata=HeadlineWordsTest)
PredTest[PredTest<0] = 0

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

