
# Load the dataset

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)

# Look at emails

wiki$Added[1]
wiki$Vandal[1]
wiki$Removed[1]
wiki$Added[2]
wiki$Vandal[2]
wiki$Removed[2]


table(wiki$Vandal)

# Load tm package

library(tm)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]

# Pre-process data
#corpus = tm_map(corpus, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package,
# you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). 
# This is a recent change having to do with the tolower function that 
# occurred after this video was recorded.
#corpus = tm_map(corpus, PlainTextDocument)
#corpus = tm_map(corpus, removePunctuation)

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
# Look at first email
corpusAdded[[1]]
# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# Remove sparse terms
dtmAdded = removeSparseTerms(dtmAdded, 1-0.3/100)
dtmAdded
# Create data frame
wordsAdded = as.data.frame(as.matrix(dtmAdded))
wordsAdded
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Build words removed
# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]

# Pre-process data
#corpus = tm_map(corpus, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package,
# you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). 
# This is a recent change having to do with the tolower function that 
# occurred after this video was recorded.
#corpus = tm_map(corpus, PlainTextDocument)
#corpus = tm_map(corpus, removePunctuation)

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
# Look at first email
corpusRemoved[[1]]
# Create matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
# Remove sparse terms
dtmRemoved = removeSparseTerms(dtmRemoved, 1-0.3/100)
dtmRemoved
# Create data frame


wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal=wiki$Vandal
str(wikiWords)

# Video 5


# Split the data

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data=train, method="class")
prp(wikiCART)

# Video 6

# Make predictions on the test set

pred = predict(wikiCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]

# Compute accuracy
table(test$Vandal, pred.prob >= 0.5)
table(test$Vandal)
#215/(215+42)

# wikiWords2 

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART = rpart(Vandal~., data=wikiTrain2, method="class")
prp(wikiCART)
pred = predict(wikiCART, newdata=wikiTest2)
pred[1:10,]
pred.prob = pred[,2]

table(wikiTest2$Vandal, pred.prob >= 0.5)
table(wikiTest2$Vandal)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiCART3)
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)


