library(tm)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

library(wordcloud)
negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(negativeTweets), colSums(negativeTweets), colors=brewer.pal(9, "Blues") )

install.packages("RColorBrewer")
library(RColorBrewer)