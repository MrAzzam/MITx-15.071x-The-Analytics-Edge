songs = read.csv("songs.csv")
songstrain = subset(songs, year<=2009)
songstest = subset(songs, year==2010)sum
SongsTrain = songstrain
SongsTest = songstest
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
testPredict = predict(SongsLog3, newdata=SongsTest, type="response")

table(SongsTest$Top10, testPredict >= 0.45)
