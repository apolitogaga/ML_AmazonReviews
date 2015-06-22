docs <- c("This is a text.", "This another one.")
VCorpus(VectorSource(docs))
reut21578 <- system.file("texts", "crude", package = "tm") 
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain))
writeCorpus(ovid)
writeCorpus(corpus)

inspect(corpus[1:2])
inspect(mycorpus[1:2])
inspect(ovid[1:2])
meta(corpus[[2]], "id")

meta(ovid[[2]], "id")

identical(ovid[[2]], ovid[["ovid_2.txt"]])
identical(corpus[[2]], corpus[["2"]])

writeLines(as.character(corpus[[2]]))
writeLines(as.character(ovid[[2]]))

reuters <- tm_map(reuters, stripWhitespace)
reuters <- tm_map(reuters, content_transformer(tolower))
reuters <- tm_map(reuters, removeWords, stopwords("english"))
tm_map(reuters, stemDocument)
idx <- meta(reuters, "id") == '237' &
  meta(reuters, "heading") == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'
reuters[idx]


sk <- stringdot(type = "spectrum", length = 5)
sc<-specc(training.df, centers = 10, kernel = sk)
centers(sc)
size(sc)
withinss(sc)
plot(tess, col=sc)
