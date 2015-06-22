install.packages('devtools')
devtools::install_github('rstudio/shinyapps')
shinyapps::setAccountInfo(name='apolitogaga',
                          token='2EE7EE03D1D464654B98138655E4BB15',
                          secret='4icOY2Bl67ysV8uu+OJ4pkUix6CFNX++s8YwC4FV')

library(shinyapps)
shinyapps::deployApp('/Users/hectorapolorosalespulido/Documents/dev/github/ML_AmazonReviews/')
install.packages("shiny")
library(shiny)

require(ggplot2)
library(wordcloud)
library("tm")
library(SnowballC)

data(reuters)
reuters
cs <- as.character(data)
dschar <- VectorSource(cs)
ds <- VectorSource(data)
corpus <- Corpus(ds)
corpus2 <- Corpus(dschar)
dtm <- DocumentTermMatrix(corpus)  
dtm2 <- DocumentTermMatrix(corpus2)
row.names(data[,1])
row.names(data[1,])
i=1
for(i in 1:nrow(data))
  row.names(data[i,]) <- data[i,10001]

rownames(data)[1] <- data[i,10001]
row.names(data[i,]) <- data[i,10001]
data[,10001]
nammm<-c(data[,10001])
row.names<-

corpus[[1]]
tdm <- TermDocumentMatrix(corpus)   
tdm
inspect(tdm)
inspect(dtm)
inspect(dtm2)
freq <- colSums(as.matrix(dtm)) 
freq2 <- colSums(as.matrix(dtm2))
length(freq)   
ord <- order(freq)   
ord2 <- order(freq2)   

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms) 

freq[tail(ord)]
freq[tail(ord2)]
library(ggplot2)



freq <- sort(colSums(as.matrix(data[-10001])), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(data, freq>50), aes(word, freq))
p <-p+ geom_bar(stat="identity")
p <- p+ theme(axis.text.x=element_text(angle=45,hjust=1))

library(wordcloud)
wordcloud(names(freq), freq, min.freq=150, colors=brewer.pal(6, "Dark2"))

plot(dtm,terms=findFreqTerms(dtm, lowfreq=1)[1:5],corThreshold=0.01)
plot(corpus,terms=findFreqTerms(dtm, lowfreq=1)[1:5],corThreshold=0.01)

inspect(ds[1:5, 1:20])

library(kernlab)
data(spirals)
sc <- specc(spirals, centers=2)
sc
centers(sc)
size(sc)
withinss(sc)
plot(spirals, col=sc)


tdm.onlytags <- tdm[rownames(tdm)%in%TagSet$tag,]


mycorpus <- Corpus(VectorSource())

sData <- specc(train10.df[-10001], centers=10)
centers(sc)
size(sc)
withinss(sc)
plot(spirals, col=sc)