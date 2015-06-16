install.packages("foreign")
library(foreign)

str(data)
#### UNIX COMMMAND TO REMOVE THE CTRL character from the file.###########
## cat -v data/amRevData.arff | sed 's/\^A/\t/g' > data/test.arff 
data <- read.arff("data/cleanedAmazon.arff")

levels(data$class)
plot(data$class)
head(data)


read.arff("data/wise2014-train.arff")
data<- read.arff("data/amazon-commerce-reviews.arff")
attributes(data3)
names(data)
plot(data$Class)
target.reviewer<-data[,10001]
dim(data)

matrixdata<-as.matrix(data[-10001])
v <- sort(colSums(matrixdata),decreasing=TRUE)
plot(log(v))
d <- data.frame(word = colnames(data[,-10001]),freq=v/sum(v))
wordcloud(d$word,d$freq,scale=c(8,.5),max.words=100, random.order=FALSE)
plot(data[,10000],data[,10001])

library(tm)


# Now we weight by inverse document frequency
data.tfidf <- tfidf.weight(matrixdata)
# and normalize by vector length
data.tfidf <- div.by.euc.length(data.tfidf)

dim(data.tfidf)
summary(colSums(data.tfidf))

#1. remove those words shorter than 3 characters

data.tfidf.2 <- subset.data.frame (data.tfidf, 
                                         select=sapply(colnames(data.tfidf), FUN=nchar)>2)
dim(data.tfidf.2)

#2. remove those words whose total sum is not greater than the third quartile of the distribution

(r.3rdQ <- summary(colSums(data.tfidf.2))[5])

data.tfidf.2 <- subset.data.frame (data.tfidf.2, 
                                         select=colSums(data.tfidf.2)>r.3rdQ)
dim(data.tfidf.2)
colnames(data.tfidf.2)[30:70]

# agregar target a los datos
data.tfidf.2<-data.frame(data.tfidf.2,target.reviewer)
data.tfidf.2$target.reviewer <- factor(data.tfidf.2$target.reviewer) 

#cambio de nombre ... a points3
colnames(data.tfidf.2)[colnames(data.tfidf.2) %in% c("...")]<-"points3"
data.tfidf.2$points3

#Relief
library(CORElearn)
estReliefF <- attrEval("target.reviewer",data.tfidf.2, estimator="ReliefFexpRank", ReliefIterations=1000)
print(sort(estReliefF,decreasing=TRUE))






