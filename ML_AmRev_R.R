install.packages("foreign")
library(foreign)

str(data)
#### UNIX COMMMAND TO REMOVE THE CTRL character from the file.###########
## cat -v data/amRevData.arff | sed 's/\^A/\t/g' > data/test.arff 
dataFFF <- read.arff("data/cleanedAmazon.arff")
data2 <- read.arff("data/authoS/amazon-commerce-reviews.arff")#Amazon_initial_50_30_10000
data3 <- read.arff("data/authorDS/charactersdataset_20_50.arff")
data4 <- read.arff("data/authorDS/charactersdataset_30_50.arff")

levels(data3$class)
plot(data3$class)
head(data3)


read.arff("data/wise2014-train.arff")
data<- read.arff("data/amazon-commerce-reviews.arff")
attributes(data)
names(data)
plot(data$Class)
class<-data[,10001]
dim(data)

v <- sort(colSums(matrixdata),decreasing=TRUE)
plot(log(v))
d <- data.frame(word = colnames(data[,-10001]),freq=v/sum(v))
wordcloud(d$word,d$freq,scale=c(8,.5),max.words=100, random.order=FALSE)
plot(data[,10000],data[,10001])

library(tm)

matrixdata<-as.matrix(data[-10001])
# Now we weight by inverse document frequency
data.tfidf <- tfidf.weight(matrixdata)
# and normalize by vector length
data.tfidf <- div.by.euc.length(data.tfidf)

dim(data.tfidf)
summary(colSums(data.tfidf))

# remove those words whose total sum is not greater than the third quartile of the distribution

(r.3rdQ <- summary(colSums(data.tfidf))[5])

data.tfidf.2 <- subset.data.frame (data.tfidf, 
                                         select=colSums(data.tfidf)>r.3rdQ)
dim(data.tfidf.2)






