install.packages("foreign")
library(foreign)
library(ggplot2)
library(wordcloud)
library("tm")
library(SnowballC)
library(kernlab)
library(ngram)
v
str(data)
#### UNIX COMMMAND TO REMOVE THE CTRL character from the file.###########
## cat -v data/amRevData.arff | sed 's/\^A/\t/g' > data/test.arff 

#data <- read.arff("data/cleanedAmazon.arff")
#saveRDS(data,"data/initialData.rds")
data <- readRDS("data/initialData.rds")
wholeData <- readRDS("data/initialData.rds")

data10

############### Separate test/training data ###############

# we have a variable class, which is the reviewer[factor], and another one which is the class keyword which is numerical
names(data)[10001] <- "clazz" # change the factor class to clazz
data$clazz
clazzN <- 30
NumClasses <- 10

data10 <- data[1:(clazzN*NumClasses),]
table(data10$clazz)
data10$clazz <- as.character(data10$clazz)
data10$clazz <- as.factor(data10$clazz)
levels(data10$clazz) <- c("Agresti","Ashbacher","Auken", "Blankenship", "Brody", "Brown","Bukowsky","CFH","Calvinnme","Chachra")
table(data10$clazz)
str(data10$clazz)
##  FROM: http://stackoverflow.com/questions/13536537/partitioning-data-set-in-r-based-on-multiple-classes-of-observations
# sample 67 training rows within classification groups
training.rows <-
  tapply( 
    # numeric vector containing the numbers
    # 1 to nrow( x )
    1:nrow( data10 ) , 
    
    # break the sample function out by
    # the classification variable
    data10$clazz , 
    
    # use the sample function within
    # each classification variable group
    sample , 
    
    # send the size = 67 parameter
    # through to the sample() function
    size = 25 
  )

# convert list back to a numeric vector
tr <- unlist( training.rows )

# split original data frame into two:

# all the records sampled as training rows
training10.df <- data10[ tr , ]

# all other records (NOT sampled as training rows)
testing10.df <- data10[ -tr , ]
######## Checkpoint 2 get the data.
#saveRDS(training10.df,"data/training10Data.rds")
#saveRDS(testing10.df,"data/testing10Data.rds")
training.df <- readRDS("data/trainingData.rds")
testing.df <- readRDS("data/testingData.rds")
train10.df <- readRDS("data/training10Data.rds")
test10.df <- readRDS("data/testing10Data.rds")
######## end the merging of data



levels(data$clazz)
plot(data$clazz)
head(data)
names(data)

target.reviewer<-data[,10001]
dim(data)

data <- train10.df
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
?specc


tess


data.tfidf.2$._W
class(data.tfidf.2$target.reviewer)
data.tfidf.2[2366]
formula <- data.tfidf.2$target.reviewer~data.tfidf.2$this
sData <- specc(formula, centers=10,data=data.tfidf.2)
centers(sData)
size(sData)
withinss(sData)
plot(data.tfidf.2[-2366], col=sData)

tess <- data.tfidf.2[,1:50]
tess <- data.frame(tess,data.tfidf.2[2366])

specc(tess, centers,
      kernel = "stringdot", kpar = list(length=4, lambda=0.5),
      nystrom.red = FALSE, nystrom.sample = length(tess)/6,
      iterations = 200, mod.sample = 0.75, na.action = na.omit)



