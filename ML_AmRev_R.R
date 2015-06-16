install.packages("foreign")
 
library(foreign)
install.packages("RWeka")
library(RWeka)

read.arff("data/wise2014-train.arff")
data<- read.arff("data/amazon-commerce-reviews.arff")

data2 <- read.arff("data/amRevData1.arff")#Amazon_initial_50_30_10000
?read.arff
getwd()
