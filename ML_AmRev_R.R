install.packages("foreign")
library(foreign)

read.arff("data/wise2014-train.arff")
data<- read.arff("data/amazon-commerce-reviews.arff")

data2 <- read.arff("data/authorDS/Amazon_initial_50_30_10000.arff")#Amazon_initial_50_30_10000
data3 <- read.arff("data/authorDS/charactersdataset_20_50.arff")
data4 <- read.arff("data/authorDS/charactersdataset_30_50.arff")


?read.arff
getwd()
