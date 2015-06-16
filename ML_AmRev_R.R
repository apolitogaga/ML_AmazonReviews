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
?read.arff
getwd()





##### Start the analysis ######
(N <- dim(reuters)[1])  # number of rows
(selected.tops <- tops[tops>N/l])



