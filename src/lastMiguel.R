#########################################################################################################
#                                                                                                       #
#                                                                                                       #
#                                        Barcelonatech                                                  #
#                                  Machine Learning Project                                             #
#                                Miguel Mossa & Apolo Rosales                                           #
#                                     DR. Lluís Belanche                                                #
#                                                                                                       #
#                                                                                                       #  
#########################################################################################################


library(foreign)
library(ggplot2)
library(wordcloud)
library("tm")
library(SnowballC)
library(kernlab)


#ANALYSIS EXPLORATORY AND PRE-PROCESSING
data<- train10.df


################################################################################################################################################
#---------AuxFunctions-------------------------------------------
################################################################################################################################################
harmDiagProb <-function(x){
  var=1
  i=0
  for(i in i:nrow(x) )
    var[i] <- prop.table(x,1)[i,i]
  return(as.numeric(var))
}

doTheCloud <- function(data2,rand=FALSE){
  v <- sort(colSums(data2[-ncol(data2)]),decreasing=TRUE)
  pal2 <- brewer.pal(8,"Dark2")
  d <- data.frame(word = colnames(data2[,-ncol(data2)]),freq=v/sum(v))
  wordcloud(d$word,d$freq,scale=c(8,.3), random.order=rand, color=pal2)
}


################################################################################################################################################
#---------Load and create test sets-------------------------------------------
################################################################################################################################################

data <- readRDS("data/initialData.rds")
wholeData <- readRDS("data/initialData.rds")

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



names(data)[10001] <- "clazz" # change the factor class to clazz
matrixdata1<-as.matrix(data[-10001])
library(tm)
library(wordcloud)


################################################################################################################################################
#---------Word Cloud PLOT-------------------------------------------
################################################################################################################################################


for(i in 1:10000) var2[i] <- sum(wholeData[,i])
plot(log(var2))
# the different sets in our dataset
data1<-cbind(wholeData[1:3558], wholeData[10001])
data2<-cbind(wholeData[3559:5927], wholeData[10001])
data3<-cbind(wholeData[5928:6566], wholeData[10001])
data4<-wholeData[6567:ncol(wholeData)]
doTheCloud(data1)
doTheCloud(data2)
doTheCloud(data3)
doTheCloud(data4)




tr <- row.names(training.df)
dim(data[tr,])
dim(data[-tr,])

v <- sort(colSums(matrixdata1),decreasing=TRUE)
plot(log(v))
d <- data.frame(word = colnames(data[,-10001]),freq=v/sum(v))
row.names(d)
wordcloud(row.names(d),v,scale=c(8,.5),max.words=300, random.order=FALSE)
plot(data[,10000],data[,10001])


#source('C:/Users/miguelmossa/Documents/Machine Learning - MIRI/L1pack-II/reuters-aux.R')
# Now we weight by inverse document frequency
data.tfidf1 <- tfidf.weight(matrixdata1)
# and normalize by vector length
data.tfidf1 <- div.by.euc.length(data.tfidf1)

dim(data.tfidf1)

#1. remove those words shorter than 3 characters

data.tfidf.3 <- subset.data.frame (data.tfidf1, 
                                   select=sapply(colnames(data.tfidf1), FUN=nchar)>2)

dim(data.tfidf.3)

#2. remove those words whose total sum is not greater than the third quartile of the distribution

(r.3rdQ <- summary(colSums(data.tfidf.3))[5])

data.tfidf.3 <- subset.data.frame (data.tfidf.3, 
                                   select=colSums(data.tfidf.3)>r.3rdQ)
dim(data.tfidf.3)

#cambio de nombre a la columna de clases
target.class1<-data$clazz

#cambio de nombre ... a points3
colnames(data.tfidf.3)[colnames(data.tfidf.3) %in% c("...")]<-"points3"

#asignacion de columna clases a matriz de frequencia inversa filtrada
datacomplete<-data.frame(data.tfidf.3,target.class1)


# FEATURE SELECTION AND EXTRACTION

#-----------PCA-------------------------------

library(FactoMineR)
PCAnew <- PCA(datacomplete,quali.sup=c(ncol(datacomplete)),ncp=500) # retain the first 500 PCs
summary(PCAnew)
plot(PCAnew$eig$eigenvalue,type="l",main="Screeplot")
#Psiframe<-data.frame(target.class1,PCAnew$ind$coord[,1:500])
Psiframe<-data.frame(target.class1,PCAnew$ind$coord)


#-----------Relief ----------------------------
library(CORElearn)
estReliefF <- attrEval("target.class1",datacomplete, estimator="ReliefFexpRank", ReliefIterations=500)
featuresranking<-sort(estReliefF,decreasing=TRUE)

View(as.data.frame(estReliefF))

colnames(estReliefF)
colnames(estReliefF)
View(row.names(estReliefF))
names(estReliefF)

doTheCloud(datacomplete)

pal <- brewer.pal(9, "BuGn")
#pal <- pal[-(1:2)]
wordcloud(names(estReliefF),estReliefF,scale=c(8,.3),max.words=300, random.order=FALSE,colors=pal)
# MODELS  

library(kernlab)

K=c(10,30,50,70,100,200,300,380,500) # dim size to use CV
Cp=10^seq(-2,3) # list of C's possible parameters


#---------SVM and PCA--------------------------

# initialize vectors to show the results
err <- numeric(length(Cp))
Cs<-numeric(length(K))
error.rate.test.Ondim<-numeric(length(K))
error.CV<-numeric(length(K))
SV<-numeric(length(K))


for(i in 1:length(K)){
  
  for(j in 1:length(Cp)){
    mi.svm <- ksvm (target.class1~.,data=Psiframe[tr,1:K[i]],C=Cp[j],cross=10)
    err[j]<-cross(mi.svm)
  }
  Cmin<-which.min(err)
  Cs[i]<-Cp[Cmin]
  error.CV[i]<-min(err)
  mi.svm.final <- ksvm (target.class1~.,data=Psiframe[tr,1:K[i]],C=Cp[Cmin],cross=10)
  
  SV[i]<-nSV(mi.svm.final) # Support vectors
  
  svmpred <- predict (mi.svm.final, Psiframe[-tr,2:K[i]])
  
  tt <- table(datacomplete[-tr,ncol(datacomplete)],svmpred)
  
  error_rate.test <- 100*(1-sum(diag(tt))/sum(tt))
  print(error_rate.test)
  error.rate.test.Ondim[i]<-error_rate.test  
}

#---------SVM and Relief-------------------------------------------

# initialize vectors to show the results
err2 <- numeric(length(Cp))
Cs2<-numeric(length(K))
error.rate.test.Ondim2<-numeric(length(K))
error.CV2<-numeric(length(K))
SV2<-numeric(length(K))

for(i in 1:length(K)){
  x<-cbind(datacomplete$target.class1,datacomplete[,names(datacomplete) %in% names(featuresranking)[1:K[i]]])
  colnames(x)[1] <- "target.class1"
  
  #Calculate the best C parameter
  for(j in 1:length(Cp)){
    mi.svm2 <- ksvm (target.class1~.,data=x[tr,],C=Cp[j],cross=10)
    err2[j]<-cross(mi.svm2)
  }
  Cmin<-which.min(err2)
  Cs2[i]<-Cp[Cmin] # the best C
  error.CV2[i]<-min(err2) # CV error with the best C
  
  mi.svm.final2 <- ksvm (target.class1~.,data=x[tr,],C=Cp[Cmin],cross=10) # recalculate the model using the best C parameter
  
  SV2[i]<-nSV(mi.svm.final2) # Support vectors
  
  svmpred2 <- predict (mi.svm.final2, datacomplete[-tr,-ncol(datacomplete)])
  
  tt <- table(datacomplete[-tr,ncol(datacomplete)],svmpred2)
  
  error_rate.test <- 100*(1-sum(diag(tt))/sum(tt))
  print(error_rate.test)
  error.rate.test.Ondim2[i]<-error_rate.test  
}
----------------------------------------------------------------------
library(psych)
harmDiagProb <-function(x){
  var=1
  i=0
  for(i in i:nrow(x) )
    var[i] <- prop.table(x,1)[i,i]
  return(as.numeric(var))
}




library(randomForest)
model.rf <- randomForest(target.class1 ~ ., data = x[tr,], ntree=100, proximity=FALSE, strata=tess$V601)
summary(x[tr,])
summary(x)
################################################################################################################################################
#---------RandomForest and PCA--------------------------
################################################################################################################################################
# initialize vectors to show the results
err3 <- numeric(length(Cp))
Cs3<-numeric(length(K))
error.rate.test.Ondim3<-numeric(length(K))
error.rate.harmonic3<-numeric(length(K))
error.CV3<-numeric(length(K))
#SV<-numeric(length(K))
K=c(1000,1500,2000) # dim size to use CV
(ntrees <- round(10^seq(1,4,by=0.4))) # ntrees posibilities


for(i in 1:length(K)){
  for(j in 1:length(ntrees)){
    #mi.svm <- ksvm (target.class1~.,data=Psiframe[tr,1:K[i]],C=Cp[j],cross=10)
    mi.rf <- randomForest(target.class1 ~ ., data = Psiframe[tr,1:K[i]], ntree=ntrees[j], proximity=FALSE, strata=x$target.class1)
    err3[j]<- mi.rf$err.rate[ntrees[j],1]
  }
  nTreemin <- which.min(err3)
  Cs3[i]<-ntrees[nTreemin]
  error.CV3[i]<-min(err3)
  mi.rf.final <- randomForest(target.class1 ~ ., data = Psiframe[tr,1:K[i]], ntree=ntrees[nTreemin], proximity=FALSE, strata=x$target.class1)
  #mi.svm.final <- ksvm (target.class1~.,data=Psiframe[tr,1:K[i]],C=Cp[Cmin],cross=10)
  
  #SV[i]<-nSV(mi.svm.final) # Support vectors
  
  #svmpred <- predict (mi.svm.final, Psiframe[-tr,2:K[i]])
  pred.rf <- predict (mi.rf.final, Psiframe[-tr,2:K[i]], type="class")
  
  (ct <- table(Truth=datacomplete[-tr,ncol(datacomplete)], Pred=pred.rf))
  #tt <- table(datacomplete[-tr,ncol(datacomplete)],svmpred)
  


  error_rate.test <- 100*(1-sum(diag(ct))/sum(ct))
  print(error_rate.test)
  error.rate.test.Ondim3[i]<-error_rate.test  
  error.rate.harmonic3[i]-harmonic.mean(harmDiagProb(ct)*100)
}


################################################################################################################################################
#---------RandomForest and Relief-------------------------------------------
################################################################################################################################################
# initialize vectors to show the results
err4 <- numeric(length(Cp))
Cs4<-numeric(length(K))
error.rate.test.Ondim4<-numeric(length(K))

error.rate.harmonic4<-numeric(length(K))
error.CV4<-numeric(length(K))
#SV2<-numeric(length(K))

for(i in 1:length(K)){
  x<-cbind(datacomplete$target.class1,datacomplete[,names(datacomplete) %in% names(featuresranking)[1:K[i]]])
  colnames(x)[1] <- "target.class1"
  #Calculate the best C parameter
  for(j in 1:length(ntrees)){
    #mi.svm2 <- ksvm (target.class1~.,data=x[tr,],C=Cp[j],cross=10)11
    mi.rf <- randomForest(target.class1 ~ ., data = x[tr,], ntree=ntrees[j], proximity=FALSE, strata=x$target.class1)
    err4[j]<- mi.rf$err.rate[ntrees[j],1]
    #err2[j]<-cross(mi.svm2)
  }
  nTreemin <- which.min(err4)
  Cs4[i]<-ntrees[nTreemin] # the best C
  error.CV4[i]<-min(err4) # CV error with the best C

  mi.rf.final2 <- randomForest(target.class1 ~ ., data = x[tr,], ntree=ntrees[nTreemin], proximity=FALSE, strata=x$target.class1)
  #mi.svm.final2 <- ksvm (target.class1~.,data=x[tr,],C=Cp[Cmin],cross=10) # recalculate the model using the best C parameter
  
  #SV2[i]<-nSV(mi.svm.final2) # Support vectors
  
  pred.rf2 <- predict (mi.rf.final2, x[-tr,], type="class")
  
  (ct <- table(Truth=datacomplete[-tr,ncol(datacomplete)], Pred=pred.rf2))
  
  #svmpred2 <- predict (mi.svm.final2, datacomplete[-tr,-ncol(datacomplete)])
  
  #tt <- table(datacomplete[-tr,ncol(datacomplete)],svmpred2)
  
  error_rate.test <- 100*(1-sum(diag(ct))/sum(ct))
  print(error_rate.test)
  error.rate.test.Ondim4[i]<-error_rate.test  
  error.rate.harmonic4[i] <- harmonic.mean(harmDiagProb(ct))
  
}
----------------------------------------------------------------------  

  # the results using SVM with PCA and Relief
  
Testing.PCA.SVM<-cbind(Cs,SV,error.rate.test.Ondim,error.CV,K)
colnames(Testing.PCA.SVM)<-c("OptC-SVM","SupportVectors","ErrorRateTest","CVerror","Dim(PCs)")
Testing.Relief.SVM<-cbind(Cs2,SV2,error.rate.test.Ondim2,error.CV2,K)
colnames(Testing.Relief.SVM)<-c("OptC-SVM","SupportVectors","ErrorRateTest","CVerror","Dim(RankingRelief)")


Testing.PCA.rf<-cbind(Cs3,error.rate.test.Ondim3,error.CV3,K)
colnames(Testing.PCA.rf)<-c("OptN-trees","ErrorRateTest","OOBError", "Dim(PCs)")

Testing.Relief.rf<-cbind(Cs4,error.rate.test.Ondim4,error.CV4,K)
colnames(Testing.Relief.rf)<-c("OptN-trees","ErrorRateTest","OOBError","Dim(RankingRelief)")


Testing.Relief.rf2<-cbind(Cs4,error.rate.test.Ondim4,error.CV4,K)
colnames(Testing.Relief.rf2)<-c("OptN-trees","ErrorRateTest","OOBError","Dim(RankingRelief)")

Testing.PCA.SVM
Testing.Relief.SVM
library(xtable)

import 
print(xtable(Testing.PCA.rf))

print(xtable(Testing.Relief.rf))
print(xtable(Testing.Relief.rf2))
