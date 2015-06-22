library(randomForest)
library(TunePareto)
library(tree)
library(psych)
library(MASS)
library(wordcloud)
library("tm")
library(SnowballC)

harm <- function (a,b) { 2/(1/a+1/b) }
harmDiagProb <-function(x){
  var=1
  i=0
  for(i in i:nrow(x) )
    var[i] <- prop.table(x,1)[i,i]
  return(as.numeric(var))
}
prop.table(ct,1)[1,1]


var=matrix(nrow=1,ncol=nrow(ct))
i=0
for(i in i:nrow(ct) )
  var[i] = prop.table(ct,1)[i,i]
return(var) 

######################################################################## 
############## stuff about the data we have#######
########################################################################
plot(wholeData)
i=1
?matrix
var = matrix(nrow=1500,ncol = 1)
var2=1
for(i in 1:10000) var2[i] <- sum(wholeData[,i])
plot(log(var2))

wholeData[1:3,5928:5930]

data1<-cbind(wholeData[1:3558], wholeData[10001])
data2<-cbind(wholeData[3559:5927], wholeData[10001])
data3<-cbind(wholeData[5928:6566], wholeData[10001])
data4<-wholeData[6567:ncol(wholeData)]


v <- sort(colSums(data2[-ncol(data2)]),decreasing=TRUE)
plot(log(v))
d <- data.frame(word = colnames(data2[,-nrow(data2)]),freq=v/sum(v))
wordcloud(d$word,d$freq,scale=c(8,.5),max.words=100, random.order=FALSE)


wordcloud(data3,d$freq,scale=c(8,.5),max.words=100, random.order=FALSE)
doTheCloud <- function(data2,rand=FALSE){
  v <- sort(colSums(data2[-ncol(data2)]),decreasing=TRUE)
  pal2 <- brewer.pal(8,"Dark2")
  d <- data.frame(word = colnames(data2[,-ncol(data2)]),freq=v/sum(v))
  wordcloud(d$word,d$freq,scale=c(8,.3), random.order=rand, color=pal2)
}

doTheCloud(datacomplete, TRUE)



doTheCloud(data1)
doTheCloud(data2)
doTheCloud(data3)
doTheCloud(data4)

var=1
for(i in 1:ncol(sz)) var[i] <- sum(sz[,i])
plot(var)
sum(sz[1])

sum(sz[2])
sum(sz[3])

var[6550:6600,]

var<- as.data.frame(var)
hist(as.matrix(var))

plot(as.matrix(var[3000:4000,]))
plot(log(as.matrix(var)))
tess[10001]





################################################################################################################################################
################################################################################################################################################
################################################################# get the data #################################################################
tess <- train10.df
test <- test10.df

train10.df$clazz

tess <- train10.df[1:600]
test <- test10.df[1:600]
tess[601] <- train10.df$clazz
test[601] <- test10.df$clazz
names(tess)[601] <- "clazz" # change the factor class to clazz

tess$clazz

var <- colnames(train10.df[1:600])
ncol(tess)
      

names(tess)<-sprintf("V%d",1:ncol(tess))
names(tess)<-var

names(test)<-sprintf("V%d",1:ncol(tess))
names(tess)<-var


model.rf2 <- randomForest(V601 ~ ., data = tess, ntree=100, proximity=TRUE)
pred.rf1 <- predict (model.rf1, test, type="class")

(ct <- table(Truth=test$V601, Pred=pred.rf1))
prop.table(ct, 1)

sum(diag(ct))/sum(ct)
# error rate in %
round(100*(1-sum(diag(ct))/sum(ct)),2)
var <- harmDiagProb(ct)
harmonic.mean(var[1:12])
var[13]
var <-  as.matrix(var)
mean(var)
?harmonic.mean
(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))
model.plot <- MDSplot(model.rf1, tess$V601,pch=as.numeric(tess$V601))
harm(prop.table(ct,1)[1,1], prop.table(ct,1)[2,2])

################################################################################################################################################
(ntrees <- round(10^seq(1,4,by=0.4)))
rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0


ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(V601 ~ ., data = tess, ntree=nt, proximity=FALSE, strata=tess$V601)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}

rf.results

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])




myForest <- randomForest(V601 ~ ., data = tess, ntree=2512, proximity=TRUE,importance=TRUE)

model.plot <- MDSplot(myForest, tess$V601,pch=as.numeric(tess$V601))
plot(myForest$importance[,1])
#plot fun 
install.packages("ggRandomForests")
library(ggRandomForests)
library(ggplot2)
theme_set(theme_bw())
plot(gg_vimp(myForest), lbls=st.labs)
################################################################################################################################################
################################################################################################################################################
################################################################# LDA #################################################################
kk<-c(453,427,564)
model.lda = lda(V601 ~ . ,data = tess[-kk])
pred.lda = predict(model.lda, test, type="class")
pres.lda.revw <- predict(model.lda, test)$class
table(test, pred.lda)
ldatable <- table(Truth=test$V601, Pred=pres.lda.revw)
prop.table(ldatable,1)
sum(diag(ldatable))/sum(ldatable)
round(100*(1-sum(diag(ldatable))/sum(ldatable)),2)
harmonic.mean(harmDiagProb(ldatable))

