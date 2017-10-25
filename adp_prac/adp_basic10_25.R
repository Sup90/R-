install.packages("randomForest")
library(randomForest)
m<-randomForest(Species~.,data=iris)
m
a<-predict(m,newdata = iris)
table(iris$Species,a)
m<-randomForest(iris[,1:4],iris[,5])
m
m<-randomForest(Species~.,data=iris,importance=T)
m
importance(m)
varImpPlot(m,main="varImPlot of iris")
(grid<-expand.grid(ntree=c(10,100,200),mtry=c(3,4)))
library(cvTools)
install.packages("cvTools")
library(foreach)
set.seed(719)
K=10
R=3
cv<-cvFolds(NROW(iris),K=K,R=R)

?cvFolds
cv
str(cv)
?cvFolds
grid<-expand.grid(ntree=c(10,100,200),mtry=c(3,4))
result<-foreach(g=1:NROW(grid),.combine = rbind)%do% {
  foreach(r=1:R,.combine = rbind)%do% {
    foreach(k=1:K,.combine = rbind)%do% {
      validation_idx<-cv$subsets[which(cv$which==K),r]
      train<-iris[-validation_idx,]
      validation<-iris[validation_idx,]
      #¸ðµ¨ÈÆ·Ã
      m<-randomForest(Species~.,data=train,ntree=grid[g,"ntree"],
                      mtry=grid[g,"mtry"])
      #¿¹Ãø
      predicted<-predict(m,newdata = validation)
      #¼º´ÉÆò°¡
      precision<-sum(predicted==validation$Species)/NROW(predicted)
      return(data.frame(g=g,precision=precision))
      }
  }
}
result
library(plyr)
ddply(result,.(g),summarize,mean_precision=mean(precision))
grid[c(4,6),]
library(nnet)
m<-nnet(Species~.,data=iris,size=3)#Àº´ÐÃþ ³ëµå=3
predict(m,newdata = iris)

predict(m,newdata = iris,type="class")
class.ind(iris$Species)
m2<-nnet(iris[,1:4],class.ind(iris$Species),size=3,softmax = T)
predict(m2,newdata = iris[,1:4],type="class")
iris$Species==predict(m2,newdata = iris[,1:4],type="class")
table(iris$Species,predict(m2,newdata = iris[,1:4],type="class"))
install.packages("kernlab")
library(kernlab)
(m<-ksvm(Species~.,data=iris))
head(predict(m,newdata=iris))
ksvm(Species~.,data=iris,kernel="vanilladot")
(m<-ksvm(Species~.,data=iris,kernel="polydot",kpar=list(degree=3)))
help(tune)
??tune
install.packages("e1071")
library(e1071)
tune(svm, Species~., data = iris, 
     gamma = 2^(-1:1), cost = 2^(2:4))
attributes(result)
library(mlbench)
data("BreastCancer")
table(BreastCancer$Class)
x<-upSample(subset(BreastCancer,select =-Class),BreastCancer$Class)
library(caret)
table(BreastCancer$Class)
table(x$Class)
NROW(x)
NROW(unique(BreastCancer))
NROW(unique(x))

x
