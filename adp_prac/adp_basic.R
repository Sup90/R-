d<-subset(iris,Species=="virginica"|Species=="versicolor")
str(d)
d
d$Species<-factor(d$Species)
str(d)
(m<-glm(Species~.,data=d,family = "binomial"))
fitted(m)[c(1:5,51:55)]
f<-fitted(m)
d
d$Species<-as.numeric(d$Species)
str(d$Species)
unique(d$Species)
ifelse(f>.5,1,0)==as.numeric(d$Species)-1
is_correct<-(ifelse(f>.5,1,0)==as.numeric(d$Species)-1)
sum(is_correct)
sum(is_correct)/NROW(is_correct)
predict(m,newdata = d[c(1,10,55),],type="response")
library(nnet)
(m<-multinom(Species~.,data=iris))
head(fitted(m))
predict(m,newdata = iris[c(1,51,101),],type="class")
predict(m,newdata = iris[c(1,51,101),],type="probs")
predict(m,newdata = iris,type="probs")

#모델의 정확도는 예측된 Species와 실제 Species 를 비교
predicted<-predict(m,newdata = iris)
predicted
sum(predicted==iris$Species)/NROW(predicted)
xtabs(~predicted+iris$Species)

?xtabs

install.packages("rpart")
library(rpart)
(m<-rpart(Species~.,data=iris))
plot(m,compress = T,margin = .2)
text(m,cex=1.5)
install.packages("rpart.plot")
library(rpart.plot)
prp(m,type=4,extra = 2,digits=3)
head(predict(m,newdata=iris,type="class"))
install.packages("party")
library(party)
(m<-ctree(Species~.,data = iris))
plot(m)
