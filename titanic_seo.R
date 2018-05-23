getwd()
dat<-read.csv("Data/titanic/titanic3.csv",)
head(dat)
dim(dat)
str(dat)
dat1<-dat[,!names(dat) %in% c("home.dest","boat","body")]
head(dat1)
View(dat1)
str(dat1)
dat1[,c(1,3,8,10)]<-as.factor(dat1[,c(1,3,8,10)])
#안되는 코드
str(dat1)                                  
dat1$pclass<-as.factor(dat1$pclass)
dat1$name<-as.character(dat1$name)
dat1$ticket<-as.character(dat1$ticket)
dat1$cabin<-as.character(dat1$cabin)
dat1$survived<-factor(dat1$survived,levels = c(1,0),labels = c("dead","survived"))
str(dat1)
dat1$sex<-as.factor(dat1$sex)
str(dat1)
levels(dat1$embarked)
table(dat1$embarked)
levels(dat1$embarked)[1]<-NA
table(dat1$embarked,useNA = "always")
dat1$cabin<-ifelse(dat1$cabin=="",NA,dat1$cabin)
?ifelse()
str(dat1)
dat1<-dat1[,-12]
library(caret)
install.packages("caret")
set.seed(137)
test_idx<-createDataPartition(dat1$survived,p=0.1)$Resample1
dat1.test<-dat1[test_idx,]
dat1.train<-dat1[-test_idx,]
nrow(dat1.test)
prop.table(table(dat1.test$survived))
prop.table(table(dat1.train$survived))
createDataPartition(dat1$survived,p=0.1)$Resample1
save(dat1,dat1.test,dat1.train,file="titanic.RData")
head(Titanic)
str(Titanic)
createFolds(dat1.train$survived,k=10)
create_ten_fold_cv<-function(){
  set.seed(137)
  lapply(createFolds(dat1.train$survived,k=10),function(idx){
    return(list(train=dat1.train[-idx,],validation=dat1.train[idx,]))
  } )
}
x<-create_ten_fold_cv()
str(x$Fold01$train)
str(x$Fold01$validation)
head(x[1])
x[[1]]
install.packages("Hmisc")
library(Hmisc)
data<-create_ten_fold_cv()[[1]]$train
str(data)
summary(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=data,method = "reverse")
summary(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=data)
?Hmisc:summary
??Hmisc
data.complete<-data[complete.cases(data),]
featurePlot(
  data.complete[,
                sapply(names(data.complete),
                        function(n){
                          is.numeric(data.complete[,n])
                        })],data.complete[,c("survived")],
  "ellipse",labels = "survived"
                      
)
?featurePlot
install.packages("ellipse")
library(ellipse)
mosaicplot(survived~pclass+sex,data=data,color=T,main="pclass and sex")
1install.packages("mosaicplot")
??mosaicplot
install.packages("graphics")
library(graphics)
xtabs(~sex+pclass,data=data)
xtabs(survived=="survived"~sex+pclass,data=data)
xtabs(survived=="survived"~sex+pclass,data=data)/xtabs(~sex+pclass,data=data)
predicted<-c(1,0,0,1,1)
actual<-c(1,0,0,0,0)
sum(predicted==actual)/NROW(predicted)
library(rpart)
m<-rpart(
  survived~pclass+sex+sibsp+parch+fare+embarked,
  data=dat1.train)
p<-predict(m,newdata=dat1.train,type="class")
head(p)

library(foreach)
folds<-create_ten_fold_cv()
rpart_result<-foreach(f=folds) %do% {
  model_rpart<- rpart(
    survived~pclass + sex + age + sibsp + parch + fare + embarked,
    data=f$train
  )
  predicted<-predict(model_rpart,newdata=f$validation,
                       type="class")
  return(list(actual=f$validation$survived, predicted=predicted))
}
length(rpart_result[[1]]$predicted)
length(rpart_result[[1]]$actual)
evaluation<-function(lst){
  accuracy<-sapply(lst,function(one_result){
    return(sum(one_result$predicted==one_result$actual)/NROW(one_result$actual))
  })
  print(sprintf("MEAN+/-SD: %.3f+/-%.3f",
                mean(accuracy),sd(accuracy)))
        return(accuracy)
}
(rpart_accuracy<-evaluation(rpart_result))
install.packages("party")
library(party)
ctree_result<-foreach(f=folds) %do% {
  model_ctree<-ctree(
    survived ~ pclass + sex + age + sibsp + parch +fare + embarked,
    data=f$train)
  predicted<-predict(model_ctree,newdata=f$validation,
                     type="response")
  return(list(actual=f$validation$survived,predicted=predicted)
  )
}
(ctree_accuracy<-evaluation(ctree_result))
plot(density(rpart_accuracy),main="rpart VS ctree")
lines(density(ctree_accuracy),col="red",lty="dashed")
View(dat1.train[order(dat1.train$ticket),
                c("ticket","parch","name","cabin","embarked")])




sum(is.na(dat1.train$ticket))
sum(is.na(dat1.train$embarked))
sum(is.na(dat1.train$cabin))
nrow(dat1)

family_result<-foreach(f=folds) %do%{
  f$train$type<-"T"
  f$validation$type<-"V"
  all<-rbind(f$train,f$validation)
  ctree_model<-ctree(
    survived~pclass+sex+age+sibsp+parch+fare+
      embarked,
    data=f$train
  )
  all$prob<-sapply(
    predict(ctree_model,type="prob",newdata=all),
    function(result){result[1]}
  )
}
family_idx<-0
ticket_based_family_id<-ddply(all,.(ticket),function(rows){
  family_idx<<-family_idx+1
  return(data.frame(family_id=paste0("TICKET_",family_idx)))

})
library(dplyr)
library(plyr)
nrow(distinct(all,ticket))
?distinct
nrow(all)
library()
browser(ticket_based_family_id)
detach(
  "package:dplyr",unload = T)
detach_package(dplyr)
unloadNamespace("dplyr")
search()
browser()
testthat
str(ticket_based_family_id)
head(ticket_based_family_id)
levels(ticket_based_family_id$family_id)
all<-adply(all,1,function(row){
  family_id<-NA
  if(!is.na(row$ticket)){
    family_id<-subset(ticket_based_family_id,
                      ticket==row$ticket)$family_id
  }
  return(data.frame(family_id=family_id))
})
str(all)
head(ticket_based_family_id)
all<-ddply(all,.(family_id),function(rows){
  rows$avg_prob<-mean(rows$prob)
  return(rows)
})
ddply(all,.(family_id))
distinct(all,family_id)
nrow(all)
head(all)
all<-ddply(all,.(family_id),function(rows){
  rows$maybe_parent<-F
  rows$maybe_child<-F
  if(NROW(rows)==1||
     sum(rows$parch)==0||
     nrow(rows)==sum(is.na(rows$age)))
  {return(rows)
}
  max_age<-max(rows$age,na.rm=T)
  min_age<-min(rows$age,na.rm=T)
  return(adply(rows,1,function(row){
    if(!is.na(row$age)&&!is.na(row$sex)){
    row$maybe_parent<-(max_age-row$age)<10
    row$maybe_child<-(row$age-min_age)<10
    }
    return(row)
  }))
  })

all<-ddply(all,.(family_id),function(rows){
  rows$avg_parent_prob<-rows$avg_prob
  rows$avg_child_prob<-rows$avg_prob
  if(NROW(rows)==1||sum(rows$parch)==0){
    return(rows)
  }
  parent_prob<-subset(rows,maybe_parent==T)[,"prob"]
  if(NROW(parent_prob)>0){
    rows$avg_parent_prob<-mean(parent_prob)
  }
  child_prob<-c(subset(rows,maybe_child==T)[,"prob"])
  if(NROW(child_prob)>0){
    rows$avg_child_prob<-mean(child_prob)
    
  }
  rows
  })

str(all)
f$train<-subset(all,type=="T")
f$validation<-subset(all,type=="V")
(m<-ctree(survived~pclass+sex+age+sibsp+parch+fare
          +embarked+maybe_parent+maybe_child+age+
            sex+avg_prob+avg_parent_prob+avg_child_prob,
          data=f$train
          ))
print(m)
predicted<-predict(m,newdata=f$validation)

family_accuracy<-evaluation(family_result)
