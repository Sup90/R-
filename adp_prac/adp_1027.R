getwd()
ls()
setwd("C:\\Users\\Subb\\Desktop\\R_work\\business\\R")
result<-read.csv("ad_result.csv",header = T,stringsAsFactors = F)
str(result)
plot(result$tvcm,y=result$install)
plot(result$magazine,result$install)
a<-lm(data = result,install~tvcm+magazine)
result$tvcm<-result$tvcm*10
result$magazine<-result$magazine*10
summary(a)
plot(a)
library(ggplot2)
ggplot(result,aes(x=tvcm,y=install))+geom_point()+xlab("TV 광고비")+
  ylab("신규 유저수")+scale_x_continuous(label=comma)+scale_y_continuous(labels = comma)
library(scales)
ggplot(result,aes(x=magazine,y=install))+geom_point()+xlab("잡지 광고비")+ylab("신규 유저수")+
  scale_x_continuous(labels = comma)+scale_y_continuous(labels = comma)
dau<-read.csv("section7-dau.csv",header = T,stringsAsFactors = F)
str(dau)
head(dau)
unique(dau$region_month)
library(reshape2)

dau<-read.csv("section7-dau.csv",stringsAsFactors = F,header = T)
head(dau07)


cust_id<-sample(20,size =50,replace = T )
cust_id
?sample
prod_id<-c(paste0("prod"+))
a<-rep("prod",50)
a
b<-sample(9,size=50,replace = T)
for i
b
prod_id<-paste(a,b,sep = "")
prod_id
a<-
?seq
a<-sample(1000,50,replace = T)
a
b<-sample(50,10)
b
a[b]<-(-a[b])
a
atm<-a
a
df<-data.frame(cust_id,prod_id,atm)
#dplyr의 mutate 와 ifelse 함수를 같이 쓰시면 될 것 같습니다. 
#가장 편한 방법은 custom function을 만든 뒤 ddply 함수를 쓰시면 될 듯 합니다.
install.packages("plyr")
library(plyr)
str(adply(iris[,1:4],2,function(row){print(row)}))
df
ddply(df,.(cust_id,prod_id),function(trans){ifelse(>0,1,0)})
ddply(df,.(cust_id),mutate,ifelse(atm>0,1,0))

ddply(df,.(prod_id,cust_id),mutate,ifelse(atm>0,1,0))
?mutate
mutate(airquality, Ozone = -Ozone)
melt(df,id.vars = df$cust_id,df$prod_id)
cust_id
df.st<-dcast(df,cust_id~prod_id,value.var = "atm",fun.aggregate = function(x)ifelse
      (sum(x)>0,1,0))
df.st
df[order(df$cust_id,rank(df$prod_id)),]
a<-read.csv("C:/Users/Subb/Desktop/Data_set.csv",header =T,stringsAsFactors = T)
cor(df.st,method = "pearson" )
cor(t,method="pearson")
str(df.st)
?melt

t<-melt(df.st,id=c("cust_id","prod_id"))
cor(t$cust_id,t$value,method = "pearson")

?cor
df$cust_id
ix<-sample(nrow(a),1000,replace = F)
df<-a[ix,]
df
par(c(1,1))
par(mfrow=c(1,1))
for(i in 3:14){ggplot(df,aes(x=df[,i],y=n))+geom_line}
str(df[,3:14])
ggplot(df[,4])
?ggplot
ggplot(as.integer(iris[,2]))+geom_line()
ggplot(iris,aes(x=iris$Sepal.Length,y=n))+geom_line()
hist(iris$Sepal.Length,freq = F)
ggplot(data=iris,mapping=aes(x=iris$Sepal.Length,y=value))+geom_point()
format(Sys.Date(),"%d")
substr(as.character(Sys.Date()),2,5)
as.character(Sys.Date())
?subset
a<-lm(data=iris[,1:4],iris$Species~.)
predict(object = a,newdata = b)
b<-sapply(iris[,1:4],mean)
b
mean(iris[,1:4])
str(b)
b[1]
summary(a)
getwd()
setwd("C:/Users/Subb/Desktop/")

a<-read.table(file = "amazon0302_adj.tsv",header = T,sep="\t")
str(a)
