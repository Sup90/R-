install.packages(c("plyr","ggplot2","scales","reshape","foreach","rpart","partykit","randomForest","caret","ykmeans","caret"))
getwd()
setwd("C:/Users/student/Desktop/source/DataAnalysis_src/R")
dau<-read.csv("section3-dau.csv",header = T,stringsAsFactors = F)
dpu<-read.csv("section3-dpu.csv",header = T,stringsAsFactors = F)
install<-read.csv("section3-install.csv",header = T,stringsAsFactors = F)
str(dau)
str(dpu)
str(install)
dau$log_date<-as.Date(dau$log_date)
dau$user_id<-as.character(dau$user_id)
dau.
head(install)
dau.install<-merge(dau,install,by=c("user_id","app_name"))
dau.install
dau.install.payment<-merge(dau.install,dpu,by=c("log_date","user_id","app_name"),all.x = T)
dau.install.payment[!is.na(dau.install.payment$payment),]
dau.install.payment[,dau.install.payment$payment==NA]
dau.install.payment$payment==NA
iris[iris$Species!="setosa",]
dau.install.payment$payment[is.na(dau.install.payment$payment)]=0
(is.na(dau.install.payment$payment)
cast
dau.install.payment
ggplot(dau.install.payment,)
dau.install.payment$log_month<-substr(dau.install.payment$log_date,1,7)
dau.install.payment$install_month<-substr(dau.install.payment$install_date,1,7)
head(dau.install.payment)
library("plyr")
mau.payment <- ddply(dau.install.payment,
                     .(log_month, user_id, install_month), # 그룹화
                     summarize, # 집계 명령
                     payment = sum(payment) # payment 합계
)
library(reshape2)
na.omit(dau.install.payment)
head(mau.payment)
mau.payment
ggplot(mau.payment[])
mau.payment$user.type<-ifelse(mau.payment$install_month==mau.payment$log_month,"install","existing")
mau.payment.summary<-ddply(mau.payment,.(log_month,user.type),summarize,total.payment=sum(payment))
mau.payment.summary
ggplot(mau.payment.summary[])
hist(x=mau.payment.summary$log_month,y=mau.payment.summary$total.payment,labels = mau.payment.summary$user.type)
?hist
library("ggplot2")
library("scales")
ggplot(mau.payment.summary, aes(x = log_month, y = total.payment,
                                fill = user.type)) + geom_bar(stat="identity") + scale_y_continuous(label = comma)
?ggplot
?hist
ggplot(mau.payment[mau.payment$payment > 0 & mau.payment$user.type == "install", ], 
       aes(x = payment, fill = log_month)) + geom_histogram(position = "dodge")+scale_y_continuous(label = comma)


dau<-read.csv("section4-dau.csv",header = T,stringsAsFactors = F)
info<-read.csv("section4-user_info.csv",header = T,stringsAsFactors = F)
head(dau)
head(info)
str(dau)
str(info)
dau.user.info<-merge(dau,info,by=c("user_id","app_name"));head(dau.user.info1)
dau.user.info2<-merge(info,dau,by=c("user_id","app_name"));head(dau.user.info2)
user.info
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7)
table(dau.user.info[, c("log_month", "gender")])
table(dau.user.info[,c("log_month","generation","gender")])
table(dau.user.info[, c("log_month", "device_type")])
?dcast
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id",
      length,margins = T)
?dcast

nrow(dau.user.info)
dcast(dau.user.info,log_month~device_type,value.var = "user_id",length)
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
dau.user.info.device.summary
?ddply
str(dau.user.info.device.summary)
dau.user.info.device.summary$log_date<-as.character(dau.user.info.device.summary$log_date)
dau.user.info.device.summary$dau<-as.integer(dau.user.info.device.summary$dau)
ggplot(dau.user.info.device.summary,aes(x=log_date,y=dau,col=device_type,ity=device_type,shape=device_type))+
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)
dau.user.info.device.summary$log_date<-as.Date(dau.user.info.device.summary$log_date)
limits <- c(0, max(dau.user.info.device.summary$dau))
?ggplot
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=0.5) +
  geom_point(size=2) +
  scale_y_continuous(label=comma, limits=limits)

str(dau.user.info.device.summary)
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
?ggplot
imp<-read.csv("section5-ab_test_imp.csv",header = T,stringsAsFactors = F)
goal<-read.csv("section5-ab_test_goal.csv",header = T,stringsAsFactors = F)
head(imp)
head(goal)
imp.a<-merge(imp,goal,by="transaction_id",all.x=T,suffixes=c("",".g"))
imp.a$is.goal<-ifelse(is.na(imp.a$user_id.g),0,1)
head(imp.a)
ab.test.imp <- merge( goal,imp, by="transaction_id", all.x=T, suffixes=c("",".g"))
head(ab.test.imp)
ab.test.imp1 <- merge( imp,goal, by="transaction_id", all.x=T, suffixes=c("",".g"))
head(ab.test.imp1)
imp.a<-ddply(imp.a,.(test_case),summarise,cvr=sum(is.goal)/length(user_id))
length(iris$Species)
nrow(iris)
chisq.test(imp.a$test_case, imp.a$is.goal)
str(imp.a)
imp.a$log_date<-as.Date(imp.a$log_date)
ggplot(imp.a,aes=(x=log_date,y=cvr))
?ggplot
ab.test.imp.summary <-
  ddply(imp.a, .(log_date, test_case), summarize,
        imp=length(user_id),
        cv=sum(is.goal),
        cvr=sum(is.goal)/length(user_id))
head(ab.test.imp.summary)
ab.test.imp.summary
length(imp.a$user_id[imp.a$log_date=="2013-10-01"&imp.a$test_case=="A"])
ab.test.imp.summary<-ddply(ab.test.imp.summary, .(test_case),transform,
                           cvr.avg=sum(cv)/sum(imp))
head(ab.test.imp.summary)
ggplot(ab.test.imp.summary,aes(x=log_date,y=cvr,col=test_case,lty=test_case,shape=test_case))+
  geom_line(lwd=0.5)+
       geom_point(size=4)+
       
       geom_line(aes(y=cvr.avg,col=test_case)) +
         scale_y_continuous(label=percent, limits=limits)
str(ab.test.imp.summary)
       ?geom_point
str(ab.test.imp.summary)
ggplot(ab.test.imp.summary,aes(x=log_date,y=cvr, col=test_case,lty=test_case, shape=test_case)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  geom_line(aes(y=cvr.avg,col=test_case)) +
  scale_y_continuous(label=percent, limits=limits)
?plot()
limits <- c(0, max(ab.test.imp.summary$cvr))
dau<-read.csv("section7-dau.csv",stringsAsFactors = F,header = T)
head(dau)
mau<-unique(dau[,c("region_month","device","user_id")])
fp.mau1<-unique(dau[dau$device=="FP"&dau$region_month=="2013-01",c("region_month","device","user_id")])
fp.mau2<-unique(dau[dau$device=="FP"&dau$region_month=="2013-02",c("region_month","device","user_id")])
sp.mau1<-unique(dau[dau$device=="SP"&dau$region_month=="2013-01",c('region_month','device','user_id')])
sp.mau2<-unique(dau[dau$device=="SP"&dau$region_month=="2013-02",c('region_month','device','user_id')])
mau$is_access<-1
mau
fp.mau1<-merge(fp.mau1,mau[mau$region_month=="2013-02",c("user_id","is_access")],by="user_id",all.x=T)
mau[mau$region_month=="2013-02",]
fp.mau1$is_access[is.na(fp.mau1$is_access)]<-0
head(fp.mau1)
unique(mau$is_access)
mau[mau$is_access!=1]
fp.mau1
head(fp.mau1)
fp.mau2$is_fp<-1
fp.mau1<-merge(fp.mau1,fp.mau2[,c("user_id","is_fp")],by="user_id",all.x=T)
fp.mau1
fp.mau1$is_fp[is.na(fp.mau1$is_fp)]<-0
fp.mau1<-fp.mau1[fp.mau1$is_access==0|fp.mau$is_sp==1,]
head(fp.mau)
library("reshape2")
fp.dau1<-dau[dau$device=="FP"&dau$region_month=="2013-01",]
fp.dau1$is_access<-1
fp.dau1.cast<-dcast(fp.dau1,user_id~region_day,value.var = "is_access",function(x) as.character(length(x)))
names(fp.dau1.cast)[-1]<-paste0("X",1:31,"day")
fp.dau1.cast
