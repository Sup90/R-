setwd("C:\\Users\\DS\\Desktop\\source\\DataAnalysis_src")
getwd
getwd()
ls
ls()
dau03<-read.csv("R/section3-dau.csv",header = T,sep = ",")
head(dau03)
dpu03<-read.csv("R/section3-dpu.csv",header = T,sep =",")
head(dpu03)
install03<-read.csv("R/section3-install.csv",header=T,sep = T)
install03<-read.csv("R/section3-install.csv",header=T,sep = ",")
head(dpu03)
head(install03)
install.packages("dplyr")
library(dplyr)
group_by(dpu03)
group_by(dpu03,user_id)
group_by(dpu03,user_id)
group_by(dpu03,user_id,log_date)
head(dau)
head(dau03)
head(install)
head(install03)
dau_install03<-merge(dau03,install03,by="user_id")
dau_install03_2<-merge(dau03,install03,by=c("app_name","user_id")
dau_install03_2<-merge(dau03,install03,by=c("app_name","user_id"))
dau_install03_2<-merge(dau03,install03,by=c("app_name","user_id"))
head(dau_install03)
head(dau_install03_2)
head(dpu)
head(dpu03)
dau_install_dpu03<-merge(dau_install_dpu03,dpu03,by=c("app_name","
user_id","log_date"))
dau_install_dpu03<-merge(dau_install03,dpu03,by=c("app_name","
user_id","log_date"))
dau_install_dpu03<-merge(dau_install03,dpu03,by=c("app_name","user_id","log_date"))
dau_install_dpu03<-merge(dau_install03,dpu03,by=c("app_name","user_id","log_date"))
dau_install_dpu03<-merge(dau_install03,dpu03,by=c("app_name","user_id","log_date"),all.x=T)
head(dpu03)
dau_install_dpu03<-merge(dau_install03_02,dpu03,by=c("app_name","user_id","log_date"),all.x=T)
dau_install_dpu03<-merge(dau_install03_2,dpu03,by=c("app_name","user_id","log_date"),all.x=T)
dau_install_dpu03_02<-merge(dau_install03_2,dpu03,by=c("app_name","user_id","log_date"))
nrow(dau_install_dpu03)
nrow(dau_install_dpu03_02)
head(dau_install_dpu03)
head(dau_install_dpu03_02)
dau_install_dpu03[,is.na(dau_install_dpu03$payment)=T]$payment=0
dau_install_dpu03[,is.na(dau_install_dpu03$payment)=T]$payment<-0
dau_install_dpu03[,is.na(dau_install_dpu03$payment)]$payment=0
dau_install_dpu03[,is.na(dau_install_dpu03$payment)]$payment=0
dau_install_dpu03[,is.na(dau_install_dpu03$payment)]
dau_install_dpu03[,is.na(dau_install_dpu03$payment)==T]
dau_install_dpu03[,is.na(payment)==T]
head(dau_install_dpu03[,payment>0])
head(dau_install_dpu03[,dau_install_dpu03$payment>0])
head(dau_install_dpu03)
head(dau_install_dpu03$payment[,dau_install_dpu03$payment>0])
head(dau_install_dpu03$payment[,dau_install_dpu03$payment==NA])
head(dau_install_dpu03$payment[dau_install_dpu03$payment==NA])
head(dau_install_dpu03$payment[dau_install_dpu03$payment>0])
head(dau_install_dpu03$payment[dau_install_dpu03$payment>=0])
dau_install_dpu03$payment[dau_install_dpu03$payment0==NA]<-0
head(dau_install_dpu03)
dau_install_dpu03$payment[is.na(dau_install_dpu03$payment)==T]<-0
head(dau_install_dpu03)
dau_install_dpu03$log_mon<-substr(dau_install_dpu03$log_date,1,7)
head(dau_install_dpu03)
group_by(dau_install_dpu03,user_id,log_mon)
dau_install_dpu03$inst_mon<-substr(dau_install_dpu03$install_date,1,7)
mau.payment<-ddply(dau_install_dpu03,.(log_mon,user_id,inst_mon),
summarize,
payment=sum(payment))
install.packages("plyr")
library(plyr)
mau.payment<-ddply(dau_install_dpu03,.(log_mon,user_id,inst_mon),
summarize,
payment=sum(payment))
head(mau.payment)
mau[user_id=="1"]
mau.payment[user_id=="1"]
mau.payment[user_id==1]
mau.payment[mau.payment$user_id==1]
mau.payment[,mau.payment$user_id==1]
mau.payment[,mau.payment$user_id==1]
mau.payment[mau.payment$user_id==1]
mau.payment
mau.payment$user_id==1
mau.payment[mau.payment$user_id==1,]
mau.payment[mau.payment$user_id==1]
mau.payment[mau.payment$user_id==1,]
mau.payment$user_type<-ifelse(mau.payment$inst_mon==log_mon,"new","old")
mau.payment$user_type<-ifelse(mau.payment$inst_mon==mau.payment$log_mon,"new","old")
head(mau.payment)
mau.payment.summary<-ddply(mau.payment,.(log_mon,user_type),summarize,total.payment=sum(payment))
head(mau.payment.summary)
library("ggplot2")
install.packages("ggplot2")
install.packages("scales")
library(scales)
library("ggplot2")
head(mau.payment.summary)
ggplot(mau.payment.summary,aes(x=log_mon,y=total.payment,fill=user_type))+geom_bar(stat="identity")+scale_y_continuous(labels = comma)
old_theme = theme_update(
axis.title.x = theme_text(family="HiraKakuProN-W3"),
axis.title.y = theme_text(family="HiraKakuProN-W3", angle=90),
plot.title = theme_text(family="HiraKakuProN-W3", size=14.4))
head(mau.payment)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type="new"],aes(x=payment,fill=log_mon))+geom_histogram(position = "
dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type="new"],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=200000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=2000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(binwidth=20000)
position = "dodge"
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)+scale_x_continuous(labels = human_gbp)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)+scale_x_continuous(labels = payment)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)+scale_x_continuous(labels = mau.paymet$payment)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)+scale_x_continuous(labels = mau.payment$payment)
ggplot(mau.payment[mau.payment$payment>0 & mau.payment$user_type=="new",],aes(x=payment,fill=log_mon))+geom_histogram(position = "dodge",binwidth=20000)+scale_x_continuous(labels =comma)
mau.payment[mau.payment$user_type=="new",]
nrow(mau.payment[mau.payment$user_type=="new",])
unique(mau.payment[mau.payment$user_type=="new",]$user_id)
length(unique(mau.payment[mau.payment$user_type=="new",]$user_id))
install.packages("knitr")
library(knitr)
knit("RGitHub.Rmd")
knit("RGitHub.Rmd")
