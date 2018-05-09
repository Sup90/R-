#chapter08 클러스터링
#유저 세그멘테이션 분석려
s<-getwd()
s
setwd("R-/bussiness_prac/R/")
act<-read.csv("sample-data/section8/daily/action/game-01/2013-10-31/data.tsv",header = T,stringsAsFactors = F,sep="\t")
head(act)
dau<-read.csv("sample-data/section8/daily/dau/game-01/")
#foreach사용
#plyr 사용
library(plyr)
install.packages("foreach")
library(foreach)
readtsvdates<-function(base.dir,app.name,date.from,date.to){
  date.from<-as.Date(date.from)
  date.to<-as.Date(date.to)
  dates<-seq.Date(date.from,date.to,by="day")
  x<-ldply(foreach(day=dates,combine=rbind) %do%{
    #rbind로 행을 합쳐서 데이터 프레임으로 변환 
    #ldply 리스트를 데이터 프레임으로 변호
    #%do%
    # foreach: Specify the variables to iterate over
    # %do%: Execute the R expression sequentially
    # do는 순차적으로 실행
    # dopar은 병렬적으로 실행
    # %dopar%: Execute the R expression using the currently registered backend
    #seq.Date
    #?seq.Date
    #Generate Regular Sequences of Date
    #시작날짜와 종료날짜를 통해 date List 생성
    read.csv(sprintf("%s/%s/%s/data.tsv",base.dir,app.name,day),
             #sprintf
             #?sprintf
             #String Formatting Commands
             #주어진 인자를 규칙에 맞게 문자열로 변환해 출력
             #a="C:/Users/DS/Documents/R-/bussiness_prac/R"
             #sprintf("%s/",a)
             #"C:/Users/DS/Documents/R-/bussiness_prac/R/"
             header = T,
             sep="\t",
             stringsAsFactors = F)
    })
  x
}
a="C:/Users/DS/Documents/R-/bussiness_prac/R"
sprintf("%s/",a)
#seq.Date
#?seq.Date
#Generate Regular Sequences of Date
#시작날짜와 종료날짜를 통해 date List 생성

readdau<-function(app.name,date.from,date.to=date.from){
  data<-readtsvdates("sample-data/section8/daily/dau/",app.name,date.from,date.to)
  data
}
readdpu<-function(app.name,date.from,date.to=date.from){
  data<-readtsvdates("sample-data/section8/daily/dpu/",app.name,date.from,date.to)
  data
  }
readactiondaily<-function(app.name,date.from,date.to = date.from){
  data <-readtsvdates("sample-data/section8/daily/action/",app.name ,date.from,date.to)
  data
}

dau<-readdau("game-01","2013-05-01","2013-10-31")
head(dau)
dpu<-readdpu("game-01","2013-05-01","2013-10-31")
head(dpu)
user.action<-readactiondaily("game-01","2013-10-31","2013-10-31")
dau2<-merge(dau,dpu[,c("log_date","user_id","payment")],by=c("log_date","user_id"),all.x=T)
dau2$is.payment<-ifelse(is.na(dau2$payment),0,1)
head(dau2)
dau2[is.na(dau2$payment)==F,]
dau2$log_month<-substr(dau2$log_date,1,7)
head(dau2)
mau<-ddply(dau2,.(log_month,user_id),summarize,payment=sum(payment),access_days=length(log_date))
head(mau)
install.packages("ykmeans")
library(ykmeans)
mau$payment
