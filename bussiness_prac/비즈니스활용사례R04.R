#business example practice r
getwd()
setwd("/Users/DS/Documents/R-/bussiness_prac/")
dau04<-read.csv("../bussiness_prac/R/section4-dau.csv",header = T,stringsAsFactors = F)
head(dau04)
user_info04<-read.csv("R/section4-user_info.csv",header = T,stringsAsFactors = F)
head(user_info04)
library(plyr)(
dau_info04<-merge(dau04,user_info04,by=c("user_id","app_name"))
nrow(dau_info04)-nrow(dau_info04_2)
nrow(dau04)
length(unique(dau04$user_id))
length(unique(user_info04$user_id))
length(unique(dau04$user_id))-length(unique(user_info04$user_id))
dau_info04$log_mon<-substr(dau_info04$log_date,1,7)
#데이터 집계함수
count(df = dau_info04,vars = c("gender","log_mon"))
count(dau_info04,vars=c("log_mon","generation"))
#table로 재조함 집계
table(dau_info04[, c("log_mon", "gender")])
table(dau_info04[,c("log_mon","generation")])
#dcast를 사용하여 재조합
library(reshape2)
dcast(dau_info04,formula =log_mon~gender+generation ,value.var = "user_id")
dcast(dau_info04, log_mon ~ gender + generation, value.var = "user_id",
      length)
#dcast 필수 요소 formula(연산 혹은 표형태)
#value.var(data의 값으로 들어가는 것), 
#mean, length, sum등 연산자 선택
table(dau_info04[,c("log_mon","device_type")])
library(ggplot2)
#분석 시각화를 위한 데이터 프레임 생성
dau_sales_device_type<-ddply(dau_info04,.(log_date,device_type),summarize,dau=length(user_id))
dau_sales_device_type
#ddply(데이터프레임,.(열1,열2),연산자,새로생성 변수)
#year를 없앤 날짜 데이터로 만들기
dau_sales_device_type$log_date<-as.Date(dau_sales_device_type$log_date)
dau_sales_device_type
library(scales)
limits<-c(0,max(dau_sales_device_type$dau))
ggplot(data = dau_sales_device_type,aes(x = log_date,y=dau,col=device_type,lty=device_type,shape=device_type))+
  geom_line(lwd=1)+
  geom_point(size=4)+
  scale_y_continuous(label=comma,limits=limits)
#col=디바이스 타입에 따른 색 차이
#lty=디바이스 타입 따른 라인 타입
#shape=디바이스 타입에 따른 포인트 타이
#데이터 집계함수
dau_info04[, c("log_mon", "gender")]
#2개의 열을 가지고 행과 열을 만듬
#3가지 있을 경우 패널데이터 방ㅅ
head(dau_info04)
