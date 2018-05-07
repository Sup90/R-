#5장 A/B테스트
getwd()
setwd("c:/Users/rhkdt/Desktop/R-/bussiness_prac/R")
ab_imp<-read.csv("section5-ab_test_imp.csv",header = T,sep = ",",stringsAsFactors = F)
head(ab_imp)
ab_goal<-read.csv("section5-ab_test_goal.csv",header = T,stringsAsFactors = F)
head(ab_goal)
ab_imp_goal<-merge(x = ab_imp,y = ab_goal,by = "transaction_id",suffixes = c("",".g"),all.x = T)
#imp가 전체 건수 goal이 성공 케이스, suffix는 접미어로 두개의 값을 넣으면 앞에는 x의 컬럼 뒤는 y의 컬럼
head(ab_imp_goal)
ab_imp_goal$flag<-ifelse(is.na(ab_imp_goal$user_id.g),0,1)
#user_id.g가 na이면 0, 아니면 1
head(ab_imp_goal)
install.packages("plyr")
library(plyr)
head(ab_imp_goal[is.na(ab_imp_goal$user_id.g)==F,])
ddply(ab_imp_goal,.variables = .(test_case),summarize,cvr=sum(flag)/length(user_id))
#ddply로 데이터 계산 테이블만들기
#변수는 테스트 케이스
#계산 방법은 서머리
#계산 값은 테스트 케이스에 따른 유저 전체
chisq.test(ab_imp_goal$test_case,ab_imp_goal$flag)
#카이검정 실행
#p-value < 2.2e-16
#p-value가 0.05보다 작으면 일반적으로 통계적 유의미한 차이가 있다고 간주
#결과적으로 a,b를 나눈 테스트의 의미가 있었다.
ab_imp_goal_summary<-
  ddply(ab_imp_goal,.(log_date,test_case),summarize,
        imp=length(user_id),
        cv=sum(flag),
        cvr=sum(flag)/length(user_id),
        cvr.avg=sum(cv)/sum(imp))
#ddply를 통해 데이터셋 새로 조정
#log_date와 test_case를 변수로 사용하여
#imp,cv,crv를 새로 만들음
#여기는 행별로 새로 만들음
head(ab_imp_goal_summary)
ab_imp_goal_summary<-ddply(ab_imp_goal_summary,.(test_case),transform,cvr.avg=sum(cv)/sum(imp))
#transform을 통해 원래 데이터에 새로운 집계결과 추가 가능
#여기는 전체 데이터 를 통해 새로 만들음
install.packages("ggplot2")
library(ggplot2)
library(scales)
ab_imp_goal_summary$log_date<-as.Date(ab_imp_goal_summary$log_date)
#날짜 데이터로 변환
limits<-c(0,max(ab_imp_goal_summary$cvr))
#축을 위해 한계선 설정
ggplot(ab_imp_goal_summary,aes(log_date,cvr,col=test_case,lty=test_case,shape=test_case))+geom_line(lwd=1)+
  geom_line(aes(y=cvr.avg,col=test_case))+
  geom_point(size=4)+
    scale_y_continuous(label=percent,limits = limits)
#lty=test_case를 통해 점선화
#shape를 통해 포인트 모양 다르게
#geom_line(lwd=2)굵기 조절
#geom_point(size=10)포인트 사이즈 조절
#geom_line(aes(y=cvr.avg,col=test_case)) 가로축 선 삽입
#scale_y_continuous(label=percent,limits = limits) y축 정보 삽ㅇ