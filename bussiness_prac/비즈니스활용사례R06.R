#비스니스활용사례로 배우는 데이터분석06장 
#최적화 문제
#문제인식
#광고와 신규 유저수의 인과관계 분석
#CPI-> Cost Per Install
#광고상품에 해당하는 애플리케이션이 설치된 기기 수에 
#따라 광고 비용을 지불하는 방식. 
#다운로드로 인한 사용자 유입은 물론, 
#인기순위 상승까지 노려볼 수 있기 때문에 
#모바일 게임에서 주로 사용된다.
#TV와 잡지의 광고비와 획득 유저수 관계 파악
#이를 통해 TV와 잡지에 광고 비율 결정
#중회귀 분석을 통해 광고비용과 신규 유저간의 관계를 파악
getwd()
setwd("R-/bussiness_prac/R/")
ad_result<-read.csv("ad_result.csv",header = T,stringsAsFactors = F)
head(ad_result)
library(ggplot2)
library(scales)
ggplot(ad_result,aes(tvcm,install))+geom_point()+
  xlab("TV 광고비")+
  ylab("신규 유저수")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels=comma)

ggplot(ad_result,aes(magazine,install))+geom_point()+
  xlab("잡지 광고비")+
  ylab("신규 유저수")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels=comma)
#중회귀 분석 실행
lm1<-lm(install~.,data=ad_result[,c("install","tvcm","magazine")])
lm1        
#절펴 188.71, tv광고 회귀계수 0.136, 잡지광고비 회귀계수 0.725
summary(lm1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1406.87  -984.49   -12.11   432.82  1985.84 
#1분위수의 절댓값이 3분위수의 절댓값보다 커 약간의 치우침은 있지만
#수정된 결정계수가 0.92로 높기 때문에 의사결정에 문제는 없는 것으로 보임
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  188.1743  7719.1308   0.024  0.98123   
# tvcm           1.3609     0.5174   2.630  0.03390 * 
# magazine       7.2498     1.6926   4.283  0.00364 **
# 잡지의 광고 효과가 더 높은 것으로 보인다.