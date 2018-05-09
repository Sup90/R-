#07장 로지스틱 회귀분석
#DAU
DAU<-read.csv("R-/bussiness_prac/R/section7-dau.csv",header = T,stringsAsFactors = F)
head(DAU)
head(unique(DAU$region_month))
mau<-unique(DAU[,c("region_month","device","user_id")])
mau
fp.mau<-unique(DAU[DAU$device=="FP",c("region_month","device","user_id")])
sp.mau<-unique(DAU[DAU$device=="SP",c("region_month","device","user_id")])
fp.mau
sp.mau
fp.mau1<-fp.mau[fp.mau$region_month=="2013-01",]
fp.mau2<-fp.mau[fp.mau$region_month=="2013-02",]
sp.mau1<-sp.mau[sp.mau$region_month=="2013-01",]
sp.mau2<-sp.mau[sp.mau$region_month=="2013-02",]
mau$is_access<-1
#is_access를 mau에 추가
head(mau)
fp.mau1<-merge(fp.mau1,mau[mau$region_month=="2013-02",c("user_id","is_access")],by="user_id",all.x=T)
#1월 피처폰 데이터와 2월의 데이터를 결합하여 1월 피처폰 데이터 중 2월 데이터가 없는 유저는 NA로 결함ㄷ
fp.mau1
fp.mau1$is_access[is.na(fp.mau1$is_access)]<-0
head(fp.mau1)
fp.mau2$is_fp<-1
head(fp.mau2)
fp.mau1<-merge(fp.mau1,fp.mau2[,c("user_id","is_fp")],by="user_id",all.x=T)
fp.mau1$is_fp[is.na(fp.mau1$is_fp)]<-0
#1월 피처폰 사용자 기록에 user아이디를 통해 2월 피처폰 데이터를 결합
#이를 통해 1월 피처폰 데이터 중 2월에 사용이 없는 사람 구분 가능
sp.mau2$is_sp<-1
fp.mau1<-merge(fp.mau1,sp.mau2[,c("user_id","is_sp")],by="user_id",all.x=T)
fp.mau1$is_sp[is.na(fp.mau1$is_sp)]<-0
head(fp.mau1)

fp.mau1<-fp.mau1[fp.mau1$is_access==0|fp.mau1$is_sp==1,]
head(fp.mau1)
library(reshape2)
fp.dau1<-DAU[DAU$device=="FP"&DAU$region_month=="2013-01",]
fp.dau1$is_access<-1
#피처폰을 쓰고 1월 사용 내역이 있는 유저를 뽑고 is_access변수에 1을 넣음
fp.dau1.cast<-dcast(fp.dau1,user_id~region_day,value.var = "is_access",function(x) as.character(length(x)))
#
head(fp.dau1.cast)

names(fp.dau1.cast)[-1]<-paste0("X",1:31,"day")
head(fp.dau1.cast)
names(fp.dau1.cast)[-1]
names(fp.dau1.cast)
#1빼고 다 보여주기
fp.dau1.cast<-merge(fp.dau1.cast,fp.mau1[,c("user_id","is_sp")],by="user_id")
#fp.mau데이터를 통해 dau에 is_sp 컬럼을 결합
head(fp.dau1.cast)
table(fp.dau1.cast$is_sp)
#is_sp값 확인

fit.logit<-step(glm(is_sp~.,data=fp.dau1.cast[,-1],
                    family = binomial))
summary(fit.logit)
#step 변수 선택 입력
#glm,family=binomial-> 로지스틱 입력
fp.dau1.cast$prob<-round(fitted(fit.logit),2)
fitted(fit.logit)
fp.dau1.cast$pred<-ifelse(fp.dau1.cast$prob>0.5,1,0)
head(fp.dau1.cast)
?fitted
#fitted 모델링을 통해 나온 값은 반환하는 함수

table(fp.dau1.cast[,c("is_sp","pred")])

fp.dau1.cast1<-fp.dau1.cast[fp.dau1.cast$is_sp==1&fp.dau1.cast$pred==1,]
head(fp.dau1.cast1[order(fp.dau1.cast1$prob,decreasing = T),])
fp.dau1.cast2<-fp.dau1.cast[fp.dau1.cast$is_sp==0&fp.dau1.cast$pred==1,]
head(fp.dau1.cast2[order(fp.dau1.cast2$prob,decreasing = T),])
fp.dau1.cast3<-fp.dau1.cast[fp.dau1.cast$is_sp==0&fp.dau1.cast$pred==0,]
head(fp.dau1.cast3[order(fp.dau1.cast3$prob),])
fp.dau1.cast3[order(fp.dau1.cast3$prob),]
              