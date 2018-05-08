#07장 로지스틱 회귀분석
#DAU
DAU<-read.csv("c:/Users/rhkdt/Desktop/R-/bussiness_prac/R/section7-dau.csv",header = T,stringsAsFactors = F)
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
fp.mau1<-merge(fp.mau1,mau[mau$region_month=="2013-02",c("user_id","is_access")],by="user_id",all.x=T)가
#1월 피처폰 데이터와 2월의 데이터를 결합하여 1월 피처폰 데이터 중 2월 데이터가 없는 유저는 NA로 결함ㄷ
fp.mau1
fp.mau1$is_access[is.na(fp.mau1$is_access)]<-0
