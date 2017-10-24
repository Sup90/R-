library(plyr)
library(foreach)
# 이용시작일 데이터를 읽어들이는 함수
readInstall <- function(app.name, target.day) {
  base.dir <- "sample-data/section9/snapshot/install"
  f <- sprintf("%s/%s/%s/install.csv", base.dir, app.name, target.day)
  read.csv(f, header = T, stringsAsFactors = F)
}

# DAU 데이터를 읽어들이는 함수
readDau <- function(app.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  ldply(foreach(day = dates, combine = rbind) %do% {
    base.dir <- "sample-data/section9/daily/dau"
    f <- sprintf("%s/%s/%s/dau.csv", base.dir, app.name, day)
    read.csv(f, header = T, stringsAsFactors = F)
  })
}

# 행동로그를 읽어들이는 함수
readAction <- function(app.name, action.name, date.from, date.to) {
  date.from <- as.Date(date.from)
  date.to <- as.Date(date.to)
  dates <- seq.Date(date.from, date.to, by = "day")
  ldply(foreach(day = dates, combine = rbind) %do% {
    base.dir <- "sample-data/section9/daily/action"
    f <- sprintf("%s/%s/%s/%s/%s.csv",
                 base.dir, app.name, action.name, day, action.name)
    read.csv(f, header = T, stringsAsFactors = F)
  })
}

setwd("C:\\Users\\student\\Desktop\\source\\DataAnalysis_src\\R")
# install
install <- readInstall("game-01", "2013-09-30")
head(install)
str(install)
# DAU
dau <- readDau("game-01", "2013-06-01", "2013-09-30")
head(dau)
str(dau)



# battle
battle <- readAction("game-01", "battle", "2013-06-01", "2013-08-31")
head(battle)
# message
msg <- readAction("game-01", "message", "2013-06-01", "2013-08-31")
head(msg)
# help
hlp <- readAction("game-01", "help", "2013-06-01", "2013-08-31")
head(hlp)

# 접속밀도 산출
# DAU에 이용시작일 정보를 결합하기
dau.inst <- merge(dau, install, by = "user_id", suffixes
                  = c("", ".inst"))
head(dau.inst)

# 이용시작후 7~13일의 데이터만 골라내기
dau.inst$log_date <- as.Date(dau.inst$log_date)
dau.inst$log_date.inst <- as.Date(dau.inst$log_date.inst)
dau.inst$elapsed_days <- as.numeric(dau.inst$log_date -
                                      dau.inst$log_date.inst)
dau.inst.7_13 <-
  dau.inst[dau.inst$elapsed_days >= 7 &
             dau.inst$elapsed_days <= 13, ]
head(dau.inst.7_13)
unique(dau.inst.7_13$elapsed_days)
# 접속밀도 산출
dau.inst.7_13.login.ds <- ddply(dau.inst.7_13, .(user_id), summarize,
                                density = length(log_date)/7)
head(dau.inst.7_13.login.ds)
length(dau.inst.7_13[dau.inst.7_13$user_id=="654",]$log_date)

# 분석대상 유저 데이터에 접속밀도를 결합하기
target.install <- install [install$log_date >= "2013-06-01" &
                             install$log_date <= "2013-08-25", ]

target.install
# 분석대상 유저 데이터에 접속밀도를 결합하기
target.install.login.ds <-
  merge(target.install, dau.inst.7_13.login.ds,
        by = "user_id", all.x = T)
target.install.login.ds$density <-
  ifelse(is.na(target.install.login.ds$density), 0,
         target.install.login.ds$density)
head(target.install.login.ds)

# '싸움' 액션에 관한 데이터 작성하기
# 싸움과 Install(이용시작일) 데이터를 결합
battle.inst <- merge(battle, install, by = "user_id", suffixes = c("",
                                                                   ".inst"))
head(battle.inst)

# 싸움 행동일과 이용시작일의 차이(경과일수)를 계산
battle.inst$log_date <- as.Date(battle.inst$log_date)
battle.inst$log_date.inst <- as.Date(battle.inst$log_date.inst)
battle.inst$elapsed_days <- as.numeric(battle.inst$log_date -
                                         battle.inst$log_date.inst)


# 경과일수가 1주이내인 것만 골라냄
battle.inst2<-battle.inst[battle.inst$elapsed_days>=0&battle.inst$elapsed_days<=6,]
# 경과 일수별 싸움 행동 횟수가 열에 오도록 데이터를 배치
library(reshape2)
battle.inst2$elapsed_days
battle.inst2.cast<-dcast(battle.inst2,user_id~elapsed_days,value.var = "count",sum)
head(battle.inst2.cast)
 #비율 데이터, 주성분 데이터 작성
 #비율 데이터 작성
battle.inst2.cast.prop<-battle.inst2.cast
battle.inst2.cast.prop[,-1]<-battle.inst2.cast.prop[,-1]/rowSums(battle.inst2.cast.prop[,-1])
head(battle.inst2.cast.prop)
#battle.inst2.cast.prop[,-1]는 첫번째 열 빼는거
# PCA
b.pca<-prcomp(battle.inst2.cast[,-1],scale = T)
b.pca$center
b.pca$sdev
b.pca$scale
b.pca$x
summary(b.pca)
battle.inst2.cast.pca<-data.frame(user_id=battle.inst2.cast$user_id,b.pca$x)
head(battle.inst2.cast.pca)
?data.frame
# '메시지' 액션에 관한 데이터 작성하기
# 메시지와 Install(이용시작일) 데이터를 결합하기
msg.inst<-merge(msg,install,by="user_id",suffixes=c("",".inst"))
head(msg.inst)
# 메시지 행동일과 이용시작일의 차이(경과일수)를 계산
msg.inst$log_date<-as.Date(msg.inst$log_date)
msg.inst$log_date.inst<-as.Date(msg.inst$log_date.inst)
msg.inst$elapsed_days<-as.numeric(msg.inst$log_date-msg.inst$log_date.inst)
# 경과일수가 1주일이내인 것만 골라내기
msg.inst2<-msg.inst[msg.inst$elapsed_days>=0&msg.inst$elapsed_days<=6,]

# 경과일수별 메시지 행동횟수가 열에 오도록 데이터를 배치하기
msg.inst2$elapsed_days<-paste0("d",msg.inst2$elapsed_days)
msg.inst2.cast<-dcast(msg.inst2,user_id~elapsed_days,value.var="count",sum)
head(msg.inst2.cast)
# 비율 데이터, 주성분(PCA) 데이터 작성
# 비율 데이터 작성
msg.inst2.cast.prop<-msg.inst2.cast
msg.inst2.cast.prop[,-1]<-msg.inst2.cast.prop[,-1]/rowSums(msg.inst2.cast.prop[,-1])
head(msg.inst2.cast.prop)
#pca
m.pca<-prcomp(msg.inst2.cast[,-1],scale=T)
summary(m.pca)
msg.inst2.cast.pca<-data.frame(user_id=msg.inst2.cast$user_id,m.pca$x)
head(msg.inst2.cast.pca)
# '협력' 액션에 관한 데이터 작성하기
# 협력과 Install(이용시작일) 데이터를 결합하기
hlp.inst<-merge(hlp,install,by="user_id",suffixes = c("",".inst"))
# 협력 행동일과 이용시작일의 차이(경과일수)를 계산
hlp.inst$log_date <- as.Date(hlp.inst$log_date)
head(hlp.inst)
hlp.inst$log_date.inst<-as.Date(hlp.inst$log_date.inst)
hlp.inst$elapsed_days<-as.numeric(hlp.inst$log_date-hlp.inst$log_date.inst)

# 경과일수가 1주일 이내인 것만 골라내기
hlp.inst2<-hlp.inst[hlp.inst$elapsed_days>=0&hlp.inst$elapsed_days<=6,]
hlp.inst2
# 경과일수별 협력 행동횟수가 열에 오도록 데이터를 배치하기
hlp.inst2$elapsed_days<-paste0("d",hlp.inst2$elapsed_days)
hlp.inst2
hlp.inst2.cast<-dcast(hlp.inst2,user_id~elapsed_days,value.var = "count",sum)
head(hlp.inst2.cast)
hlp.inst2.cast
# 비율 데이터, 주성분 (PCA) 데이터 작성
# 비율 데이터 작성
hlp.inst2.cast.prop<-hlp.inst2.cast
hlp.inst2.cast.prop[,-1]<-hlp.inst2.cast.prop[,-1]/rowSums(hlp.inst2.cast.prop[,-1])
hlp.inst2.cast.prop
#pca
h.pca<-prcomp(hlp.inst2.cast[,-1],scale=T)
summary(h.pca)
hlp.inst2.cast.pca<-data.frame(user_id=hlp.inst2.cast$user_id,h.pca$x)
hlp.inst2.cast.pca
# 행동로그에 대한 클러스터링
# 클러스터 데이터 작성함수
createClusterData<-function(aname,x,x.prop,x.pca){
  set.seed(10)
  df<-ldply(foreach(i=3:6,combine=rbind) %do% {
    km<-kmeans(x[,-1],i)
    km.prop<-kmeans(x.prop[,-1],i)
    km.pca<-kmeans(x.pca[,-1],i)
    data.frame(user_id=x$user_id,cluster.type=sprintf("%s%02d",aname,i),
               freq.cluster.id=km$cluster,prop.cluster.id=km.prop$cluster,
               pca.cluster.id=km.pca$cluster)
  })
  cluster.melt<-melt(df,id.vars=c("user_id","cluster.type"))
  dcast(cluster.melt,user_id~cluster.type+variable)
}
#싸움
battle.cluster<-createClusterData("battle",battle.inst2.cast,battle.inst2.cast.prop
                                  ,battle.inst2.cast.pca)
head(battle.cluster)
#메시지
msg.cluster<-createClusterData("msg",msg.inst2.cast,
                               msg.inst2.cast.prop,
                               msg.inst2.cast.pca)
#협력
hlp.cluster<-createClusterData("hlp",hlp.inst2.cast,hlp.inst2.cast.prop,
                               hlp.inst2.cast.pca)
head(hlp.cluster)

#클러스터의 결합
#cluster data
cluster.data<-merge(target.install.login.ds,battle.cluster,
                    by="user_id",all.x=T)
cluster.data<-merge(cluster.data,msg.cluster,by="user_id",all.x=T)
cluster.data<-merge(cluster.data,hlp.cluster,by="user_id",all.x=T)
cluster.data[is.na(cluster.data)]<-0

# 접속밀도가 오름차순이 되도록 정렬
# 열에 따로따로 위치한 각 클러스터를 하나로 모으기
cluster.data.melt<-melt(cluster.data[,-c(2:6)],id.vars = c("user_id","density"))
head(cluster.data.melt)
str(cluster.data.melt)
length(unique(cluster.data.melt$user_id))

# 클러스터 종별/클러스터 번호별로 평균 접속밀도를 계산
cluster.data.avg<-ddply(cluster.data.melt,.(variable,value),summarize,average.density=mean(density))
head(cluster.data.avg)
str(cluster.data.avg)
# 새로운 클러스터 번호를 부여
cluster.data.avg<-arrange(cluster.data.avg,variable,average.density)
cluster.data.avg<-ddply(cluster.data.avg,.(variable),transform,value2=sort(value))
head(cluster.data.avg)
# 새로운 클러스터 번호를 결합하기
cluster.data.melt2<-merge(cluster.data.melt,cluster.data.avg,by=c("variable","value"))
head(cluster.data.melt2)
# 클러스터 종별이 열에 위치하도록 데이터 배치
cluster.data2<-dcast(cluster.data.melt2,user_id+density~variable,value.var = "value2")
head(cluster.data2)
# 클러스터 종별이 열에 위치하도록 데이터 배치
cluster.data2<-dcast(cluster.data.melt2,user_id+density~variable,value.var="value2")

# 결정트리분석 실행, 시각화
library(rpart)
fit<-rpart(density~.,cluster.data2[,-1])
print(fit)
library(partykit)
plot(as.party(fit),tp_args = T)
plot(as.party(fit),ip_args = T)
plot(as.party(fit),ep_args =  T)
plot(as.party(fit),drop_terminal =T)
plot(as.party(fit))
plot(fit)

# '협력' 주성분 클러스터 상세내용
cluster.data3<-cluster.data.melt2[cluster.data.melt2$variable=="hlp06_pca.cluster.id",
                                  c("user_id","average.density","value2")]
names(cluster.data3)[3]
names(cluster.data3)[3]<-"cluster"
hlp.inst2.cast.prop2<-merge(hlp.inst2.cast.prop,cluster.data3,by="user_id")
table(hlp.inst2.cast.prop2$cluster)



# 클러스터별 평균 접속밀도 산출
hlp.inst2.cast.prop2
hlp.inst2.cast.prop2$average.density[1]
hlp.inst2.cast.summary<-ddply(hlp.inst2.cast.prop2,.(cluster),summarize,
                              login.density=average.density[1],d0=sum(d0)/length(user_id),
                              d1=sum(d1)/length(user_id),
                              d2=sum(d2)/length(user_id),
                              d3=sum(d3)/length(user_id),
                              d4=sum(d4)/length(user_id),
                              d5=sum(d5)/length(user_id),
                              d6=sum(d6)/length(user_id))
hlp.inst2.cast.summary                      
#클러스터별 평균 접속밀도 가시화
library(ggplot2)
ggplot(hlp.inst2.cast.summary,aes(x=cluster,y=login.density))+
  geom_line()+
  geom_point()

# 클러스터별 날짜별 협력 행동 가시화
hlp.inst2.cast.summary[,-2]
hlp.inst2.cast.summary.melt<-melt(hlp.inst2.cast.summary[,-2],id.vars = "cluster")
hlp.inst2.cast.summary.melt$days<-as.numeric(substr(hlp.inst2.cast.summary.melt$variable,2,3))
hlp.inst2.cast.summary.melt$cluster<-as.factor(hlp.inst2.cast.summary.melt$cluster)
ggplot(hlp.inst2.cast.summary.melt,aes(x=days,y=value,col=cluster))+geom_line()+geom_point()
