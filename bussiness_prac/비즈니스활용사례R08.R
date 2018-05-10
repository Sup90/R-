#chapter08 클러스터링
#유저 세그멘테이션 분석려
s<-getwd()
s

setwd("c:/Users/rhkdt/Desktop/R-/bussiness_prac/R/")
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
library("ggplot2")
install.packages("ggplot2")
library(scales)
head(user.action)
user.action2<-ykmeans(user.action,"A47","A47",3)
#?ykmeans
#A47이 랭킹이다. 
a<-kmeans(user.action[,-c(1:3)],centers = 3)
head(a)
a$cluster
user.action$cluster<-a$cluster
train.user.action<-scale(user.action)
user.action[complete.cases(user.action)==T,]
user.action[]
head(user.action2)
table(user.action2$cluster)
table(user.action$cluster)
user.action[user.action$cluster==user.action2$cluster,]

ggplot(arrange(user.action2,desc(A47)),
       aes(x=1:length(user_id),y=A47,
           #x 축은 그냥 숫자 명수
           #y 축은 랭킹 포인트 크기순으로 정렬된 값
           col=as.factor(cluster),
           shape=as.factor(cluster)))+
  geom_line()+
  xlab("user")+
  ylab("Ranking point")+
  scale_y_continuous(labels = comma)+
  ggtitle("Ranking Point")+
  theme(legend.position = "none")
  #범례 생략

?ggplot

#상위 랭킹 유저 픽
user.action.h<-user.action2[user.action2$cluster>=2,names(user.action)]

user.action.h2<-user.action2[user.action2$cluster>=2,]
length(names(user.action2))
user.action.h==user.action.h2
length(names(user.action))
#전처리를 위해 기계학습 라이브러리 사용
install.packages("caret")
library(caret)

user.action.f<-user.action.h[,-c(1:4)]
user.action.h[,-c(1:4)]
row.names(user.action.f)<-user.action.h$user_id
head(user.action.f)
#정보량이 0에 가까운 변수 제거
nzv<-nearZeroVar(user.action.f)
#nearZeroVar
#분산이 낮은 변수를 제거
?nearZeroVar
nzv
user.action.f.filtered<-user.action.f[,-nzv]
#변수 간에 상관이 높은 것을 제거
user.action.cor<-cor(user.action.f.filtered)
user.action.cor
highly.cor.f<-findCorrelation(user.action.cor,cutoff = .7)
#findCorrelation
#제거해야하는 높은 상관 관계 변수 리스트 반환
user.action.f.filtered<-user.action.f.filtered[,-highly.cor.f]
#주성분 분석 실행
#pca
user.action.pca.base<-prcomp(user.action.f.filtered,scale=T)
#scale로 표준화함
?prcomp
user.action.pca.base$rotation
user.action.pca.base$x
str(user.action.pca.base)
user.action.pca<-data.frame(user.action.pca.base$x)
#유저별 주성분 변수 데이터 프레임 삽입
keys<-names(user.action.pca)
keys
user.action.km<-ykmeans(user.action.pca,keys,"PC1",3:6)
user.action.pca
user.action.kmㅅ
table(user.action.km$cluster)
ggplot(user.action.km,
       aes(x=PC1,y=PC2,col=as.factor(cluster),shape=as.factor(cluster)))+
  geom_point()
user.action.f.filtered$cluster<-user.action.km$cluster
user.action.f.filtered
user.action.f.center<-ldply(lapply(sort(unique(user.action.f.filtered$cluster)),
  function(i){
    x<-user.action.f.filtered[user.action.f.filtered$cluster==i,
                              -ncol(user.action.f.filtered)]
    apply(x,2,function(d) mean(d))
      }))
install.packages("fmsb")
user.action.f.center
library("fmsb")
radarchartframe<-function(df){
  df<- data.frame(df)
  dfmax<-apply(df,2,max)+1
  dfmin<-apply(df,2,min)-1
  as.data.frame(rbind(dfmax,dfmin,df))
}
#상관관계 높은 변수 제외
user.action.f.center
df<-user.action.f.center[,-(ncol(user.action.f.center)-1)]
(ncol(user.action.f.center)-1)
ncol(user.action.f.center)
df.cor<-cor(df)
df.cor
df.highly.cor<-findCorrelation(df.cor,cutoff = 0.91)
df.filtered<-df[,-df.highly.cor]
df.filtered<-radarchartframe(scale(df.filtered))
names(df.filtered)
user.action.f.filtered$user_id<-as.numeric(row.names(user.action.f.filtered))
user.action.f.filtered
user.kpi<-merge(user.action.f.filtered,mau,by=
                  "user_id")
ddply(user.kpi,.(cluster),summarize,
      arpu=round(mean(payment)),
      access_days=round(mean(access_days)))
user.kpi
#다시 봐야할 파트... 어렵다...