#chapter09
#결정트리분석
#게임접속 0~6일동안 접속일 비율이 설명변수
#그다음 7~13일의 접속 비율이 목적변수
library(plyr)
library(foreach)
getwd()
setwd("c://Users//DS//Documents//R-//bussiness_prac//R")
readinstall<-function(app.name,target.day){
  base.dir<-"sample-data/section9/snapshot/install"
    f<-sprintf("%s/%s/%s/install.csv",base.dir,app.name,target.day)
  read.csv(f,header = T,stringsAsFactors = F)
}

readdau<-function(app.name,date.from,date.to){
  date.from<-as.Date(date.from)
  date.to<-as.Date(date.to)
  dates<-seq.Date(date.from,date.to,by="day")
  ldply(foreach(day=dates,combine=rbind) %do% {
    base.dir<-"sample-data/section9/daily/dau"
    f<-sprintf("%s/%s/%s/dau.csv",base.dir,app.name,day)
    read.csv(f,header = T,stringsAsFactors = F)
  }
          )
}
readaction<-function(app.name,action.name,date.from,date.to){
  date.from<-as.Date(date.from)
  date.to<-as.Date(date.to)
  dates<-seq.Date(date.from,date.to,by="day")
  ldply(foreach(day=dates,combine=rbind) %do% {
    base.dir<-"sample-data/section9/daily/action"
    f<-sprintf("%s/%s/%s/%s/%s.csv",
               base.dir,app.name,action.name,day,action.name)
    read.csv(f,header = T,stringsAsFactors = F)
  })
}
install<-readinstall("game-01","2013-09-30")
head(install)
head(dau)
dau<-readdau("game-01","2013-06-01","2013-09-30")
battle<-readaction("game-01","battle","2013-06-01","2013-08-31")
head(battle)
msg<-readaction("game-01","message","2013-06-01","2013-08-31")
hlp<-readaction("game-01","help","2013-06-01","2013-08-31")
head(battle)
head(msg)
head(hlp)
dau.inst<-merge(dau,install,by="user_id",suffixes = c("",".inst"))
head(dau.inst)
dau.inst$log_date<-as.Date(dau.inst$log_date)
dau.inst$log_date.inst<-as.Date(dau.inst$log_date.inst)
dau.inst$elapsed_days<-as.numeric(dau.inst$log_date-dau.inst$log_date.inst)
dau.inst.7_13<-
  dau.inst[dau.inst$elapsed_days>=7 & dau.inst$elapsed_days<=13,]
head(dau.inst.7_13)
dau.inst.7_13.login.ds<-ddply(dau.inst.7_13,.(user_id),summarize,
                              density=length(log_date)/7)
head(dau.inst.7_13.login.ds)
unique(dau.inst.7_13.login.ds$density)
dau.inst.7_13.login.ds
target.install<-install[install$log_date>="2013-06-01" & install$log_date
                        <="2013-08-25",]
target.install.login.ds<-
  merge(target.install,dau.inst.7_13.login.ds,
        by="user_id",all.x=T)
target.install.login.ds$density<-
  ifelse(is.na(target.install.login.ds$density),0,
         target.install.login.ds$density)
head(target.install.login.ds)

battle.inst<-merge(battle,install,by="user_id"
                   ,suffixes = c("",".inst"))
                                                            
head(battle.inst)

battle.inst$log_date<-as.Date(battle.inst$log_date)
battle.inst$log_date.inst<-as.Date(battle.inst$log_date.inst)
battle.inst$elapsed_days<-as.numeric(battle.inst$log_date-battle.inst$log_date.inst)

battle.inst2<-battle.inst[battle.inst$elapsed_days>=0 & battle.inst$elapsed_days <=6,]
library(reshape2)
battle.inst2$elapsed_days<-paste0("d",battle.inst2$elapsed_days)
battle.inst2.cast<-dcast(battle.inst2,user_id~elapsed_days,value.var = "count",sum)
head(battle.inst2.cast)

battle.inst2.cast.prop<-battle.inst2.cast
battle.inst2.cast.prop[,-1]<-
  battle.inst2.cast.prop[,-1]/rowSums(battle.inst2.cast.prop[,-1])

