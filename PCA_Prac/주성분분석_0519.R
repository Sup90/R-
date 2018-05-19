#주성분 분석 연습
#R 데이터 변환 : (5) 차원 축소 - (5-1) 주성분분석 (PCA) 참고
#http://rfriend.tistory.com/61
getwd()
setwd("c:/Users/rhkdt/Desktop/R-/")
a<-read.csv("PCA_Prac/secu_com_finance_2007.csv",header = T,stringsAsFactors = F)
head(a)
# V1 : 총자본순이익율
# V2 : 자기자본순이익율
# V3 : 자기자본비율
# V5 : 자기자본회전율
# V4 : 부채비율
  
#주성분 분석을 위해서는 단위를 표준화 한 데이터를 사용해야 데이터 왜곡을 피할 수 있다.
library(dplyr)
install.packages("dplyr")
a.scale<-ddplyr)
library(apply)
install.packages("apply")
apply()
a.2 <- transform(a,  V1_s = scale(V1),  
                   V2_s = scale(V2),  
                   V3_s = scale(V3),  
                  V4_s = scale(V4), 
                  V5_s = scale(V5))
a.2
a.scale<-apply(a[,2:ncol(a)],2,function(x) scale(x))
head(a.scale)
a.scale<-cbind(a[,1],a.scale)
a.scale
#표준화 방안 2가지
system.time(a.2 <- transform(a,  V1_s = scale(V1),  
                             V2_s = scale(V2),  
                             V3_s = scale(V3),  
                             V4_s = scale(V4), 
                             V5_s = scale(V5)))
system.time(a.scale<-apply(a[,2:ncol(a)],2,function(x) scale(x)))
cor(a.scale[,-1])
#V4는 부채비율이기 때문에 나머지 변수와 방향이 반대로 움직인다.
#그렇게 때문에 scale 이후 V4 맥스 값에서 관찰값을 빼는 방법으로 
# V4의 방향을 변환하여  나머지 변수와 변화 방향을 같게 하여
# 같은 주성분에 반영되록 변환 
a.scale<-transform(a.scale,V4=max(V4)-V4)
head(a.scale)
a.scale<-a.scale[,-6]
cor(a.scale[,-1])
plot(a.scale[,-1])
a.prcomp<-prcomp(a.scale[,2:ncol(a.scale)])
summary(a.prcomp)
plot(a.prcomp,type="l")
#주성분 3개용 까지 사요
biplot((a.prcomp))
print(a.prcomp)

#pc2는 낮은것이 좋음(부호가 음수)

#개꿀 코드 주성분 분석 누적기여율 0.8이 되는 주성분의 갯수
#찾아주는 코드
###########################################################
## PCA (Principal Component Analysis)
## User Defined Function
##  - finding PC k which Cumulative드 Proportion is over 0.8
###########################################################

   
 pca <- function(dataset){
     pc = prcomp(dataset, scale = TRUE)
       k = 0
       R = 0
     
         while(R < 0.8) {
             k = k +  1
             R = sum(pc[[1]][1:k]^2)/sum(pc[[1]]^2)
         
               cat("When number of Principal Component(k) is ", k, 
                             ", Cumulative Proportion(R) is ", R, "\n", "\n", sep="")
           }
       
         SelectedDataSet = pc[[5]][,1:k]
         return(SelectedDataSet)
       }

 pca(a[,c(2:6)])


