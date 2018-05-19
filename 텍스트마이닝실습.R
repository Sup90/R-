install.packages("KoNLP")
library("KoNLP")
install.packages("stringr")
library(stringr)

txt2<-"하하하 윤기광섭이다. 나는 윤광섭이지"
strsplit(txt2," ")
strsplit(txt2,"")
extractNoun(txt2)
a<-"words 수만큼 freq (빈도)를 나타낸다.
 등장하는 단어의 가장 작은 빈도 수는10로,
등장하는 단어의 가장 큰 빈도 수는 200이다.
빈도가 가장 큰 단어를 중앙에 두도록 하기 위해 random order는 False 값을 준다.
scale(폰트의 크기)은 최고 6픽셀에서, 제일 작은건 0.2 픽셀까지
rotation되는 단어의 빈도는 0.1정도로 하고, 
컬러는 위에서 정한 pal의 값으로 컬러 팔레트를 사용한다. "
extractNoun(a)
SimplePos22(a)
#품사 보는 함수
txt3<-SimplePos09(a)
txt_n<-str_match(txt3,"([A_Z가-힣]+)/N")
txt_n
SimplePos09(a)
txt_np <- str_match(txt3,'([A-Z가-힣]+)/[NP]')
txt_np
#명사만 보기

#extractNoun()- 한글 명사 추출
txt<-c( "나는 사과와 바나나를 좋아합니다." , "나는 바나나 바나나 바나나 바나나 바나나가 최고 좋아요!", "나는 복숭아와 사과를 좋아합니다." , "나는 복숭아와 사과를 좋아합니다." , "나는 사과와 포도를 좋아합니다.", "나는 파인애플과 복숭아를 좋아합니다." )
txt
txt2<-extractNoun(txt)
txt2
#extractNoun은 명사를 공백기준으로 추출
#즉 띄어쓰기가 잘못되면 명사 추출 실패
.libPaths()
#C:\Users\DS\Documents\R\win-library\3.5\KoNLP_dic\current
#들어가서 dic_user.txt를 보면 sejongdic을 실행하기 전에는 목록이 한정적
useSejongDic()
extractNoun(a)
# mergeUserDic(data.frame(c('유관순','안중근'),c('ncn')))
#단어 내부 사전에 추가
#mergeUserDic(data.frame(readLines("mergefile.txt"), "ncn"))
#단어장을 사전에 추가
#map,unique 함수를 통해 중복되는 명사 축약
q1<-Map(extractNoun,a)
q<-Map(extractNoun,txt)
q1
w<-unique(q)#중복 리스트 제거
w1<-unique(q1)
w1
w
txt4<-lapply(w,unique)#중복 단어 제거!
txt41<-lapply(w1,unique)
txt4
txt41
#gsub을 통한 특정글자 제거
txt5<-rapply(txt41, function(x)gsub("바나나","망고",x),how="replace")
txt5

#제거해야할 글자가 많다면 제거 단어를 파일에 저장해놓고 불러서 쓸 수 있다.
#Ex
readline(a)
txt41
a
z<-unlist(txt4)
z1<-unlist(txt5)
z1
length(txt5)
cnt_txt<-length(z1)
cnt_txt
for( i in 1:cnt_txt){
  txt5<-rapply(txt41,function(x)gsub((txt[i]),"",x),how="replace")
}

#extractNoun() 함수의 UTF-8 에러 처리하기
a2<-Filter(function(x){nchar(x)<=110},a1)
#글자수 일정 숫자 이하인 것만 넣기
a2
filte
a1<-gsub(" ","",a)
a2
?gsub
tran1<-Map(extractNoun,a1)
a1
a1<-"wordskkkkk만큼freq(빈도)를나타낸다.\n등장하는단어의가장작은빈도kkkkk는10로,\n등장하는단어의가장큰빈도kkkkk는200이다.\n빈도가가장큰단어를중앙에두도록하기위해rkkkkkndomorder는Fkkkkklse값을준다.\nsckkkkkle(폰트의크기)은최고6픽셀에서,제일작은건0.2픽셀까지\nrotkkkkktion되는단어의빈도는0.1정도로하고,\n컬러는위에서정한pkkkkkl의값으로컬러팔레트를사용한다.wordskkkkk만큼freq(빈도)를나타낸다.\n등장하는단어의가장작은빈도kkkkk는10로,\n등장하는단어의가장큰빈도kkkkk는200이다.\n빈도가가장큰단어를중앙에두도록하기위해rkkkkkndomorder는Fkkkkklse값을준다.\nsckkkkkle(폰트의크기)은최고6픽셀에서,제일작은건0.2픽셀까지\nrotkkkkktion되는단어의빈도는0.1정도로하고,\n컬러는위에서정한pkkkkkl의값으로컬러팔레트를사용한다."
a1<-gsub("\n","",a1)
a1
nchar(a1)
useNIADic()
useSejongDic()
extractNoun(" 글을 보신 누구나 편하게 신청 가능한 모임입니다.

            저희 커뮤니티에 궁금증을 가지신 분들은 하기 링크를 통해 함께하실 수 있습니다!! 
            
            
            
            데이터 분석 질문 답변 & 친목 톡방 : https://open.kakao.com/o/gcaPzHr
            
            개인 문의 : https://open.kakao.com/o/sQ019XL
            
            
            
            정모 목적
            
            - 공유 : 실무에서 개척 중인 '테이터' 분야에 대해 이론과 실무경험을 공유하여 함께 발전해 나가는 정모 지향
            
            - 네트워크 : 온라인 형식을 넘어선, 오프라인 '데이터' 모임을 바탕으로 현실 선상까지 이어지는 인적 네트워크 형성
            
            
            
            개요
            
            - 장소 : 홍익대학교(서울)
            
            - 시간 : 2018년 6월 2일 토요일 오후 4시 ~ 7시 + 뒷풀이
            
            - 예상 시간 : 세션 3시간+뒷풀이
            
            - 인원 : 선착순 최대 40명
            
            - 뒷풀이 장소 : 미정
            
            - 개인부담비 : 1인 10000원 ( 국민 97242054859 예금주 : 최선열 )
            
            -> 하기 신청 접수 후 입금 바랍니다. ( 발표자 제외 )
            
            신청 접수 : https://goo.gl/forms/o7x8l0So1xz1lLAg2
            
            
            
            현재까지 신청된 세션 주제 : 
            
            < 개회 >
            
            진행 : 방장, 부방장
            
            - 네트워크 타임
            
            - 데이터 분석 관련 가벼운 토의
            
            - 향후 커뮤니티 방향성
            
            < 1. 후 님 발표 >
            
            주제 : 데이터분석과 취업
            
            < 2. R치킨 님 발표 >
            
            주제 : 프로세스 마이닝을 이용한 컨설팅
            
            < 3. bear 님 발표 > 
            
            주제 : 한국어 감성 사전 기반의 감정 평가기법
            
            < 4. 링컨 님 발표 >
            
            주제 : 실무에서의 엑셀로 하는 회귀분석 
            
            
            
            
            
            * 누구나 참여 가능한 모임입니다 ! 실무자, 학생, 비전공자, 전공자, 관련 업종, 비관련 업종, 톡방 회원, 톡방 비회원 등등 편하게 참석 부탁드립니다!
            
            
            
            해당 모임을 통해 유익하고 즐거운 커뮤니티가 형성되기를 희망합니다.
            
            감사합니다.
            ")
a<-" 글을 보신 누구나 편하게 신청 가능한 모임입니다.

저희 커뮤니티에 궁금증을 가지신 분들은 하기 링크를 통해 함께하실 수 있습니다!! 



데이터 분석 질문 답변 & 친목 톡방 : https://open.kakao.com/o/gcaPzHr

개인 문의 : https://open.kakao.com/o/sQ019XL



정모 목적

- 공유 : 실무에서 개척 중인 '테이터' 분야에 대해 이론과 실무경험을 공유하여 함께 발전해 나가는 정모 지향

- 네트워크 : 온라인 형식을 넘어선, 오프라인 '데이터' 모임을 바탕으로 현실 선상까지 이어지는 인적 네트워크 형성



개요

- 장소 : 홍익대학교(서울)

- 시간 : 2018년 6월 2일 토요일 오후 4시 ~ 7시 + 뒷풀이

- 예상 시간 : 세션 3시간+뒷풀이

- 인원 : 선착순 최대 40명

- 뒷풀이 장소 : 미정

- 개인부담비 : 1인 10000원 ( 국민 97242054859 예금주 : 최선열 )

-> 하기 신청 접수 후 입금 바랍니다. ( 발표자 제외 )

신청 접수 : https://goo.gl/forms/o7x8l0So1xz1lLAg2



현재까지 신청된 세션 주제 : 

< 개회 >

진행 : 방장, 부방장

- 네트워크 타임

- 데이터 분석 관련 가벼운 토의

- 향후 커뮤니티 방향성

< 1. 후 님 발표 >

주제 : 데이터분석과 취업

< 2. R치킨 님 발표 >

주제 : 프로세스 마이닝을 이용한 컨설팅

< 3. bear 님 발표 > 

주제 : 한국어 감성 사전 기반의 감정 평가기법

< 4. 링컨 님 발표 >

주제 : 실무에서의 엑셀로 하는 회귀분석 





* 누구나 참여 가능한 모임입니다 ! 실무자, 학생, 비전공자, 전공자, 관련 업종, 비관련 업종, 톡방 회원, 톡방 비회원 등등 편하게 참석 부탁드립니다!



해당 모임을 통해 유익하고 즐거운 커뮤니티가 형성되기를 희망합니다.

감사합니다.
"
Encoding(a)<- "EUC-KR"
a1<-str_replace_all(a,"\\W"," ")
a1
a
a1
install.packages("tm")
library(tm)
a2<-tm_map(a3,stripWhitespace)
a2
class(a2)
a3<-Corpus(VectorSource(a))
a3
inspect(a3)
Encoding(a3$content)<- "UTF-8"
Encoding(a3$content)
a3$content
VectorSource(a1)
class(a3)
a3
inspect(a2$1)
tdm3
str(a2)
Encoding(a2$content)
a2$content
Encoding(a3$content)
tdm4<-TermDocumentMatrix(tdm3)
inspect(tdm4)
Encoding(a)<-"UTF-8"
a
a3$content
Encoding(tdm$dimnames)
Encoding(tdm$dimnames$Terms)
tdm$dimnames$Terms[Encoding(tdm$dimnames$Terms)=="unknown"]
inspect(tdm)
str(tdm)
as.matrix(tdm)
a4<-as.matrix(tdm)
a4
Encoding(a3)
a3
?TermDocumentMatrix
tdm1<-as.matrix(tdm)
tdm2<-tm_map(a3,stripWhitespace)
tdm2<-tm_map(tdm2,removeNumbers)
tdm2$content
tdm3<-tm_map(tdm2,removePunctuation)
tdm3$content
tdm4<-tm_map(tdm3,PlainTextDocument)
Encoding(tdm4$content$content)
tdm4$content$content
str(tdm4$content)
Encoding(tdm4$content$content)<-"UTF-8"
inspect(tdm3)
tm_map(tdm3,)
str(tdm3)
Encoding(tdm3$content)
tdm3$content<-gsub("[A-Za-z]","",tdm3$content)
tdm4<-TermDocumentMatrix(tdm3)
tdm4$dimnames
str(tdm4)
inspect(tdm4)
Encoding(tdm4$dimnames$Terms)<-"EUC-KR"
Encoding(tdm4$content$content)
inspect(tdm4$content)
Encoding(tdm3$dimnames$Terms)<- "UTF-8"
tdm4$content$content
inspect(tdm3$dimnames$Terms)
tdm3$dimnames
tdm3$dimnames$Terms
m2<-as.matrix(tdm3)
m2
Encoding(a)<- "EUC-KR"
tdm5<-tm_map(tdm4,removePunctuation)
inspect(tdm5$content)
Encoding(tdm5$content)
tdm6<-Corpus(vectorsource(tdm5))
tdm5
tdm6<-TermDocumentMatrix(tdm5)
inspect(tdm6)
Encoding(tdm6$dimnames$Terms)="UTF-8"
tdm6$dimnames$Terms
as.matrix(tdm6)
class(tdm3$content[1])
Encoding(tdm4$dimnames$Terms) = 'UTF-8'
tdm4
tdm4$dimnames$Terms
inspect(tdm4)
q<-as.matrix(tdm4)
q[3]
Encoding(a)<-"UTF-8"
Encoding(a)<-"EUC-KR"
a<-"媛\u0080<eb>뒫<ed>븳 "
a
aa<-TermDocumentMatrix(tdm3)
Encoding(aa$dimnames$Terms)<-"EUC-KR"
inspect(aa)
inspect(tdm3)
Encoding(tdm3$content)
tdm3$content
v