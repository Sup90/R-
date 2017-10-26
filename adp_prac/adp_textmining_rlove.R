txt1<-"Start R programming wiht R-LOVE book."
txt1
strsplit(txt1," ")
library(KoNLP)
library(stringr)
txt2<-"R¶óºä Ã¥À¸·Î R ÇÁ·Î±×·¡¹ÖÀ» ½ÃÀÛÇÏ¼¼¿ä~!"
txt2
strsplit(txt2," ")
extractNoun(txt2)
SimplePos09(txt2)
txt3<-"R¶óºä Ã¥À¸·Î R ÇÁ·Î±×·¡¹ÖÀ» ½ÃÀÛÇÏ¼¼¿ä~!"
txt4<-SimplePos09(txt3)
txt4
txt_n<-str_match(txt4,'([A-Z°¡-ÆR]+)/N') #¸í»çÈ®ÀÎ
txt_n
txt_p<-str_match(txt4,'([A-Z°¡-ÆR]+)/P')
txt_p
txt_NP<-str_match(txt4,'([A-Z°¡-ÆR]+)/NP')
txt_NP
txt_J<-str_match(txt4,'([A-Z°¡-ÆR]+)/J')
txt_J
txt_X<-str_match(txt4,'([A-Z°¡-ÆR]+)/X')
txt_X
v2<-("º½ÀÌÁö³ª ¸é¿©¸§ÀÌ°í ¿©¸§ÀÌÁö³ª¸é°¡À» ÀÔ´Ï´Ù")
extractNoun(v2)
useSejongDic( )
txt_5 <- "¿ì¸®´Â À¯°ü¼ø ÀÇ»ç¿Í ¾ÈÁß±Ù ÀÇ»ç°¡ µ¶¸³Åõ»çÀÓÀ» ¹Ýµå½Ã ±â¾ïÇÕ½Ã´Ù"
extractNoun(txt_5)
buildDictionary(data.frame(c('À¯°ü¼ø','¾ÈÁß±Ù'),c('ncn')))
?buildDictionary
txt1<-readLines("ÁÁ¾ÆÇÏ´Â°úÀÏ.txt")
txt1
Map(extractNoun,txt1)
gsub(¡°º¯°æÀü±ÛÀÚ¡± , ¡±º¯°æÈÄ±ÛÀÚ¡± , data)
txt5<-rapply(txt4,function(x) gsub("¶óºä","",x),how="replace")
txt5
data2<-Map(extractNoun)
tran1 <- Map(extractNoun, data1)
library(wordcloud2)
install.packages("wordcloud2")
wordcount2 <- head(sort(wordcount, decreasing=T),100)
setwd("C:\\Users\\student\\Desktop\\¼ö¾÷ ¹®¼­\\R\\4-1ETL~1")
data<-readLines("jeju.txt")
data
train1<-Map(extractNoun,data)
train2<-unique(train1)
train3<-sapply(train1,unique)
train3
train3<-rapply(train3,function(x) gsub("È¿À²","",x),how="replace")
train5<-removePunctuation(train4,preserve_intra_word_dashes = TRUE)
train4<-unlist(train3)
train5
train3
train4 <- sapply(train3, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1},x)} )
train4
write(unlist(train4),"abc.txt")
data4 <- read.table("abc.txt")
data4
wordcount3<-Filter(function(x){nchar(x)>5},wordcount)
hist(wordcount3)
(wordcount<-table(data4))
data6<-removePunctuation(data5,preserve_intra_word_dashes = TRUE)
str(data5)
data5<-unlist(data4)
wordcount
wordcount2<-Filter(function(x){nchar(x)<=10},wordcount)
wordcount3<-
library(RColorBrewer)
palate<-brewer.pal(7,"Set2")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=5,random.order=F,random.color=T,colors=palate)
library(wordcloud)
legend(0.3,1 ,legend="¿µÈ­ ´ñ±Û ºÐ¼® - ¹ÐÁ¤ ")
a<-iris

a$seg
for(i in 1:nrow(iris)){
  a$seg[i]<-sample(1:6,1)}
a$seg
if(a$Sepal.Length>3.5){}
apply(iris[,1:4],MARGIN = iris$Species,sum)
cor(iris[,1:4],method = "pearson")
read.csv()

str(wordcount)
a<-unlist(wordcount)
order(a)
head(sort(wordcount,decreasing = T),30)


findAssocs(TermDocumentMatrix(crude),"oil",0.7)
data("crude")
data(acq)
to_dtm<-function(corpus,label){
  x<-tm_map(corpus,tolower)
  x<-tm_map(corpus,removePunctuation)
  return(DocumentTermMatrix(x))
  
}
crude_acq<-c(to_dtm(crude),to_dtm(acq))
crude_acq
crude_acq_df<-cbind(as.data.frame(as.matrix(crude_acq)),
                    LABEL=c(rep("crude",20),rep("acq",50)))

str(crude_acq_df)
length(crude_acq_df$LABEL)
crude_acq_df
