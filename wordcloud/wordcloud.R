
install.packages("tm")
library(tm)
library(KoNLP)
library(wordcloud2)
library(RColorBrewer)
useSejongDic() 
txt <- readLines("b.txt",encoding = "UTF-8") 
txt
data2<-sapply(txt,extractNoun,USE.NAMES = F)
data2
data3<-unlist(data2)
data3
data3<-Filter(function(x){nchar(x)>=2},data3)
data3<-gsub("\\d+","",data3)
data3<-gsub("[A-Za-z]","",data3)
data3<-gsub("_callback", "", data3)
data3<-gsub("\\(", "[", data3)
data3<-gsub("\\)", "]", data3)
data3<-gsub(";", "", data3)
data3<-gsub("\n", "", data3)
data3<-gsub("'",'"',data3)
data3 <-gsub("[[:punct:]]+", "", data3)
data3<-removePunctuation(data3,preserve_intra_word_dashes = TRUE)
data3<-gsub("[[:cntrl:]]","",data3)
data3<-Filter(function(x){nchar(x)>=2},data3)
write(unlist(data3),"cloudcomputing2.txt")
data4<-read.table("cloudcomputing2.txt",quote="")
?read.table()
wordcount<-table(data4)
wordcount
#png("a.png")




windowsFonts(font=windowsFont("ÈÞ¸ÕÆíÁöÃ¼"))
wordcloud2(wordcount,figPath="logo.png",size = 0.7, fontFamily="ÈÞ¸ÕÆíÁöÃ¼",color='random-dark' , backgroundColor="white",minSize=5,gridSize=8)

