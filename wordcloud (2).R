setwd("C:\\Users\\student\\Desktop")

library(KoNLP)
library(wordcloud)
library(RColorBrewer)
useSejongDic() 
txt <- readLines("a.txt") 
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
data3<-gsub("[[:cntrl:]]","",data3)
write(unlist(data3),"cloudcomputing2.txt")
data4<-read.table("cloudcomputing2.txt")
wordcount<-table(data4)
wordcount
png("c.png")
palete<-(brewer.pal(4,"Set2"))
wordcloud(names(wordcount),freq=wordcount,rot.per=0.25,min.freq = 3,
          random.order=F,random.color=T,colors=palete)
dev.off()

