#apply계열 함수 정리

#apply
#apply() function is the base function.
#We will learn how to apply family functions by trying out the code. 
#apply() function takes 3 arguments:
#apply는 기본 함수로서, 3가지 인자를 갖는다
#데이터, 
#row/col(1은 row방향으로, 2는 col방향으로), 
#데이터에 적용 될 함수 function
apply(iris[,1:3],1,mean)
iris
apply(iris[,1:3],2,function(x) x%%10)
#R 산술 연산자
#^, ** 지수승
# %/%정수형 나머지(나머지 값 버림)
# %% 나머지
iris[,1:3]

#lapply
# lapply function is applied for operations on 
# list objects and returns a list object of same length of original set.
# lapply function in R, returns a list of the same length as input list object, 
# each element of which is the result of applying FUN 
# to the corresponding element of list.
# lapply함수는 리스트 객체 작업에 사용되는 함수로서 적용된 리스트와 
# 같은 길이의 리스트를 반환한다.
# R에서 lapply는 각 원소에 함수를 적용한 결과를 인풋데이터와 같은 길이로 반환한다. 

lapply(iris[,1:3],max)
lapply(iris[,1:3],sum)
lapply(iris[,1:3],function(x) (x*10)%%10)
lapply
class(iris[,2][1])
iris[,2][1]

#sapply
# sapply is wrapper class to lapply with difference being 
# it returns vector or matrix instead of list object.
# sapply는  lappy와 달리 벡터나 매트릭스 형태로 반환하는 
# wrapper클래스이다.
# sapply(txt6, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 },x)} )
# > sapply(iris[1:3],mean)
# Sepal.Length  Sepal.Width Petal.Length 
# 5.843333     3.057333     3.758000 
# > class(sapply(iris[1:3],mean))
# [1] "numeric"
# 
# > lapply(iris[1:3],mean)
# $`Sepal.Length`
# [1] 5.843333
# 
# $Sepal.Width
# [1] 3.057333
# 
# $Petal.Length
# [1] 3.758
# > class(lapply(iris[1:3],mean))
# [1] "list"


#tapply
# tapply() is a very powerful function that lets 
# you break a vector into pieces and then apply 
# some function to each of the pieces. 
# In the below code, first each of mpg in mtcars data 
# is grouped by cylinder type and then mean() function is calculated.
# tapply는 매우 강력한 함수로서 벡터를 등분해서 각등분에 함수를 적용할수 있다.
# tapply는 3개의 인자 (쪼갤데이터,쪼갤기준,함수)
tapply(iris[,1:4],iris[,5],mean)

#by
# by works similar to group by function in SQL, 
# applied to factors, where in we may apply operations on 
# individual results set. In the below example, 
# we apply colMeans() function to all the observations 
# on iris dataset grouped by Species.
# by는 sql의 group 함수와 비슷한 역할을 하며 개별 결과셋에 함수를 실행한 
# factor에 적용된다. 
by(iris[,1:4],iris[,5],colMeans)
