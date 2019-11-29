## DAY 4
## 함수 반환값(return값)이 여러 개일 때의 처리

myfunc <- function(x,y){
  val.sum <- x + y
  val.mul <- x * y
  return (list(sum = val.sum, mul = val.mul))
}

result <- myfunc(5,8)
s <- result$sum
m <- result$mul
cat('5 + 8 = ' , s, '\n')
cat('5 * 8 = ' , m, '\n')

#Matrix 생성
z <- matrix(1:20, nrow = 4)
z

z <- matrix(1:20, ncol = 4)
z

z <- matrix(1:20, nrow = 4, ncol = 5)
z

z <- matrix(1:20, nrow = 4, ncol = 5, byrow = T)
z

x <- 1:4
x
y <- 5:8
y
z <- matrix(1:20, nrow = 4, ncol = 5)
z 

m1 <- cbind(x,y)
m1
m2 <- rbind(x,y)
m2
m3 <- rbind(m2,x)
m3
m4 <- cbind(z,x)
m4

#matrix에서 cell의 값 추출
z[2,3]
z[1,4]
z[2, ]
z[,4]

z[2, 1:3]
z[1, c(1,2,4)]
z[1:2,]
z[, c(1,4)]

#Matrix에서 행/열에 이름 지정
score <- matrix(c(90,85,69,78,
                  85,96,49,95,
                  90,80,70,70),
                nrow = 4, ncol = 3)
score
rownames(score) <- c("KANG1", "Young2", "Sonny3", "Pengsoo4")
colnames(score) <- c("English", "Thai", "German")
score

score["KANG1", "Thai"]
score["Young2", c("Thai", "German")]
score["Sonny3",]
score[, "English"]
rownames(score)
colnames(score)
colnames(score)[2]

#Data Frame 생성
city <- c("Seoul", "Tokyo", "Washington")
rank <- c(1,3,2)
city.info <- data.frame(city, rank)
city.info

name <- c("KANG","PENG","SOO")
age <- c(40, 32, 10)
gender <- factor(c("M","N","F"))
blood.type = factor(c("O","?","A"))
person.info <- data.frame(name,age,gender,blood.type)
person.info

person2.info <- data.frame(name = c("KANG", "PENG","SOO"),
                           age = c(40,32,10),
                           gender = factor(c("M","N","F")),
                           blood.type = factor (c("O", "?","A")))
person2.info

city.info[1,1]
city.info[1, ]
city.info[, 1]
city.info[city.info$city,]
city.info[, "rank"]
city.info["rank"]

person.info$name
person.info[person.info$name == "KANG",] 
person.info[person.info$name == "KANG"] #콤마 하나 안찍으면 다르다
person.info[person.info$name == "KANG", c("name", "age")]

data()

iris

iris[, c(1:2)]
iris[, c(1,3,5)]
iris[, c("Sepal.Length", "Species")]
iris[1:5]; iris[1:5, c(1,3)]

#matrix와 data frame에서 사용하는 함수
dim(person.info) #often-used 관측치의 수와 컬럼(변수)의 수 (중요)
dim(iris)
nrow(person.info)
nrow(m3)
ncol(person.info)
ncol(m3)
head(iris) #often-used (중요)
tail(iris) #often-used(중요)
str(iris) #often-used #자료 구조 확인 (중요)
str(city.info)
iris[, 5]
unique(iris[, 5])
table(iris[, "Species"]) #often-used 팩터타입일때ㅑ 가능. 종류별 횟수 (중요)
table(person.info[ , "blood.type"])
table(person.info[ , "gender"])


#Matrix/DataFrame 사용 함수
#행별/열별 합계와 평균 계산
colSums(iris[ ,-5]); apply(iris[ , 1:4], 2, sum) #5번째 열은 팩터형이라 연산 안됨. 수치형만 가능 2가 뭐야?
colMeans(iris[ ,-5]); apply(iris[ , 1:4], 2, mean)
rowSums(iris[ ,-5]); apply(iris[ , -5], 1, sum)
rowMeans(iris[ ,-5]);  apply(iris[ , -5], 1, mean)
apply(iris[ , -5],2,median)

  #행, 열 방향 전환
z <- matrix(1:20, nrow = 4, ncol = 5); 
z #전치행렬
t(z)

#조건에 맞는 행과 열의 값 추출(데이터 프레임만 가능)
IR.1 <- subset(iris, Species == "setosa"); IR.1
IR.2 <- subset(iris, Sepal.Length > 5.0 & Sepal.Width > 4.0); IR.2
IR.2[ , c(2,4)]

#matrix/dataframe산술 연산
a <- matrix(1:2, 4, 5); a
b <- matrix(21:40, 4, 5) ; b

2*a
b-5
2*a + 3*b

a+b
b-a
b/a
a*b

#matrix/dataframe 자료구조 확인/변환
class(iris); str(iris);
class(state.x77); str(state.x77)
is.matrix(iris)
is.data.frame(iris)
is.matrix(state.x77)
is.data.frame(state.x77)

st <- data.frame(state.x77)
str(st)
head(st)
class(st)

iris.m = as.matrix(iris[ , 1:4])
head(iris.m)
class(iris.m)
