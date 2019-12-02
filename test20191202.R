#day 5

setwd ("D:/KANGYounghoon/BM_ClassMaterial_R")
df <- read.table(file = "airquality.txt", header = T)
df
class (df)
dim(df)
str(df)
head(df, 3)
tail(df, 2)

install.packages("xlsx") #search and install 'xlsx' in the box on the right
install.packages("rJava")

library(rJava)
library(xlsx)

df.xlsx <- read.xlsx ("airquality.xlsx", sheetIndex = 1,
                      encoding = "UTF-8")
df.xlsx
class(df.xlsx)
str(df.xlsx)
head(df.xlsx, 3)
tail(df.xlsx, 3)

score <- c(76,84,69,50,95,7,82,71,88,84)
which(score == 69)
which(score >=85)
which.max(score)
min(score)
which.min(score)

idx <- which(score >= 60)
score [idx] <- 61
score

idx <- which(iris[, 1:4] > 5.0, arr.ind = TRUE)
idx

#단일변수(일변량) 범주형 자료탐색 #################################################
favorite <-c('WINTER','SUMMER',  'SUMMER', 'SPRING'
            , 'SUMMER', 'FALL','FALL', 'SUMMER', 'SPRING', 'SPRING')

favorite
class(favorite)
table(favorite)
table(favorite) / length(favorite)
ds <- table(favorite)
ds
barplot(ds, main = 'favorite season')

ds.new <- ds[c(2,3,1,4)] #봄여름가을겨울 순서로 바꿈
ds.new
barplot(ds.new, main="favorite season") #bar chart 막대그래프
pie(ds, main = 'favorite season')
pie(ds.new, main = 'favorite season') #pie chart원그래프
help(pie)

favorite.color <-c(2,3,2,1,1,2,2,1,3,2,1,3,2,1,2)
ds <- table(favorite.color)
ds
barplot(ds, main = 'favorite season')
colors <- c('yellowgreen', 'skyblue', 'navy')
names(ds) <- colors; ds
barplot(ds, main = 'favorite season', col = colors) # col = 색상 입력 인수 
pie(ds, main = 'favorite season', col = colors)

# 단일변수(일변량) 연속형 자료 탐색 ################################################
weight <- c(60,62,64,65,76, 69)
weight.heavy <- c(weight, 120); weight.heavy
#평균
mean(weight); mean(weight.heavy)
#중앙값
median(weight); median(weight.heavy)
#절사평균: 상위 일부와 하위 일부 제거 (예시에선 20%)
mean(weight, trim = 0.2); mean(weight.heavy, trim = 0.2)
#사분위수: 이사분위(25%)값이 중앙값과 같다. 일사분위부터 삼사분위까지 대략 50%.
quantile(weight.heavy) #0가 최소값, 100%가 최대값
quantile(weight.heavy, (0:10)/10)
summary(weight.heavy) #이사분위=중앙값=median

#산포 distribution
#분산 Variance
var(weight); var(weight.heavy)
#표준편차=분산의 제곱근 - standard deviation
sd(weight); sd(weight.heavy)
#값의 범위(최소값과 최대값)
range(weight); range(weight.heavy)
#최대값과 최소값의 차이
diff(range(weight)); diff(range(weight.heavy))


#histogram: 연속형 자료의 분포를 시각화 
#[연속형이라 그래프가 붙어 있음. 떨어져있는 건 막대그래프 bar chart]
#연속형 자료에서는 구간을 나누고 구간에 속한 값들의 개수를 세는 방법으로 사용
str(cars)
dist <- cars [, 2]; dist
hist (dist, main = "Histogram for 제동거리",
      xlab = '제동거리', ylab = '빈도수',
      border = 'yellow', col = 'navy',
      las = 0, breaks = 2) #las는 축 서식 글자방향/breaks는 막대의 수

#상자그림(boxplot, 상자수염그림)
#사분위수를 시각화하여 그래프 형태로 표시
#상자그림은 하나의 그래프로 데이터의 분포형태를포함한 다양한 정보를 전달
#자료의 전반적인 분포를 이해하는데 도움
#구체적인 최소/최대/중앙값을 알기는 어렵다
boxplot(dist, main ="자동차 제동거리")
boxplot.stats(dist)
boxplot.stats(dist)$stats #정상범위 사분위수
boxplot.stats(dist)$n     #관측치 개수
boxplot.stats(dist)$conf  #중앙값 신뢰구간
boxplot.stats(dist)$out   #이상치(특이값) 목록

#일변량 중 그룹으로 구성된 자료의 상자그림
boxplot(Petal.Length~Species, #기준은 길이, 종별로 보겠다.
        data = iris, 
        main = '품종별 꽃잎 길이')
boxplot(iris$Petal.Length~iris$Species,
        main = '품종별 꽃잎 길이')

#한 화면에 여러 그래프 그리기
par(mfrow = c(2,3)) # 2X3 가상화면 분할
barplot(table(mtcars$carb), main = "zzz",
        xlab = "carburetors", ylab = 'freq',
        col = 'blue')
barplot(table(mtcars$cyl), main = "yyy",
        xlab = "cyl", ylab = 'freq',
        col = 'navy')
barplot(table(mtcars$gear), main = "xxx",
        xlab = "gear", ylab = 'freq',
        col = 'skyblue')
par(mfrow = c(1,1)) #가상화면 분할 해제

