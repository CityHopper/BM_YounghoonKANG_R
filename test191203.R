# day 6

#다중변수 자료 탐색
#두 변수 사이의 산점도
#산점도 scatter plot : 2변수로 구성된 자료의 분포를 알아보는 그래프 관측값들의
#분포를 통해 두 변수 사이의 관계를 파악

wt <- mtcars$wt
mpg <- mtcars$mpg
plot (wt, mpg, 
      main ="중량-연비 그래프",
      xlab = "중량", ylab = "연비",
      col = 4, pch = 16) #pch 0~25
plot (mtcars$wt, mtcars$mpg, 
      main ="중량-연비 그래프",
      xlab = "중량", ylab = "연비",
      col = 4, pch = 16) #pch 0~25
plot (mtcars[,c("wt","mpg")], 
      main ="중량-연비 그래프",
      xlab = "중량", ylab = "연비",
      col = 4, pch = 16) #pch 0~25
plot (wt~mpg, data = mtcars, #wt, mpg 콤마 사용 가능
      main ="중량-연비 그래프",
      xlab = "중량", ylab = "연비",
      col = 4, pch = 16) #pch 0~25

vars <- c("mpg", "disp", "drat", "wt")
target <- mtcars[, vars]
head(target)
pairs(target, main = "multi plots") #다변량 산점도: 대칭 관계[x좌표 y좌표 서로 바뀜]

#그룹 정보가 있는 두 변수의 산점도
iris.2 <- iris[,3:4]
iris$Species
point <- as.numeric(iris$Species) #범주형을 수치화
point #setosa 1, versi 2, virginica 3
iris.2
color <- c("red", "green", "blue") #1번 red 2번 green 3번 blue
plot(iris.2, main = "Iris plot",
     pch = c(point),
     col = color[point],
     )

#선형관계와 상관계수r [음] -1 <= r <= 1 [양]
#0.5 이상이면 상관관계가 높다
#상관분석

beers <- c(5,2,9,8,3,7,3,5,3,5)
bal <- c(0.1,0.03,0.19,0.12,0.04,0.095,0.07,0.06,0.02,0.05)
tbl <- data.frame(beers, bal)
tbl
plot(tbl)
plot(bal~beers, data = tbl) #산점도

#회귀식 y = xw + b (종속변수는 독립변수x*weight w + bias b)
res<-lm(bal~beers, data = tbl) #회귀식 구하는 함수 : weight 값이 나옴
res
abline(res) #최적의 예측선 출력
cor(tbl[, 1:2]) #상관계수
cor(iris[, 1:4])

#상관분석 순서
# 1. 상관분석 대상 변수 선정
# 2. 산점도 작성
# 3. 회귀식 도출: lm()
#   (회귀식: 두 변수의 선형관계를 가장 잘 나타낼 수 있는 선의 식)
#   (y = xw + b)
# 4. 회귀선을 산점도에 표시 : abline()
#   (회귀선: 관측값들의 추세를 가장 잘 나타낼 수 있는 선)
# 5. 상관계수 계산: cor()
# 6. 상관분석 결과 해석


#시계열 data - 선그래프
month <- 1:12
late <- c(5,8,7,9,4,6,12,13,8,6,6,4)
plot(month, late, main = '지각생통계', #선그래프
     type = "l", lty = 1, lwd = 1, #lty 선의 모양, lwd 선의 굵기
     xlab = "Month", ylab = "late cnt")
plot(month, late, main = '지각생통계', #선그래프 + 점
     type = "b", lty = 1, lwd = 1,
     xlab = "Month", ylab = "late cnt")
plot(month, late, main = '지각생통계', #선그래프 + 점 [연속]
     type = "o", lty = 1, lwd = 1,
     xlab = "Month", ylab = "late cnt")
plot(month, late, main = '지각생통계', #
     type = "s", lty = 1, lwd = 1,
     xlab = "Month", ylab = "late cnt")

#복수의 선 그래프
late1 <- c(5,8,7,9,4,6,12,13,8,6,4,2)
late2 <- c(4,6,5,8,7,8,10,11,6,5,7,3)
plot(month, late1, main = '지각생통계', 
     type = "l", lty = 1, lwd = 1, col = "red",
     xlab = "Month", ylab = "late cnt",
     vlim = c(1,15))
lines(month, late2, type = "l", col = "blue")

#자료 탐색 실습 - 탐색적 데이터 분석
#0단계: 문제 정의
#1단계: 분석 대상 데이터셋 준비
#       BostonHousing 데이터셋(mlbench pac.)
install.packages('mlbench')
library(mlbench)
data("BostonHousing")
#crim 1인당 범죄율
#rm 주택 1가구당 방수
#dis 보스턴 5개 지역센터까지의 거리
#tax 재산세율
#medv 주택가격

class(BostonHousing)
dim(BostonHousing)
str(BostonHousing)
head(myds)
tail(BostonHousing)
myds <- BostonHousing[ , c("crim",
                           "rm",
                           "dis",
                           "tax",
                           "medv")]
myds
class(myds)
#2단계 파생변수 추가: grp 변수 추가(주택 가격 상중하)
grp <- c() #주택 가격 등급 나누기 (반복문)
for (i in 1:nrow(myds)) {
  if (myds$medv[i] >= 25.0){
    grp[i] <- "H"
  }else if(myds$medv[i] <= 17.0){
    grp[i] <- "L"
  }else {
    grp[i] <- "M"
  }
}
grp ####################################
grp <- factor(grp)
grp <- factor(grp, levels = c("H","M","L"))
myds <- data.frame(myds, grp)
head(grp)
class(myds)
myds

#3단계 데이터셋의 형태와 기본적인 내용 파악
str(myds)
head(myds)
table(myds$grp)

#4단계: 히스토그램에 의한 관측값의 분포확인
par(mfrow= c(2,3))
for (i in 1:5) {
  hist(myds [,i],
       main = colnames(myds)[i],
       col="yellow")
}
par(mfrow=c(1,1))
myds

#5단계: 상자그림에 의한 관측값의 분포 확인
par(mfrow= c(2,3))
for (i in 1:5) {
  boxplot(myds [,i],
       main = colnames(myds)[i])
}
par(mfrow=c(1,1))

# 6단계: 그룹별 관측값 분포 확인
boxplot(myds$crim~myds$grp, 
        main = "1인당 범죄율")
boxplot(myds$rm~myds$grp, 
        main = "방의 개수")

#7단계: 다중 산점도를 통환 변수간 상관관계 확인
pairs(myds[,-6]) #주택가격 등급 H/M/L 제외

#8단계: 그룹 정보를 포함한 변수간 상관관계 확인
point <- as.integer(myds$grp)
color <- c("red", "green", "blue")
pairs(myds[,-6], pch=point, col=color[point])

#9단계: 변수간 상관계수 확인 -1 <= r <= 1 (0.5이상 = 상관관계 높은 편)
cor(myds[, -6])
