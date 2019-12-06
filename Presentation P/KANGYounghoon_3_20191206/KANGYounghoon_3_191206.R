# 강영훈 20191205/20191206

# 2016-2018 연간 도내 전통시장 매출액과 카드 이용자수 및 입도관광객
library(rJava)
library(xlsx)
setwd ("C:/GitHub/BM_YounghoonKANG_R/PP/EXTRACTED")
jeju_market <- read.xlsx("제주전통시장 연도별 일간매출액-일간방문자수-제주관광객수.xlsx", sheetIndex = 1, encoding = 'UTF-8')
jeju_market #일간평균매출액(*10000원)/일간평균방문자수/일간입도관광객/연간입도관광객
is.na(jeju_market[,4]) #결측치 없다
boxplot(jeju_market[,4])

par(mfrow=c(1,3)) #각각 상자그림
for (i in 2:4) {
  boxplot(jeju_market [,i],
       main = colnames(jeju_market)[i])
}

plot(jeju_market[,2], type='o', col=1)

# 월별 2016.12~2017.11 도민/내국인/중국인/토탈 전통시장 카드이용자수/월간입도관광객
month <- 1:12
visitors_group <- read.csv("도민,내국인관광객,중국인관광객 전통시장 카드 이용수.csv", 
                            header = T)
visitors_group
str( visitors_group )

plot(month, visitors_group$visitors.all, main = "전통시장 방문객의 카드 이용 횟수",
     type="l", col="red", lty=1,
     xlab = "2016년 12월~2017년 11월", ylab = "카드 결제 횟수")
plot(month, visitors_group$travelers, main = "입도한 관광객 수",
     type="l", col="blue", las=1,
     xlab = "2016년 12월~2017년 11월", ylab = "입도관광객 수")
plot(visitors.all~travelers, data = visitors_group) #산점도

pairs(visitors_group)
par(mfrow=c(1,2)) #각각 상자그림
for (i in 5:6) {
  boxplot(visitors_group [,i], #전통시장 방문객과 입도관광객의 상자그림
          main = colnames(visitors_group)[i])
}

rowSums(is.na(visitors_group)) #결측치 하나도 없음음
cor(visitors_group[, c(5,6)])
par(mfrow=c(1,1))

visitors_group[,2]
localt <- mean(visitors_group[,2])
localt
koreant <- mean(visitors_group[,3])
koreant
chineset <- mean(visitors_group[,4])
chineset

visitors_pie <- c(localt, koreant,chineset)
lbls <- c('도민','내국인관광객','중국인관광객')
pct <- round(visitors_pie/sum(visitors_pie)*100, 2)
lbls <- paste(lbls, pct) #add percents to lables
lbls <- paste(lbls, "%", sep='')

pie(visitors_pie, main = ("도내 전통시장 카드 거래 이용자수 비율"),
    col = c('skyblue','blue','red'),
    labels <- lbls,
    radius = 2.5 ) # 카드 거래 이용자수 비율 파이 차트
plot(visitors_group$visitors.all,
        main = "월간 전통시장 카드 거래 이용자수",
        xlab = '2016.12 ~ 2017.11', ylim=c(10000,30000),
        ylab = "카드 거래 이용자수",
     type = 'l', col = 'blue')


# 입도객 X 전통시장 카드 이용자수 [월간 2016.12~2017.11]
visitors_group
plot(visitors_group$visitors.all~visitors_group$travelers,
     main = "월간 입도객별 전통시장 카드 이용자수",
     xlab="월간 입도객(2016.12~2017.11)",
     ylab="월간 전통시장 카드이용자수",
     col = 4, pch = 16)
cor(visitors_group[, c("visitors.all", "travelers")]) #산점도 0.6988378

# 입도객 X 전통시장 매출액 [연간 2016~2018]
inbound_year <- read.xlsx("연간제주관광입도객2012-2018.xlsx", sheetIndex = 1, encoding = 'UTF-8')
class(inbound_year)
jeju_market

plot(inbound_year,
     main = "연간 관광입도객 추이",
     col = "purple",
     type = 'l',
     xlab = '연도',
     ylab = '입도 관광객 수', ylim = c(7000000,17000000))


inb <- inbound_year[,2][5:7]; inb
sal <- jeju_market[,"Sales"]; sal
sal.inb <- data.frame(inb,sal); sal.inb
plot(sal.inb, main='연간 입도객별 전통시장 매출(2016-2018)',
     xlab = '연간 입도객', ylab = '연간 전통시장 매출',
     col = 4, pch = 16)
cor(sal.inb) # 산점도 -0.4977795


# 네이버 제주 시장 검색량 X 전통시장 신용카드 이용횟수
setwd ("C:/GitHub/BM_YounghoonKANG_R/PP/EXTRACTED")
naver <- read.xlsx("네이버 제주도 시장 검색량 201612-201711.xlsx", sheetIndex = 1, encoding = 'UTF-8')
naver
naver[,2]
plot(month, naver[,2], main = '네이버 - 도내 전통시장 관련 키워드 검색량',
     sub = '2016년 12월 ~ 2017년 11월 기준',
     type='l', col='green', ylim=c(0, 100),
     xlab = "월", ylab = '검색량 지수(최대치 = 100)')
visitors_group
plot (naver[,2], visitors_group$visitors.all,  
      main ="네이버 검색량에 따른 전통시장 결제 횟수",
      xlab = "네이버 검색량",
      ylab = "전통시장 카드 이용횟수",
      col = 4, pch = 16) #산점도
pay <- visitors_group$visitors.all
search <- naver[,2]
pay.search <- data.frame(pay, search)
cor(pay.search[,1:2]) # 상관계수 -0.1677105

