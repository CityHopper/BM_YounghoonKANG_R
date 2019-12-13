install.packages("xlsx") #search and install 'xlsx' in the box on the right
install.packages("rJava")

library(rJava) # 패키지 로드
library(xlsx) # 패키지 로드
library(ggplot2)

setwd("D:/New-one/BM_YounghoonKANG_R/Presentation G/191223")
inbound <- read.xlsx("제주도 전입수 2014-2018.xlsx", sheetIndex=1, encoding="UTF-8")


# 직업 선택 요인
preference <- c('명예 명성', '안정성','수입','적성 흥미','보람 자아실현','근무시간','발전성 장래성'	,'기타','잘 모르겠다')
value <- c(10.8,63.7,81.0,42.3,18.5,26.5,29.5,0.2,2.7)
total <- sum(as.data.frame(value))
total
             
names(value) <- preference
jobprf
pie(value, main = '직업 선택 요인')

value2 <- value
value2$total = sum(as.data.frame(value))

df <- data.frame(preference, value)
df
df$percent <- round(value/total*100, 1)
df


