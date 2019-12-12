install.packages("xlsx") #search and install 'xlsx' in the box on the right
install.packages("rJava")

library(rJava) # 패키지 로드
library(xlsx) # 패키지 로드
library(ggplot2)

setwd("D:/New-one/BM_YounghoonKANG_R/Presentation G/191223")
inbound <- read.xlsx("제주도 전입수 2014-2018.xlsx", sheetIndex=1, encoding="UTF-8")


