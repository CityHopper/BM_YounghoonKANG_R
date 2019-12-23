
# 계절근로자제 농어가 고용주 애로사항
p <- c('의사소통 어려움', '월급 부담', '일을 못함')
n <- c(44.2, 33.0, 22.8)

names(n) <-p
label <- paste(names(n), "\n", n/sum(n)*100, "%")
label
pie(n, labels = label)

# 계절근로자제 농어가 고용주 건의사항
p <- c('배정인원 확대', '고용기간 연장', '숙박비 유상화')
n <- c(11.2, 71.3, 17.5)

names(n) <-p
label <- paste(names(n), "\n", n/sum(n)*100, "%")
label
pie(n, labels = label)


# 계절근로자제 입국자
library(ggplot2)
year <- c(2015, 2016, 2017, 2018)
season<- c(19,200,1086,2822)
seasoninb <- data.frame(year, season)
seasoninb
ggplot(seasoninb, aes(year, season))+
  geom_line(color="#F14B69", size =2)

# 계절근로자제 이탈자
year <- c(2015, 2016, 2017, 2018)
nout <- c(0,4,18,93)
outyear <- data.frame(year, nout)
ggplot(outyear, aes(year, nout))+
  geom_line(color="#F14B69", size = 2)


library(rJava)
library(xlsx)
Sys.setlocale("LC_ALL","korean") 
# 외국인 이민자 일자리 찾는 데 어려움 설문 조사 결과
setwd("C:/GitHub/BM_YounghoonKANG_R/Presentation G/191223-2")
forejobprob <- read.xlsx("실업자가_구직_시_경험한_어려움의_원인_복수응답__이민자(외국인only)__20191221183406.xlsx", sheetIndex=1, encoding="UTF-8")

View(forejobprob)

library(ggplot2)
ggplot(forejobprob, aes(x = reorder(항목, 외국인), y = 외국인)) +
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "gray") +
  ggtitle ("실업자가 구직 시 경험한 어려움의 원인") + # 차트 제목
  theme(plot.title = element_text(size = 25,
                                  face = "bold",
                                  color = 'black')) +
  labs(subtitle = "(복수응답, 외국인 이민자, 2019)") + 
  coord_flip() #가로-세로 flip

# 실업자의 구직경로(복수응답, 외국인이민자)
Sys.setlocale("LC_ALL","korean") 
setwd("C:/GitHub/BM_YounghoonKANG_R/Presentation G/191223-2")
foreunemp <- read.xlsx("실업자의 구직경로(복수응답, 외국인이민자).xlsx", sheetIndex=1, encoding="UTF-8")
foreunemp[1:6,]

ggplot(foreunemp[1:6,], aes(x = reorder(항목, 응답결과), y = 응답결과)) +
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "gray") +
  ggtitle ("실업자의 구직 경로") + # 차트 제목
  theme(plot.title = element_text(size = 25,
                                  face = "bold",
                                  color = 'black')) +
  labs(subtitle = "(복수응답, 외국인 이민자, 2019)") + 
  coord_flip() #가로-세로 flip

# 외국인 고용 지원 센터 현황
library(ggmap)
register_google(key = 'AIzaSyCs0wxvwUGyb7GgWNhlNB4NdfnWh78c5Tg')
names <- c("제주시 우리은행")
gc <- geocode(enc2utf8(names))
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     size=c(640,640),
                     marker = gc)
ggmap(map)
dev.off()

# 전국-제주 체류자격별 등록 외국인


# 제주도 시별 외국인 인구수 ggmap
library(ggmap)
register_google(key = 'AIzaSyCs0wxvwUGyb7GgWNhlNB4NdfnWh78c5Tg')
names <- c("제주시","서귀포시")
gc <- geocode(enc2utf8(names))
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     size=c(640,640),
                     marker = gc)
ggmap(map) # 제주시 서귀포시 마커 표시 지도


setwd("D:/GitHtub/BM_YounghoonKANG_R/Presentation G/191223-2")
cjucity <- read.xlsx("2018 제주도 시별_세대_및_인구.xlsx", sheetIndex=1, encoding="UTF-8")
cjucityfor <- cjucity[-1,-c(1,3,5,6)]; cjucityfor
names <- as.character(cjucityfor[,"행정시별"])
gc <- geocode(enc2utf8(names)) # enc2utf8
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat,count=cjucityfor$외국인.계.)
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 10)
ggmap(map) +
  geom_point(data = df,
             aes(x = lon, y = lat, size = count),
             alpha = 0.6, color = "#F14B69") +scale_size_continuous(range=c(10,24)) + 
  geom_text(data = df,
            aes(x = lon-0.1, y = lat), # 텍스트 위치
            size = 5, # 텍스트 크기
            label = df$count) # 텍스트 이름

# 제주도 읍면동별 외국인 인구수 ggmap

cjutown <- read.xlsx("2018 제주도 읍면동별_세대_및_인구.xlsx", sheetIndex=1, encoding="UTF-8")
cjutownfor <- cjutown[-c(1,2,29),-c(1,3)]; cjutownfor
names <- as.character(cjutownfor[,"행정시별.2."])
gc <- geocode(enc2utf8(names)) # enc2utf8
df1 <- data.frame(name = names, lon = gc$lon, lat = gc$lat,count=cjutownfor$외국인)
cen <- c(33.698274, 126.463464)
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 9)
ggmap(map) +
  geom_point(data = df1,
             aes(x = lon, y = lat, size = count),
             alpha = 0.6, color = "#F14B69") +scale_size_continuous(range=c(1,14))

# 제주도 읍면동별 외국인 인구수 treemap
library(treemap)
cjutown2 <- cjutown[-c(1,2,29),-1]; cjutown2
foreignrate <- cjutown2$외국인/cjutown2$제주.인구
cjutown3 <- cbind(cjutown2, foreignrate); cjutown3


st <- data.frame(state.x77)
st <- data.frame(st, stname = rownames(st))
st
treemap(cjutown3, index = c("행정시별.2."),
        vSize = "외국인",
        vColor = 'foreignrate',
        type = 'value',
        title = '제주도 외국인 읍면동별 외국인 인구 및 비율')



# # install.packages("stringi")
# # install.packages("devtools")
# library(devtools)
# devtools::install_github("cardiomoon/kormaps2014")
# library(kormaps2014)
# library(moonBook2)
# library(dplyr)
#   
# library(rJava) # 패키지 로드
# library(xlsx) # 패키지 로드
# library(ggplot2)
# 
# # kormap1 : 2014년 한국행정지도(시도별)
# # kormap2 : 2014년 한국행정지도(시군구별)
# # kormap3 : 2014년 한국행정지도(읍면동별)
# 
# setwd("D:/GitHtub/BM_YounghoonKANG_R/Presentation G/191223-2")
# cjutowfor <- read.xlsx("2018 제주도 읍면동별_세대_및_인구.xlsx", sheetIndex=1, encoding="UTF-8")
# cjucityfor <- read.xlsx("행정시별_세대_및_인구_20191218194644.xlsx", sheetIndex=1, encoding="UTF-8")
# cjutowfor
# cjucityfor
# str(kormap3)
# kormap3
# changeCode(tbc)
# 
# 
# 
# ggChoropleth(cjutowfor,kormap3,fillvar="총인구_명",subarea=c("제주"))
# 
# # ggChoropleth(data = korpop1,
#              aes(fill = pop,
#                  map_id = code,
#                  tooltip = name),
#              map = kormap1,
#              interactive = T)
