
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
cjutown2 <- cjutownfor


st <- data.frame(state.x77)
st <- data.frame(st, stname = rownames(st))
st
treemap(cjutownfor, index = c("행정시별.2."),
        vSize = "Area",
        vColor = 'Income',
        type = 'value',
        title = '미국 주별 수입')



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
