# 강영훈 191210/191211

# 문1)R에서 제공하는 state.x77 데이터셋을 차원 축소하여 2차원 산점도와 3
# 차원 산점도를 작성하시오. (state.x77은 매트릭스 타입이기 때문에 데이터프레임
#                 으로 변환하여 실습한다.)
install.packages("Rtsne")
# 2차원 산점도
library(Rtsne)
library(ggplot2)
state <-as.data.frame(state.x77)
dup = which(duplicated(state))
dup
tsne <- Rtsne(state, dim = 2, perplexity = 10); tsne
statevis <- data.frame(tsne$Y)
ggplot(statevis, aes(x = X1, y = X2)) +
  geom_point(size = 2)
# 3차원 산점도
install.packages(c('rgl','car'))
library(car)
library(rgl)
library(mgcv)
tsne <- Rtsne(state, dims = 3, perplexity = 10)
statevis3 <- data.frame(tsne$Y)
scatter3d(x = statevis3$X1, y = statevis3$X2, z = statevis3$X3)


# 문2)R에서 제공하는 swiss 데이터셋을 차원 축소하여 2차원 산점도와 3차원산점도를 작성하시오.
# 2차원 산점도
dup = which(duplicated(swiss))
dup
tsne <- Rtsne(swiss, dim = 2, perplextity = 10)
swiss2 <- data.frame(tsne$Y)
ggplot(swiss2, aes(x = X1, y = X2)) +
  geom_point(size = 2)
# 3차원 산점도
scatter3d(x=swiss2$X1, y = swiss2$X2, z = swiss2$X3)


# 문3) R을 이용하여 지도를 출력하시오.
install.packages('ggmap')
library(ggmap)
register_google(key = 'AIzaSyCs0wxvwUGyb7GgWNhlNB4NdfnWh78c5Tg')

# (1) 서울시청을 중심으로 지도 크기는 600x600, 지도 유형은 roadmap인 지도를 출력
# 하시오.
map <- get_googlemap(center=cen)
ggmap(map)
gc <- geocode(enc2utf8("서울시청"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen,
                     zoom = 16,
                     size = c(600,600),
                     maptype = 'roadmap')
ggmap(map)

# (2) 금강산 지역을 근방으로 지도 크기는 500x500, 지도 유형은 hybrid, zoom은 8
# 인 지도를 출력하시오.
map <- get_googlemap(center=cen)
ggmap(map)
gc <- geocode(enc2utf8("금강산"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen,
                     zoom = 8,
                     size = c(500,500),
                     maptype = 'hybrid')
ggmap(map)

# (3) 강남역 근방으로 지도 크기는 640x640, 지도 유형은 roadmap, zoom은 16인 지
# 도를 출력하시오.
map <- get_googlemap(center=cen)
ggmap(map)
gc <- geocode(enc2utf8("강남역"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen,
                     zoom = 16,
                     size = c(640,6400),
                     maptype = 'roadmap')
ggmap(map)

# (4) 지도 유형은 roadmap, zoom은 9인 경도 127.397692, 위도 36.337058 지역의 지
# 도를 출력하시오.
cen <- c(127.397692, 36.337058)
map <- get_googlemap(center = cen,
                     zoom = 9,
                     maptype = 'roadmap')
ggmap(map)

# (5) 지도 유형은 roadmap, zoom은 10인 경도 135.502330, 위도 34.693594 지역의
# 지도를 출력하시오.
cen <- c(135.502330, 34.693594)
map <- get_googlemap(center = cen,
                     zoom = 10,
                     maptype = 'roadmap')
ggmap(map)


# 문4)R을 이용하여 서울시 한강 이남의 구청들의 위치에 마커와 구청 이름을
# 지도 위에 표시하시오.
names <- c("양천구청","강남구청","강동구청")
gc <- geocode(enc2utf8(names))
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     size=c(640,640),
                     marker = gc)
gmap <- ggmap(map)
gmap +
  geom_text(data = df,
            aes(x = lon, y = lat),
            size = 5,
            label = df$name)

# 문5)R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
# 역시 이름을 함께 표시하시오.
names <- c("부산광역시","인천광역시","대구광역시","대전광역시")
gc <- geocode(enc2utf8(names))
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     size=c(640,640),
                     zoom = 8,
                     marker = gc)
gmap <- ggmap(map)
gmap +
  geom_text(data = df,
            aes(x = lon, y = lat),
            size = 5,
            label = df$name)


# 문6)R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
# 시하되 국립공원의 이름을 함께 표시하시오.
names <- c("북한산국립공원","설악산국립공원","오대산국립공원","치악산국립공원","태백산국립공원")
gc <- geocode(enc2utf8(names))
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 8)
gmap <- ggmap(map)
gmap +
    geom_text(data = df,
              aes(x = lon, y = lat),
              size = 5,
              label = df$name)


# 문7) ‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
# 통사고 발생건수를 지도상에 원의 크기로 나타내시오. refer to 공공데이터포털
# csv 한글 깨짐
# Sys.setlocale("LC_ALL","C")
setwd("C:/Users/peace/Downloads")
caracc <- read.csv("Report.csv", 
                  sep = ",") #encoding = 'euc-kr'
# Sys.setlocale("LC_ALL","Korean")
names <- as.character(caracc[,"district"])
class(names)
gc <- geocode(enc2utf8(names)) # enc2utf8
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat,count=caracc$count)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 12)
ggmap(map) +
    geom_point(data = df,
               aes(x = lon, y = lat, size = count),
               alpha = 0.5, color = "navy") 
        # +scale_size_continuous(range=c(1,14))

# 문8)7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.
Sys.setlocale("LC_ALL","C")
setwd("C:/Users/peace/Downloads")
caracc <- read.csv("Reportjeju.csv", 
                   sep = ",") #encoding = 'euc-kr'
Sys.setlocale("LC_ALL","Korean")
names <- as.character(caracc[,"district"])
class(names)
gc <- geocode(enc2utf8(names)) # enc2utf8
df <- data.frame(name = names, lon = gc$lon, lat = gc$lat,count=caracc$count)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 12)
ggmap(map) +
    geom_point(data = df,
               aes(x = lon, y = lat, size = count),
               alpha = 0.5, color = "navy") 

