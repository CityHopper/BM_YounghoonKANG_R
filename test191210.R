# day 11
# https://www.rdocumentation.org/packages/vcd/versions/1.4-4/topics/mosaic
# Mosaic Plot
# 다중변수 범주형 데이터에 대한 각 변수의 그륩별 비율을 면적으로 표시

str(mtcars)
head(mtcars)
mosaicplot(~gear + vs, #대상 변수, x축 + y축
           data = mtcars, # 데이터셋
           color = TRUE, # 축 변수의 그룹별 음영 다르게 표시
           main = 'Gear and Vs') # 제목

mosaicplot(~gear + vs, data = mtcars,
           color = c('yellow','navy'),
           main = 'Gear and Vs')

tbl <- table(mtcars$gear, mtcars$vs)
tbl
mosaicplot(tbl, color = T, main = "Gear and Vs")


# 차원 축소 (dimension reduction)
# 차원 축소 기법: t-sne 기법
install.packages("Rtsne")
library(Rtsne)
library(ggplot2)
ds <- iris[,-5]
ds # 변수가 4개니까 4차원 자료

# 차원 축소 0단계: 중복 데이터 제거
dup = which(duplicated(ds))
dup
ds <- ds[-dup,]
ds.y <- iris$Species[-dup]; ds.y

# 차원 축소 수행 t-sne 실행
tsne <- Rtsne(ds, # 차원 축소 대상 데이터셋
              dim = 2, # 축소할 차원 2/3차원
              perplexity = 10) # perplexity차원 축소 과정에서 데이터 샘플링을 수행: 샘플의 갯수=(대상 데이터수 / 3) 보다 작게 지정
tsne <- Rtsne(ds, dim = 2, perplexity = 10)
tsne # 차원 축소시 약간의 데이터 손실을 감수해야함

# 차원 축소 결과 시각화
df.tens <- data.frame(tsne$Y)
head(df.tens)
ggplot (df.tens, aes(x = X1, y = X2, color = ds.y)) +
  geom_point(size = 2)


install.packages(c("rgl","car"))
library(car)
library(rgl)
library(mgcv)

tsne <- Rtsne(ds, dims = 3, perplexity = 10) #차원 축소 함수 Rtsne()
df.tsne <- data.frame(tsne$Y)
head(df.tsne)
scatter3d(x = df.tsne$X1, y = df.tsne$X2, z = df.tsne$X3)

# iris 데이터셋의 산점도 3차원 버전 [원래 산점도는 2차원]
points <- as.integer(ds.y)
color <- c('red','green','blue')
scatter3d(x = df.tsne$X1, y = df.tsne$X2, z = df.tsne$X3, 
          point.col = color[points],
          surface = FALSE)

# https://skyeong.net/186
# http://cloud.google.com/maps-platform/#get-started


# 공간 시각화
# google map 사용
# 절차
# 1. R 최신 버전 설치
# 2. ggplot2 최신 버전 설치
# 3. ggmap 패키지 설치
# 4. 구글맵을 사용하기 위한 API key 획득
# 5. 구글맵을 이용한 공간 시각화 수행
# My Google maps AIP Key: AIzaSyCs0wxvwUGyb7GgWNhlNB4NdfnWh78c5Tg
library(ggmap)
help(get_googlemap)
register_google(key = 'AIzaSyCs0wxvwUGyb7GgWNhlNB4NdfnWh78c5Tg')
gc <- geocode(enc2utf8("모투에카")) # '제주'의 위도와 경도 획득
gc
cen <- as.numeric(gc) # 경도/위도를 숫자로 변환
cen

# 지도 표시
map <- get_googlemap(center=cen) # 지도 중심점 좌표
ggmap(map)

gc <- geocode(enc2utf8("Bangkok"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen, # 지도 중심점 좌표
                     zoom = 15,    # 지도 확대 정도: 숫자가 클수록 확대 3~21
                     size = c(640,640), # 지도 크기
                     maptype = 'hybrid') # 지도 유형
ggmap(map)

# - ROADMAP(normal, default 2D map)
# - SATELLITE(photographic map)
# - HYBRID(photographic map + road and city names)
# - TERRAIN(map with mountains, rivers, etc.)

cen <- c(126.561099, 33.253077) # 경도, 위도 - 구글에서는 반대 순서로 나옴
map <- get_googlemap(center = cen,
                     zoom = 20,
                     maptype = 'roadmap')
ggmap(map)

# 지도 위 마커 표시
gc <- geocode(enc2utf8("제주"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen, # 지도 중심점 좌표
                     zoom = 15,
                     maptype = 'roadmap',
                     marker = gc) 
ggmap(map)

# 제주 관광지를 지도 위에 표시
names <- c("용두암", '성산일출봉', '정방폭포', '중문관광단지','한라산 1100고지', '차귀도')
addr <- c('제주시 용두암길 15', 
          '서귀포 성산읍 성산리',
          '서귀포시 동홍동 299-3',
          '서귀포시 중문동 2624-1',
          '서귀포시 색달동 산 1-2',
          '제주시 한경면 고산리 125')
gc <- geocode(enc2utf8(addr))
gc

# 관광지 명칭과 좌표값으로 DataFrame 생성
df <- data.frame(name = names, lon = gc$lon,
                 lat = gc$lat)
df
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center = cen,
                     maptype = 'roadmap',
                     zoom = 10,
                     size = c(640, 640),
                     marker = gc)
ggmap(map)

# 지도에 관광지 이름 추가
gmap <- ggmap(map)
gmap + 
  geom_text(data = df,
            aes(x = lon, y = lat), # 텍스트 위치
            size = 5, # 텍스트 크기
            label = df$name) # 텍스트 이름
help(geom_text)

# 지도에 데이터 표시
wind
dim(wind)
str(wind)
sp <- sample(1:nrow(wind), 50)
df <- wind[sp,]
head(df)

cen <- c(mean(df$lon), mean(df$lat))
gc <- data.frame(lon = df$lon, lat = df$lat)
head(gc)

map <- get_googlemap(center = cen,
                     maptype = "roadmap",
                     zoom = 6,
                     marker = gc)
ggmap(map)

# 지도에 풍속을 원의 크기로 표시
map <- get_googlemap(center = cen,
                     maptype = "roadmap",
                     zoom = 6)
gmap <- ggmap(map)
gmap +
  geom_point(data = df,
             aes(x = lon, y = lat, size = spd),
             alpha = 0.5, color = 'navy') +
  scale_size_continuous(range=c(1,14)) #원 크기 조절. 안될 때 이거 빼라

# 단계 구분도
install.packages("ggiraphExtra") # 단계 구분도를 위한 패키지
library(ggiraphExtra)

# 미국 주별 데이터셋
dim(USArrests)
str(USArrests)
head(USArrests)

library(tibble)

# USArrests 데이터셋에 지역명 변수가 따로 없고 행이름이 지역명으로 되어 있음
# tibble package의 rownames_to_column()을 이용해서 행 이름을 state 변수로 바꿔서 dataframe 생성

crime <- rownames_to_column(USArrests, var = 'state')
crime$state <- tolower(crime$state)
str(crime)
head(crime)

library(ggplot2)

# 단계 구분도를 만드려면 지역의 위도, 경도 정보가 있는 지도 데이터 필요.
# R에 내장된 map 패키지에 미국 주별 위/경도를 나타내는 state가 있음

install.packages('mapproj')
library(mapproj)

state_map <- map_data('state') # ggplot2의 map_data()를 이용 데이터 프레임 생성
state_map
str (state_map)

# ggiraphExtra package에 포함된 단계 구분도 작성 함수
ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = state_map)

ggChropleth(data = crime,
            aes(fill = Murder,
                map_id = state),
            map = state_map,
            interactive = T) #interactive = T로 하면 지도 위에 마우스 움직임에 반응하는 interactive 단계구분도 작성

# http://rpubs.com/cardiomoon/222145 # 국내 지도 사용하기
# 윈도우 사용자가 지도 데이타를 사용하고자 할때에는 changeCode()함수로 한글코드를 바꾸어 사용하시기 바랍니다. 단 interactive plot을 사용할 때에는 변환하시지 말고 사용하셔야 합니다.

install.packages("devtools") # R 패키지
devtools::install_github("cardiomoon/kormaps2014") # from GitHub
devtools::install_github("cardiomoon/moonBook2") # from GitHub

Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")
install.packages("extrafont")
library(extrafont)

library(kormaps2014)
library(moonBook2)
changeCode(areacode) # 
areacode

library(ggplot2)
theme_set(theme_gray(base_family="NanumGothic"))

# ggplot2를 이용한 단계구분도 그리기
ggplot(korpop1,aes(map_id=code,fill=총인구_명))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long,y=kormap1$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2015년도 시도별 인구분포도")+
  coord_map()

ggplot(korpop2,aes(map_id=code,fill=총인구_명))+
  geom_map(map=kormap2,colour="black",size=0.1)+
  expand_limits(x=kormap2$long,y=kormap1$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2015년도 시도별 인구분포도")+
  coord_map()


# ggChoropleth()함수를 이용한 단계구분도 그리기
ggChoropleth(korpop2,kormap2,fillvar="남자_명")
ggChoropleth(korpop3,kormap3,fillvar="주택_계_호")
ggChoropleth(korpop3,kormap3,fillvar="총인구_명",subarea=c("전라","광주"))

# ggChoropleth()함수를 이용한 Interactive plot그리기
ggChoropleth(korpop2,kormap2,fillvar="남자_명",interactive=TRUE)
