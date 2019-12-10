#
# 11일차
#
# mosic plot
#     다중변수 범주형 데이터에 대한 각 변수의 
#     그룹별 비율을 면적으로 표시
str( mtcars )
head( mtcars )
mosaicplot( ~gear + vs, # 대상 변수
            data = mtcars, # 데이터셋
            color = TRUE, # y축 변수의 그룹별 음영다르게표시
            main = "Gear and Vs" ) # 제목

mosaicplot( ~gear + vs, data = mtcars,
            color = c( "green", "blue" ),
            main = "Gear and Vs" )

tbl <- table( mtcars$gear, mtcars$vs )
tbl
mosaicplot( tbl, color = T, main = "Gear and Vs" )

#
# 차원 축소( dimension reduction )
#
install.packages( "Rtsne" )

library( Rtsne )
library( ggplot2 )

ds <- iris[ , -5 ]

# 차원 축소
# 중복 데이터 제거
dup = which( duplicated( ds ) )
dup
ds <- ds[ -dup, ]
ds.y <- iris$Species[ -dup ]

# 차원 축소 수행 - t-SNE 실행
tsne <- Rtsne( ds, # 차원 축소 대상 데이터셋
               dim = 2, # 축소할 차원 2 / 3차원
               perplexity = 10 ) # 차원 축소 과정에서 
                                 # 데이터 샘플링을 수행하는데
                                 # 샘플의 갯수
                                 # (대상데이터수) / 3 보다 작게지정
tsne <- Rtsne( ds, dim = 2, perplexity = 10 )
tsne

# 차원축소 결과 시각화
df.tens <- data.frame( tsne$Y )
head( df.tens )

ggplot( df.tens, aes( x = X1, y = X2, color = ds.y ) ) +
  geom_point( size = 2 )


install.packages( c( "rgl", "car" ) )
library( car )
library( rgl )
library( mgcv )

tsne <- Rtsne( ds, dims = 3, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )
head( df.tsne )

scatter3d( x = df.tsne$X1, y = df.tsne$X2, 
           z = df.tsne$X3 )


points <- as.integer( ds.y )
color <- c( 'red', 'green', 'blue' )
scatter3d( x = df.tsne$X1, y = df.tsne$X2,
           z = df.tsne$X3,
           point.col = color[ points ],
           surface = FALSE )

#
# 공간 시각화
#
# google map 사용
#
# 절차
# 1. R 최신버전 설치
# 2. ggplot2 최신버전 설치
# 3. ggmap 설치
# 4. 구글맵을 사용하기 위한 API key 획득
# 5. 구글맵을 이용한 공간 시각화 수행 
library( ggmap )
register_google( key = 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g' )

gc <- geocode( enc2utf8( "제주" ) ) # 원하는 지점 경도/위도 획득
gc

cen <- as.numeric( gc ) # 경도/위도를 숫자로 변환
cen

# 지도 표시
map <- get_googlemap( center = cen ) # 지도 중심점 좌표
ggmap( map )


gc <- geocode( enc2utf8( "한라산" ) )
cen <- as.numeric( gc )
map <- get_googlemap( center = cen, # 지도 중심점 좌표
                      zoom = 10,    # 지도 확대 정도
                      size = c( 640, 640 ), # 지도 크기
                      maptype = "roadmap" ) # 지도 유형
ggmap( map )

cen <- c( 126.561099, 33.253077 )
map <- get_googlemap( center = cen, 
                      zoom = 15, 
                      maptype = "roadmap" )
ggmap( map )

# 지도위 마커 표시
gc <- geocode( enc2utf8( "제주" ) )
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      marker = gc )
ggmap( map )

# 제주 관광지를 지도위에 표시
names <- c( "용두암", "성산일출봉", "정방폭포",
            "중문관광단지", "한라산1100고지", "차귀도" )
addr <- c( "제주시 용두암길 15",
           "서귀포시 성산읍 성산리",
           "서귀포시 동홍동 299-3",
           "서귀포시 중문동 2624-1",
           "서귀포시 색달동 산1-2",
           "제주시 한경면 고산리 125" )
gc <- geocode( enc2utf8( addr ) )
gc

# 관광지 명칭과 좌표값으로 Data Frame 생성
df <- data.frame( name = names, lon = gc$lon, 
                  lat = gc$lat )
df

cen <- c( mean( df$lon ), mean( df$lat ) )

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 10,
                      size = c( 640, 640 ),
                      marker = gc )
ggmap( map )

# 지도에 관광지 이름 추가
gmap <- ggmap( map )
gmap +
  geom_text( data = df, # 데이터셋
             aes( x = lon, y = lat ), # 텍스트 위치
             size = 5, # 텍스트 크기
             label = df$name ) # 텍스트 이름

# 지도에 데이터 표시
dim( wind )
str( wind )

sp <- sample( 1:nrow( wind ), 50 )
df <- wind[ sp, ]
head( df )

cen <- c( mean( df$lon ), mean( df$lat ) )
gc <- data.frame( lon = df$lon, lat = df$lat )
head( gc )

# 지도에 마커 표시
map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 6,
                      marker = gc )
ggmap( map )

# 지도에 풍속을 원의 크기로 표시
map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 6 )
gmap <- ggmap( map )
gmap +
  geom_point( data = df,
              aes( x = lon, y = lat, size = spd ),
              alpha = 0.5, col = "blue" ) +
  scale_size_continuous( range = c( 1, 14 ) ) #원크기조절

#
# 단계 구분도
#

#***** 필요시 설치 **
install.packages( "ggiraphExtra" ) # 단계구분도를 위한 Package
#******************** 

library( ggiraphExtra )

# 미국 주별 data set
dim( USArrests )
str( USArrests )
head( USArrests )

library( tibble )

# USArrests data set에 지역명 변수가 따로 없고, 대신 행이름이
# 지역명으로 되어있음
# tibble package의 rownames_to_column()를 이용해서 행 이름을
# state 변수로 바꿔서 data frame 생성
crime <- rownames_to_column( USArrests, var = "state" )
crime$state <- tolower( crime$state ) # 소문자로 수정
str( crime )

library( ggplot2 )

# 단계구분도를 만들려면 지역의 위도,경도 정보가 있는 지도 data 필요
# R에 내장된 map package에 미국 주별 위/경도를 나타내는 state가 있음

#***** 필요시 설치 **
#install.packages( "mapproj" )
#******************** 
#library( mapproj )

state_map <- map_data( "state" ) # ggplot2의 map_data()를 이용 data frame 생성 
str( state_map )

# ggiraphExtra package에 포함된 단계구분도 작성 함수
ggChoropleth( data = crime,
              aes( fill = Murder, 
                   map_id = state ),
              map = state_map,
              interactive = F )

ggChoropleth( data = crime,
              aes( fill = Murder, 
                   map_id = state ),
              map = state_map,
              interactive = T ) # interactive를 T로 하면 지도위에
                                # 마우스 움직임에 반응하는 
                                # interactive 단계구분도 작성성

#
# 대한민국 지도기반 단계구분도
#
names( kormap1 )
Encoding( names( kormap1 ) ) <- "UTF-8"
names( kormap1 )

names( korpop1 )
Encoding( names( korpop1 ) ) <- "UTF-8"
names( korpop1 )

names( kormap2 )
Encoding( names( kormap2 ) ) <- "UTF-8"
names( kormap2 )

names( korpop2 )
Encoding( names( korpop2 ) ) <- "UTF-8"
names( korpop2 )

names( kormap3 )
Encoding( names( kormap3 ) ) <- "UTF-8"
names( kormap3 )

names( korpop3 )
Encoding( names( korpop3 ) ) <- "UTF-8"
names( korpop3 )

#***** 필요시 설치 **
install.packages( "stringi" )
install.packages( "devtools")
#******************** 

#***** 필요시 설치 **
devtools::install_github( "cardiomoon/kormaps2014" )
devtools::install_github( "cardiomoon/moonBook2" )
#******************** 

library( ggplot2 )
library( dplyr )
library( stringi )

library( ggiraphExtra )
library( kormaps2014 )
library( moonBook2 )


str( changeCode( areacode ) ) # kormaps2014 package의 changeCode()는
str( changeCode( kormap1 ) )  # encoding을 cp949로 변환
str( changeCode( korpop1 ) )

#theme_set( theme_gray( base_family = "NanumGothic" ) )

ggplot( korpop1, aes( map_id = code, fill = 총인구_명 ) ) +
  geom_map( map = kormap1, colour = "black", size = 0.1 )+
  expand_limits( x = kormap1$long, y = kormap2$lat ) +
  scale_fill_gradientn( colours = c( 'white', 'orange', 'red' ) ) +
  ggtitle( "2015년도 시도별 인구분포도" ) +
  coord_map()   

ggChoropleth( data = changeCode( df ),
              aes( map_id = code, 
                   fill = 총인구_명 ),
              map = kormap1 )

df <- korpop1
str( changeCode( df ) )
df <- rename( df, pop = 총인구_명, name = 행정구역별_읍면동 )
str( changeCode( df ) )
df
changeCode( df )


ggChoropleth( data = changeCode( df ),
              aes( map_id = code, 
                   fill = pop,
                   tooltip = name ),
              map = kormap1,
              interactive = T )








