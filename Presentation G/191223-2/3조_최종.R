library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "시도별_외국인주민_현황.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 4 )
df_o <- excel_o
str( df_o )

colnames( df_o )
head( df_o)

df_o <- df_o %>% filter( 행정구역별.1. == "제주특별자치도" ) %>% 
  select( 합계.2, 합계.3, 합계.6, 합계.7, 합계.10, 합계.11, 합계.14, 합계.15 )

colnames( df_o ) <- c( "외국인주민수2015", "외국인근로자수2015", "외국인주민수2016", "외국인근로자수2016",
                       "외국인주민수2017", "외국인근로자수2017", "외국인주민수2018", "외국인근로자수2018" )
#######################
# 1.외국인주민수 현황 #
#######################
df <- df_o %>% select( 외국인주민수2015, 외국인주민수2016, 외국인주민수2017, 외국인주민수2018 ) %>% t()

df <- data.frame( 인원수 = df,
                 년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )

# 그래프 변경 시 아래 값 수정
title <- "제주 4개년(2015~2018) 외국인주민수"
xLabel <- "년도"
yLabel <- "인원수(명)"

myColor <- "#F14B69"

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = myColor, stat = "identity", width = 0.5 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

##########################
# 2. 외국인근로자수 현황 #
##########################
df <- df_o %>% select( 외국인근로자수2015, 외국인근로자수2016, 외국인근로자수2017, 외국인근로자수2018 ) %>% t()

df <- data.frame( 인원수 = df,
                 년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )

# 그래프 변경 시 아래 값 수정
title <- "제주 4개년(2015~2018) 외국인근로자수"
xLabel <- "년도"
yLabel <- "인원수(명)"

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = "#C12B49", stat = "identity", width = 0.5 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

##################################################
# 3. 외국인주민에서 외국인근로자가 차지하는 비율 #
##################################################
df <- df_o %>% select( 외국인주민수2018, 외국인근로자수2018 ) %>% t()

forgnJumin2018Cnt <- df_o$외국인주민수2018

# 원본데이터에서 주민수에 외국인근로자수가 포함되므로 pie chart 그리기 위해 주민수-근로자수로 계산한 값을 넣어줌
df[ rownames( df ) == "외국인주민수2018", 1 ] <- df[ rownames( df ) == "외국인주민수2018", 1 ] - df[ rownames( df ) == "외국인근로자수2018", 1 ]

df <- data.frame( 인원수 = round( df / forgnJumin2018Cnt * 100, 2 ),
                 구분 = c( "외국인근로자 외 외국인", "외국인근로자" ) )

title <- "2018년 제주도 외국인주민수에서 외국인근로자가 차지하는 비율"
xLabel <- "구분"

ggplot( data = df ) +
  geom_bar( mapping = aes( x = "", y = 인원수, fill = 구분 ), stat = "identity", color = "white" ) +
  coord_polar( theta = "y" ) + 
  scale_fill_manual( values = c( myColor, "gray" ) ) +
  geom_text( aes( x = "", y = 인원수 ), 
             label = ifelse( df$구분 == "외국인근로자", paste( df$인원수, "%" ), "" ), 
             position = position_stack( vjust = 0.5 ),
             size = 8, color = "white" ) +
  ggtitle( title ) + 
  theme_void()

################
# 총생산금액   #
################
excel_o <- read.xlsx( "시도별_경제활동별_지역내총생산.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o
str( df_o )

colnames( df_o )

df_o <- df_o %>% filter( 시도별 == "제주특별자치도"　& !( 경제활동별 %in% c( "순생산물세", "총부가가치(기초가격)" ) ) ) %>% 
  select( 경제활동별, X2015년.기준년가격.연쇄..1, X2015년.기준년가격.연쇄..2, X2015년.기준년가격.연쇄..3 )

colnames( df_o ) <- c( "경제활동별", "생산금액2015", "생산금액2016", "생산금액2017" )

# 지역내총생산 금액
df.grdp <- df_o[ 1, c( "생산금액2015", "생산금액2016", "생산금액2017" ) ]

# 년도별 지역총생산 내 비율 변수 추가
df_o <- df_o %>% mutate( 생산금액2015rate = round( 생산금액2015 / df.grdp$"생산금액2015" * 100, 2 ),
                        생산금액2016rate = round( 생산금액2016 / df.grdp$"생산금액2016" * 100, 2 ),
                        생산금액2017rate = round( 생산금액2017 / df.grdp$"생산금액2017" * 100, 2 ) ) %>% 
  filter( 경제활동별 != "지역내총생산(시장가격)" )

# 상위 5개 업종만
df_o <- head( df_o[ order( df_o[ , "생산금액2017rate" ], decreasing = TRUE ), ], 5 )
df <- data.frame()

for( i in 1:nrow( df_o ) ) {
  addRowData <- data.frame( 경제활동별 = df_o[ i, "경제활동별" ],
                           년도 = as.factor( 2015:2017 ),
                           생산금액 = as.numeric( df_o[ i, c( "생산금액2015", "생산금액2016", "생산금액2017" ) ] ) )
  df <- rbind( df, addRowData )
}

# 그래프 변경 시 아래 값 수정
title <- "제주 3개년(2015~2017) 총생산 상위 5개 업종"
xLabel <- "업종명"
yLabel <- "총생산금액(원)"

grayByLv <- c( "#C0C0C0", "#A0A0A0", "#808080" )

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 경제활동별, y = 생산금액, fill = 년도 ), stat = "identity", position = "dodge", width = 0.5 ) + 
  scale_fill_manual( values = grayByLv ) +
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

########################
# 읍면동별 외국인 밀집 #
########################
library( kormaps2014 )
library( moonBook2 )

excel_o <- read.xlsx( "읍면동별_세대_및_인구.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o
str( excel_o )

colnames( df_o )

# *** 년도 변경 시 select()의 변수명 변경 필요!!!
df_o <- df_o %>% select( 행정시별.2., 제주.인구.계...명., 외국인.계...명. ) %>% 
  filter( 행정시별.2. != "소계" ) %>% 
  mutate( code = "" )

colnames( df_o ) <- c( "행정구역별_읍면동", "총인구수", "외국인수", "code" )

df_o$행정구역별_읍면동 <- gsub( " ", "", df_o$행정구역별_읍면동 ) # whitespace제거

areacode <- changeCode( areacode ) # (*다시돌릴때는 history지우던가 이 부분 주석)

# changeCode()를 kormap3에 적용해야해서 myKormap3 변수 별도 생성(지도 출력시 에러 방지)
myKormap3 <- kormap3
myKormap3$name <- changeCode( myKormap3 )$name

# 제주 지역 code 별도 dataSet 생성
df_code.jeju <- myKormap3[ which( myKormap3$order == 1 & startsWith( as.character( myKormap3$code ), "390" ) ), ][ , c( "name", "code" ) ]

df_o$code <- df_code.jeju[ df_code.jeju$name == df_o$행정구역별_읍면동, "code" ]

# *** 년도 변경 시 title명 변경 필요!!!
ggChoropleth( df_o, kormap3, fillvar= "외국인수", 
              title = "2011 제주 읍면동별 외국인수 단계구분도",
              subarea = c( "제주" ) )

####################################################

# 제주도 외국인 성별 분포 ( 남/녀 막대그래프 )
#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(scales)
library(dplyr)

setwd("/Users/jeong-kyujin/Downloads")
df1 <- read.csv("시군구별_외국인주민_현황_20191218145132.csv", 
                stringsAsFactors=FALSE, fileEncoding = "CP949", encoding="UTF-8")
str(df1)
df_gen <- df1[, df1[2,] == "외국인근로자"]
df_gen <- df_gen[,df_gen[3,] != "계"]
df_gen
gen_jeju <- df_gen[c(3,21),] #제주도만
gen_jeju <- rbind(gen_jeju, names)
gen_jeju
names <- colnames(gen_jeju)
names <- substr(names, 2, 5)
gen_jeju_t <- data.frame(t(gen_jeju))
gen_jeju_t[,1] <- as.factor(gen_jeju_t[,1])
gen_jeju_t[,2] <- as.numeric(as.character(gen_jeju_t[,2]))
gen_jeju_t[,3] <- as.factor(gen_jeju_t[,3])
gen_jeju_t2 <- gen_jeju_t[-c(1,2),] # 연령이랑 맞춰서 3개년치만
gen_jeju_t2

# 맥 한글깨짐 방지 폰트 지정 #F14B69 핑크컬러
theme.tx <- element_text(family = "AppleGothic", face = "bold")

# 시각화1. 2015~2018년 외국인 근로자 성별에 따른 증감 (남75%, 여25%)
colnames(gen_jeju_t2) <- c("성별","인구수","년도")
ggplot(gen_jeju_t2, aes(x = 년도, y = 인구수, fill = 성별)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_grey() +
  theme(axis.title = theme.tx, plot.title = theme.tx,
        legend.title = theme.tx, legend.text = theme.tx)



# 제주도 외국인 연령별 분포 ( 나눈 막대 그래프)
df2 <- read.csv("시군구별_연령별__외국인_주민현황_계.csv", 
                stringsAsFactors=FALSE, fileEncoding = "CP949", encoding="UTF-8", na.strings = '*')
# 한국 국적을 가지지 않은 외국인 근로자만 추출
df_age <- df2[,(df2[1,] ==  "한국국적을 가지지 않은 자" & df2[2,] == "외국인근로자")]
df_age <- data.frame(df2[,c(1,2)], df_age)
colnames(df_age)[1:2] <- c("행정구역","연령")
age_jeju <- df_age[df_age$행정구역 == "제주특별자치도",]
age_jeju <- na.omit(age_jeju)
age_jeju
key <- colnames(age_jeju)[1:2]
age_jeju_m <- melt(age_jeju, id.vars=key)
age_jeju_m[,3] <- substr(age_jeju_m[,3], 2, 5)
age_jeju_m[,4] <- as.numeric(age_jeju_m[,4])
age_jeju_m[,2] <- as.factor(age_jeju_m[,2])
age_jeju_m[,3] <- as.factor(age_jeju_m[,3])
colnames(age_jeju_m)[3:4] <- c("년도","인구수")
age_jeju_m <- age_jeju_m[age_jeju_m$연령 != "계", -1]
age_jeju_m
age_jeju_2018 <- age_jeju_m[age_jeju_m$년도 == 2018,]
ttl2018 <- sum(age_jeju_2018$인구수)
age_jeju_2018$비율 <- age_jeju_2018$인구수 / ttl2018 * 100
age_jeju_2018

# 시각화2. 2016~2018년 외국인 근로자 연령별 인구수
ggplot(age_jeju_m, aes(x = 년도, y = 인구수, fill = 연령)) +
  geom_bar(stat="identity") +
  scale_fill_grey() +
  theme(axis.title = theme.tx, plot.title = theme.tx,
        legend.title = theme.tx, legend.text = theme.tx)


# 제주도 외국인 국적 분포 (파이그래프)
df3 <- read.csv("한국국적을_가지지_않은_자_계.csv", 
                stringsAsFactors=FALSE, fileEncoding = "CP949", encoding="UTF-8", na.strings = '*')
str(df3)
df_country <- df3[df3$행정구역.시군구.별.1. == "제주특별자치도",] # 제주도 데이터만 추출
df_country <- rbind(df3[c(1:2),], df_country)
df_country <- df_country[, c(1,105:145)] # 2018년 데이터만 추출
country_jeju <- data.frame(t(df_country))
country_jeju <- country_jeju[-1,]
colnames(country_jeju) <- c("분류","국가","인구수")
country_jeju$인구수 <- as.numeric(as.character(country_jeju$인구수))
cont_jeju <- na.omit(country_jeju[country_jeju$국가 == "소계",])
cont_jeju <- cont_jeju[-1,]
cont_jeju
ttl <- sum(cont_jeju$인구수)
cont_jeju$비율 <- cont_jeju$인구수 / ttl * 100
cont_jeju

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# 시각화3. 제주도 외국인 근로자 국적 대분류 (동북아 43.5%, 동남아 36.0% -> 전체의 79.5%)
pie <- ggplot(cont_jeju, aes(x="", y=인구수,fill=분류)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar(theta="y")
pie + scale_fill_grey() + blank_theme +
  theme(axis.title.x = element_blank(),
        legend.title = theme.tx, legend.text = theme.tx)

subcnt <- c("동북아시아", "동남아시아")
asia_jeju <- country_jeju[country_jeju$분류 %in% subcnt,]
asia_jeju <- asia_jeju[asia_jeju$국가 != "소계",]
country <- asia_jeju[order(asia_jeju$인구수, decreasing=TRUE),]
country <- country[-1]
country$국가 <- as.character(country$국가)
row <- c("그외",sum(country$인구수[6:15]))
country2 <- rbind(country[1:5,], row)
country2$인구수 <- as.numeric(country2$인구수)
cttl <- sum(country2$인구수)
country2$비율 <- country2$인구수 / cttl * 100
country2

# 시각화4. 제주도 외국인 근로자 국적 소분류 (중국 36%, 베트남 17%, 한국계 중국인 14%, 태국 12%, 인도네시아 8%)
pie2 <- ggplot(country2, aes(x="", y=인구수, fill=국가)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar(theta="y")
pie2  + scale_fill_grey() + blank_theme +
  theme(axis.title.x = element_blank(),
        legend.title = theme.tx, legend.text = theme.tx)

###########################################

install.packages('readxl')

#행정지별 세데 인구수
setwd("d:/")
dm <- read_excel("행정시별_세대_및_인구_20191221183516.xlsx")

library(readxl)
#농총 가구 

a <- read_excel("행정구역_시군구_별_농가__농가인구_20191221174659.xlsx")


#가구 

a1 <- a[1:6,-1]

a1_1 <- a1[,2]

colnames(a1_1) <- c("a")

n <- c(2013,2014,2015,2016,2017,2018)

#농촌 인구수 

a2 <- a[7:12,-1]

a2_1 <- a2[,2]
View(dm)
a2_1 <- a2_1/dm$`제주 인구(계) (명)`
a3 <- cbind(n, a1_1 , a2_1) 
colnames(a2_1) <- c("c")

View(a3)

str(a3)

library(ggplot2)



#농촌 인원 

ggplot(data = a3) +geom_line(data = a3 ,aes(n, c), color ="red",size = 0.8)
#가구 수
ggplot(a3, aes(n,a))+geom_bar(stat="identity")


##################################3



#어업 가구 

f <- read_excel("시도별_어가__어가인구__어업종사가구원_20191221175153.xlsx")

#가구 

f1 <- f[1:6,-1]

f1_1 <- f1[,2]

colnames(f1_1) <- c("a")

#어업  인구수

f2 <- f[7:12,-1]

f2_1 <- f2[,2]
f2_1 <- f2_1 / dm$`제주 인구(계) (명)`
f3 <- cbind(n, f1_1 , f2_1) 

colnames(f2_1) <- c("c")

library(ggplot2)

#어업 가구 수

ggplot(f3, aes(n,a))+geom_bar(stat="identity") 

#어업 인원 

ggplot(data = f3) +geom_line(data = f3 ,aes(n, c), color ="red",size = 0.8)

#####################################



#임업 가구 

b <- read_excel("시도별_임가수_및_임가인구_20191221175401.xlsx")

#가구 

b1 <- b[1:6,-1]

b1_1 <- b1[,2]

colnames(b1_1) <- c("a")

#임업  인구수

b2 <- b[7:12,-1]

b2_1 <- b2[,2]
b2_1 <- b2_1 /dm$`제주 인구(계) (명)`
b3 <- cbind(n, b1_1 , b2_1) 

colnames(b2_1) <- c("c")
b3
library(ggplot2)

#임업 가구 수

ggplot(b3, aes(n,a))+geom_bar(stat="identity") 

#임업 인원 

ggplot(data = b3) +geom_line(data = b3 ,aes(n, c), color ="red",size = 0.8)

str(b3)  

View(b3)





#-----------------------------------------------------------

#고령화 비율 (12~15)



o <- read_excel("고령인구비율_시도_시_군_구__20191221204942.xlsx")

str(o)

o_1<- as.numeric(o$시점)

o_1 <- cbind(o_1, o$제주특별자치도)

colnames(o_1) <- c("year","n")
o_1 <- o_1[-1,]
o_1 <- data.frame(o_1)

#농촌 비율 -> 농가 인구 65세이상 / 농가 인구 *100


h <- read_excel("연령별_농가인구_20191222142737.xlsx")
h <- h[-1,]
h0 <- unlist(h[,3])
h0 <- as.numeric(h0)
h_1 <- read_excel("농가인구_시도_시_군_구__2012.xlsx")
h_1 <- c(25688,26070,23950,26744,27275)
h2 <- (h_1/h0) *100
h3 <- cbind(o_1, h2)
h3 <- data.frame(h3)
str(h3)
#고령화 비율 -> 65세이상인구  / 제주도 인구수  *100

colnames(h3) <- c("old" , "m")
h4 <- cbind(o_1, h3)
h4
ggplot(data = h4) +
  
  geom_line(data = h4 ,aes(year, n), color ="red",size = 0.8) 

ggplot(data= h3 ) +
  
  geom_line(data = h3, aes(year, h2), color ="blue" ,size = 0.8)
#------------------

######################################

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


