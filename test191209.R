# day 10
# https://www.tidyverse.org/packages/
# https://ggplot2.tidyverse.org
install.packages("tidyverse")
library(tidyverse)
mpg
dim(mpg)
str(mpg)
head(mpg)
View(mpg)
ggplot(data = mpg) + # ggplot 함수와 실제 그래프 그리는 함수가 +로 연결되어야 함
  geom_point(mapping=aes(x = displ, y=hwy)) 

month <- c(1,2,3,4,5,6)
rain <- c(55,50,45,50, 60, 70)
df <- data.frame(month, rain)
df

# 세로 막대차트
ggplot(df, aes(x = month, y=rain)) + # aes 함수는 ggplot에 써도, geom_bar에 써도 상관 없음
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "steelblue")

# 가로 막대차트
ggplot(df, aes(x = month, y = rain)) +
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "steelblue") +
  ggtitle ("월별 강수량") +
  theme(plot.title = element_text(size = 25,
                                face = "bold",
                                color = 'red')) +
  labs(x = '월', y = '강수량') + 
  coord_flip() #가로-세로 flip

# histogram
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram(binwidth = 1.0)

ggplot(iris, aes(x = Sepal.Width, fill = Species, 
                 color = Species)) + # fill은 막대 채우는 색, color는 경계선
  geom_histogram(binwidth = 0.1, position = "dodge") + # dodge : 막대를 이어 붙이기 
  theme(legend.position = 'bottom') # 범례

##ggplot2 Scatter chart
ggplot ( data = iris, mapping = aes (x = Petal.Length,
                                     y = Petal.Width)) +
  geom_point()
                 

ggplot (data = iris) + # mapping 입력 위치 변경, 결과는 위와 같음
  geom_point( mapping = aes (x = Petal.Length,
                             y = Petal.Width))

ggplot ( data = iris, mapping = aes (x = Petal.Length,
                                     y = Petal.Width,
                                     color = Species,
                                     shape = Species)) +
  geom_point(size = 3) +
  ggtitle("꽃잎의 길이와 폭") +
  theme(plot.title = element_text (size = 25,
                                  face = 'bold',
                                   color = rgb(128,0,128,maxColorValue = 255)))

# ggplot Box plot
ggplot (data = iris, mapping = aes(y=Petal.Length)) +
  geom_boxplot(fill = 'navy',
               color = 'red')

ggplot (data = iris, mapping = aes(y=Petal.Length,
                                   fill = Species)) +
  geom_boxplot()

# ggplot Line chart
year <- 1937:1960
cnt <- as.vector(airmiles)
df <- data.frame(year, cnt)
head(df)
ggplot(df, aes(x = year, y = cnt)) +
  geom_line(col = "navy")

# ggplot 작성 graph 꾸미기 (공통)
str(economics)

# 사선
ggplot(economics, aes(x=date, y= psavert)) +
  geom_line() + 
  geom_abline(intercept = 12.18671, # intercept : y 절편값
              slope = -0.0005333) # slope : 기울기

# 평행선
ggplot(economics, aes(x = date, y= psavert)) +
  geom_line() +
  geom_hline(yintercept = mean(economics$psavert))

# 수직선 - 회귀선을 그릴 때 등 도움
x_inter <- filter (economics, #방법 1
                   psavert == min(economics$psavert ))$date
ggplot(economics, aes(x = date, y=psavert)) + #방법2
  geom_line() +
  geom_vline(xintercept = x_inter)

# 텍스트 추가
ggplot(airquality, aes(x = Day, y = Temp)) +
  geom_point() +
  geom_text(aes(label = Temp,
                vjust = 1, #수직정렬 제어
                hjust = -1)) #수평정렬 제어

# 영역 지정 및 화살표 표시
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  annotate('rect',
           xmin = 3, #좌표 지정
           xmax = 4,
           ymin = 12,
           ymax = 21,
           alpha = 0.5, #투명도
           fill = 'red') +
  annotate('segment', x= 2.5, xend = 3.7, # 화살표 넣어 강조하기
           y = 10, yend=17, color = 'blue',
           arrow = arrow(120) ) + #화살표 각도
  annotate('text', x= 2.5, y = 10, #화살표 이름
           label = 'point')

# http://rpubs.com/brandonkopp/creating-a-treemap-in-r
# treemap
install.packages('treemap')
library(treemap)
GNI2014
data(GNI2014)
dim(GNI2014)
str(GNI2014)
head(GNI2014)
View(GNI2014)
treemap(GNI2014,
        index = c('continent', 'iso3'), # 계층 구조
        vSize = 'population', # 타일 크기
        vColor= 'GNI', # 타일 컬러
        type = 'value', # 타일컬러링방법
        bg.labels = 'yellow', # 레이블배경색
        title = "World's GNI") # 제목
        
st <- data.frame(state.x77)
st <- data.frame(st, stname = rownames(st))
treemap(st, index = c("stname"),
        vSize = "Area",
        vColor = 'Income',
        type = 'value',
        title = '미국 주별 수입')

# 산점도에 bubble 추가 (bubble chart) - 산점도인데 데이터 크기만큼 버블이 찍힘
symbols(st$Illiteracy, st$Murder, # 원의 x,y 좌표
        circles = st$Population, # 원의 반지름
        inches = 0.5, # 원크기 조절값
        fg = 'white', # 원 테두리 색
        bg = 'navy', # 원 바탕색
        lwd = 1.5, # 원 테두리선 두께
        xlab = 'rate of Illiteracy', 
        ylab = 'crime(murder) rate',
        main = 'Illiteracy and Crime')
text (st$Illiteracy, st$Murder, #텍스트 출력 x,y 좌표
      rownames(st), # 출력할 text
      cex = 0.6, #폰트 크기
      col = "red") #폰트 컬러

# r-graph-gallery.com/index.html - GetCode - bubble (heatmap 도 많이 씀)
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp
