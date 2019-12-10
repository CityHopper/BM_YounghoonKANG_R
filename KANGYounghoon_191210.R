# 강영훈 191209/191210

#문1)R에서 제공하는 mtcars 데이터셋에서 gear(기어의 수)에 대해 ggplot으로
# 막대그래프를 작성하시오. 단, 제목과 x축 레이블은 ‘기어의 수’, y축 레이블
# 은 ‘빈도수’로 나타내시오.
install.packages("tidyverse")
library(tidyverse)

ggplot(mtcars, aes(x = gear)) +
  geom_bar( width = 0.7,
           fill = 'blue') +
  ggtitle("기어의 수") +
  labs(x = '기어의 수', y = '빈도수')

#문2)R에서 제공하는 mtcars 데이터셋에서 cyl(실린더의 수)에 대해 막대 색이
# 초록색인 막대그래프를 ggplot으로 작성하시오.
ggplot(mtcars, aes(x = cyl)) +
  geom_bar(width = 0.7,
           fill = 'green')

#문3) R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 구간 간격이 5.0
# 인 히스토그램을 ggplot으로 작성하시오.
ggplot(mtcars, aes(x=mpg))+
  geom_histogram(binwidth = 5.0)

#문4)R에서 제공하는 trees 데이터셋의 Girth(나무 둘레)에 대해 ggplot으로
# 히스토그램을 작성하시오. 여기에서는 히스토그램의 제목, x축 레이블, y축
# 레이블을 한글로 표시하시오. (구간 간격은 3.0, 막대의 색은 steelblue로 한다.)
ggplot(trees, aes(x=Girth))+
  geom_histogram(binwidth = 3.0, fill='steelblue')+
  ggtitle("나무 둘레")+
  labs(x='나무둘레', y='빈도수')

#문5)R에서 제공하는 mtcars 데이터셋에서 mpg(연비)를 x축으로 하고, wt(중량)를 
# y축으로 하는 산점도를 ggplot으로 작성하시오. (단, 점의 색은 gear의
# 수에 따라 다르게 표시한다.)
ggplot(mtcars, aes(x = mpg, y=wt))+
  geom_point(aes(color = gear))

# 문6)R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 ggplot으로 상자그림을 
# 작성하되, cyl(실린더 수)에 따라 그룹을 나누어 작성하시오.
ggplot(mtcars, aes(x = cyl, y = mpg, 
                   fill = as.factor(mtcars$cyl))) +
  geom_boxplot()

scale_fill_manual() # 색상 일일이 지정

# 문7) 다음은 2015년부터 2026년도까지의 예상 인구수 추계 자료이다. 연도를
# x축으로 하여 ggplot으로 선그래프를 작성하시오.

# 연도		총인구 (천명)		연도		총인구 (천명)
# 2015		51014				2021		52123
# 2016		51245				2022		52261
# 2017		51446				2023		52388
# 2018		51635				2024		52504
# 2019		51811				2025		52609
# 2020		51973				2026		52704	

year <- 2015:2026
population <- c(51014,51245,51446,51635,51811,51973,52123,52261,52388,52504,52609,52704)
df <- data.frame(year, population)
ggplot(df,aes(x=year, y=population))+
  geom_line()

# 문8)다음과 같이 데이터셋 us를 생성한 후 물음에 답하시오. 여기서 state.x77
# 은 미국 50개 주의 통계정보가, state.division은 미국 50개 주의 지역 구분
# (예: 북부, 중부, 남부……) 정보가 저장된 데이터셋이다.
us <- data.frame(state.x77, state.division);us

# (1) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 Population(인구수), 타일의 색은 Income(소득)으로 나타내고,
# 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할 수 있
# 는 것이 무엇인지 설명하시오
install.packages("treemap")
library(treemap)
us$states<-rownames(us)
head(us)
treemap(us,
        index = c('state.division', 'states'),
        vSize = 'Population',
        vColor= 'Income',
        type = 'value')
# 캘리포니아가 인구도 많고 돈도 많다


# (2) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 HS.Grad(고등학교 졸업률), 타일의 색은 Murder(범죄률)로 나타
# 내고, 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할
# 수 있는 것이 무엇인지 설명하시오.
treemap(us,
        index = c('state.division', 'states' ),
        vSize = 'HS.Grad',
        vColor= 'Murder',
        type='value')
# 전체적으로 고등학교 졸업률이 고르며, 고등학교 졸업률이 낮을 수록 범죄율이 높다

# (3) us 데이터셋에 대해 x축은 Income(소득), y축은 Illiteracy(문맹률), 원의 크기는
# Population(인구수), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버
# 블차트를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하
# 시오.
symbols(us$Income, us$Illiteracy,
       circles = us$Population,
       inches = 0.5,
       bg = 'green')
text(us$Income, us$Illiteracy,
     rownames(us),
     cex = 0.5,
     col = 'red')
# Alaska의 수입이 인구 대비 매우 크다.

# (4) us 데이터셋에 대해 x축은 Illiteracy(문맹률), y축은 Murder(범죄률), 원의 크기
# 는 Area(면적), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버블차트
# 를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하시오.
symbols(us$Illiteracy, us$Murder,
        circles = us$Area,
        inches = 0.5,
        bg = 'green')
text(us$Illiteracy, us$Murder,
     us$states,
     cex = 0.5,
     col = 'navy')
# 전체적으로 문맹률이 높을 수록 범죄율도 높아진다.
