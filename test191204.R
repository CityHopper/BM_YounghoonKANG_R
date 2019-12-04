# day 7
# 결측치 처리
############ vector의 결측치 처리
z<-c(1,2,3,NA,5,NA,8)
sum(z)
is.na(z) # NA값 확인
sum(is.na(z)) #NA값 갯수
sum(z, na.rm = TRUE) #NA값 제거 후 합
summary(z)

#결측치 대체 및 제거
z1 <- z
z2 <- c(5,8,1,NA,3,NA,7)
z1[is.na(z1)] <- 0 #단순 대입법, 0으로 대체
z1
z3 <- as.vector(na.omit(z2)) #Listwise Deletion, NA 삭제
z3

############# Matrix / Data.Frame 결측치 처리
x <- iris
x[1,2] <- NA
x[1,3] <- NA
x[2,3] <- NA
x[3,4] <- NA
head(x)

#Matrix / Data.Frame 열별 결측치확인 방법
#for문
for (i in 1:ncol(x)){
  this.na <- is.na(x[, i])
  cat(colnames(x)[i],
    "\t", sum(this.na),
    "\n")
}
#apply() #이게 낫다
col_na <- function(y){
  return (sum(is.na(y)))
}
na_count <- apply(x,2,col_na) #행:1 / 열:2
na_count
na_count <- apply(x, 2, function(y) sum(is.na(y))) #익명 함수 사용 similar to lambda() from Python
na_count
barplot(na_count[na_count > 0]) #결측치가 있는 대상만 막대그래프
install.packages("VIM")
require(VIM) #라이브러리와 같은 역할

#결측치 자료 조합 확인용 시각화 도구 #####################################################################
aggr(x, prop = FALSE, numbers = TRUE)
#두 개의 변수간의 결측치 관계 확인 시각화 도구
marginplot ( x[c("Sepal.Width", "Petal.Length")],
            pch = 20,
            col = c("darkgray","red","blue"))
head(x[, c("Sepal.Width","Petal.Length")])
x
#Matrix/DataFrame의 행(data)별 결측치 확인
rowSums(is.na(x))
sum(rowSum(is.na(x))>0)
sum(is.na(x))

#결측치를 제외한 새로운 데이터셋 생성
head( x )
x[ !complete.cases(x), ] #NA가 포함 된 행 출력: complete가 아닌!
y <- x[ complete.cases(x), ] #NA가 없는 행 출력: complete
head( y )

#특이값, 이상치
st <- data.frame(state.x77)
summary(st$Income)
boxplot(st$Income)
boxplot.stats(st$Income)$out

#특이값, 이상치 처리: NA로 변환 후 결측치 처리방법 이용
head(st)
out.val <- boxplot.stats(st$Income)$out; out.val
st$Income[st$Income %in% out.val] <- NA # %in% : 벡터 내 특정값 포함 여부 확인 연산자
head(st)
newdata <- st[complete.cases(st),] #Alaska 행 날아감
head(newdata)
#이상치를 먼저 찾고, 이상치를 결측치를 바꾼 뒤, 그 결측치 처리해라

#데이터 가공
#데이터 정렬
#vector 정렬
v1 <- c(1,7,6,8,4,2,3)
order(v1)
v1 <- sort(v1)
v1
v2 <- sort(v1, decreasing = T)
v2


#Matrix.Dataframe 정렬
head(iris) #행번호 = 입력순서
order(iris$Sepal.Length)
iris[order(iris$Sepal.Length), ]#Ascending
iris[order(iris$Sepal.Length, decreasing = T),] #Descending
iris.new <- iris[order(iris$Sepal.Length),]
head(iris.new)
iris[order(iris$Species, decreasing = T,
           iris$Sepal.Length),] #정렬 기준을 2개 설정 - 품종 먼저. 그다음 길이

#데이터 분리
sp <- split(iris, iris$Species)
sp
summary(sp)
sp$setosa
summary(sp$setosa) #변수에 대한 요약 정보

#데이터 선택 subset을 통한 추출
subset(iris, Species == 'setosa')
subset(iris, Sepal.Length > 7.5)
subset(iris, Sepal.Length > 5.1 & Sepal.Width > 3.9)
subset(iris, Sepal.Length > 7.6,
       select = c(Petal.Length, Petal.Width))

#데이터 샘플링: 숫자를 임의로 추출
#비복원 추출: 뽑은 후 버림
#복원 추출: 뽑은 후 사용할 데이터에 포함

#비복원 추출 - r에서 주로 사용
x <- 1:100; x
y <- sample(x, size = 10, replace = FALSE); y

#행을 임의로 추출
idx <- sample (1:nrow(iris), size = 50, replace = FALSE); idx
iris.50 <- iris[idx, ]
dim(iris.50)
head(iris.50)

set.seed(100) #매번 동일한 출력 결과
sample(1:20, size = 5)
set.seed(100)
sample(1:20, size = 5, replace = FALSE) # replace=FALSE 매번 다른 출력 결과
set.seed(100)
sample(1:20, size = 5)

#데이터 조합
combn(1:5, 3) #1부터 5사이에 3개 수를 뽑아 - 각 변수 가 하나의 조합을 매트릭스로

x = c("red", "green", "blue", "black", "white", "navy")
com <- combn (x,3) #6가지 색 중 3개를 뽑기 (순서 무상관): 6*5*4/3! = 20가지
com

for (i in 1:ncol(com)){
  cat(com [ ,i], "\n")
}

#데이터 집계
agg <- aggregate (iris[ , -5], #5번열 제외하고 품종별로 평균값 집계
                  by = list(iris$Species),
                  FUN = mean)
agg
agg <- aggregate (iris[ , -5], #5번열 제외하고 품종별로 평균값 집계
                  by = list(iris$Species),
                  FUN = sd)
agg

head(mtcars)
agg <- aggregate(mtcars,
                 by = list(cyl = mtcars$cyl, #cyl= vs= 이름 안붙이면 Group1, Group2로 출력됨
                           vs = mtcars$vs),
                 FUN = max)
agg

#데이터 병합 merge
x <- data.frame(name = c("a","b","c"), #x와 y 모두 name이란 변수가 있음
                mat = c(90, 80, 40))
y <- data.frame(name = c("a","b","d"),
                korean = c(75,60,90))
z <- merge(x,y, by = c("name")); z #양쪽에 공통적으로 있는 데이터만 병합. a, b만 대상. c, d,는 제외 ["Left Join"]

merge(x,y) # 교집합만
merge(x,y, all.x = T) # x 기준
merge(x,y, all.y = T) # y 기준
merge(x,y, all = T) # 모두 병합

x <- data.frame(name = c("a","b","c"), #x와 y 모두 name이란 변수가 있음
                mat = c(90, 80, 40))
y <- data.frame(sname = c("a","b","d"),
                korean = c(75,60,90))
merge(x,y, by.x = c("name"),
      by.y = c("sname"))

# dplyr package 데이터 가공시 가장 많이 사용되는 패키지
install.packages("dplyr")
library(dplyr)

# %>% : 파이프 연산자 (왼쪽 ctrl + 왼쪽 shift + m) magrittr 패키지 코드

df <- data.frame (var1 = c(1,2,1),
                  var2 = c(2,3,2))
df

#rename 이름변경
df <- rename(df, v1 = var1, v2 = var2)
df
df <- edit(df)
df
#파생변수 추가
df$sum <- df$v1 + df$v2
df

df[2,1] <- 5
df

df<- data.frame(id = c(1,2,3,4,5,6),
                class = c(1,1,1,1,2,2),
                math = c(50,60,45,30,25,50),
                english = c(98,97,86,98,80,89),
                science = c(50,60,78,58,65,98))
df

#filter() 행 추출
df %>% filter (class ==1) # '입력' -파이프 %>%- '출력' 
df %>% filter (class ==2)
df %>% filter (class !=1)
df %>% filter (id !=2)

df %>% filter (class == 1 & math >= 50)
df %>% filter (math >= 50 | english >=90)
df %>% filter (class %in% c(1,3,4))

class1 <- df %>%  filter (class ==1)
class2 <- df %>%  filter (class ==2)
class1
class2

#select() : 변수 추출
df %>%  select(math)
df %>%  select(class, math, science)
df %>%  select(-math)

# dplyr 함수 조합
df %>% 
  filter(class == 1) %>% 
  select(science)
df %>% 
  select(id, science) %>% 
  head
df %>% 
  select(id, science) %>% 
  max

#arrange() 정렬
df %>% arrange(science)
df %>% arrange(desc(science)) %>% 
  select(science)

#mutate() 파생변수 추가
df %>% 
  mutate (total = math + english + science) %>% 
  head
df %>% 
  mutate(total = math + english + science,
         average = (math + english + science)/3 ) %>% 
  head
df %>% 
  mutate(grade = ifelse(science >= 60, 'pass','fail')) %>% 
  head
df %>% 
  mutate(total = math + english + science,
         average = (math + english + science)/3 ) %>%
  mutate(grade= ifelse(average >= 90, 'pass',
                       ifelse(average < 60, 'fail',
                              'normal'))) %>% 
  head
df.sort <- df %>% 
  mutate(total = math + english + science,
         average = (math + english + science)/3 ) %>%
  arrange(desc(average)) ; df.sort

#summarise(): 집단별 요약
#group_by(): 집단별 나누기
df %>% summarise(mean_math = mean(math))
df %>% 
  group_by (class) %>% 
  summarise(mean_math = mean(math),
            mean_english = mean(english),
            mean_science= mean(science),
            n = n())
install.packages("ggplot2")
str(ggplot2::mpg) # :: 콜론 두 개를 스코프 scope라고 함 - library 함수로 패키지 호출 없이 데이터셋 사용
ggplot2::mpg
mpg <- data.frame(ggplot2::mpg)
dim(mpg)
str(mpg)
head(mpg)
View(mpg)

mpg %>% 
  group_by (manufacturer,drv) %>% 
  summarise(mean_cty = mean(cty)) %>%
  head(10)


mpg %>% 
  group_by (manufacturer) %>% 
  filter(class == 'suv') %>% 
  mutate(tot = (cty + hwy) / 2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

# 데이터 합치기
# left_join(): 가로로 합치기(변수 추가)
# inner_join(): 가로로 합치기기(변수 추가)
# full_join(): 가로로 합치기(변수 추가)
# bind_rows(): 세로로 합치기(변수 추가)
df1 <- data.frame(id = c(1,2,3,4,5),
                  midterm = c(60,80,70,90,85))
df2 <- data.frame(id = c(1,2,3,4,5),
                  final = c(65,85,75,95,80))
total <- left_join (df1, df2, by = "id"); total
df1 <- data.frame(id = c(1,2,3),
                  address = c("서울","부산",'제주'),
                  stringsFactors = F); df1 # 팩터 타입으로 바꾸지 마라
df2 <- data.frame(id = c(1,2,4),
                  gender = c('남','여','남')) ;df2
df_left <- left_join(df1, df2, by='id'); df_left #left 기준
df_inner <- inner_join(df1, df2, by='id'); df_inner #교집합
df_full <- full_join(df1, df2, by='id'); df_full #합집합

df1 <- data.frame(id = c(1,2,3,4,5),
                  test = c(60,80,70,90,85))
df2 <- data.frame(id = c(1,2,3,4,5),
                  test = c(65,85,75,95,80))
df_all <- bind_rows(df1, df2)
df_all

install.packages("psych") #psych 패키지 안에 describe 사용 위함. summary와 비슷
library(psych)
summary(mtcars)
describe(mtcars)
help(describe)

install.packages("descr")
require("descr")
df <- data.frame(id = c(1,2,4),
                  gender = c('남','여','남')) ;df
table(df$gender)
freq(df$gender) #막대그래프 자동 출력
freq(df$gender, plot = F) #막대그래프 미출력




