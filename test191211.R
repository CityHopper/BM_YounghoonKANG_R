# data mining: 의사 결정을 위해서 DB(정형화 data)로 부터 규칙과 패턴을 발견하는 기법
# text mining: text data(자연어, 비정형 data)로 부터 규칙과 패턴을 발견하는 기법

# 워드 클라우드(Word Cloud) : 자료 처리 과정과 자료 분석 과정 - 단어 빈도수 체크
# 한글 워드 클라우드 절차
# 1. Java 실행환경 구축 (jdk 설치)
# 2. 자료 수집 (text 자료)
    # 2.1 text file 형태로 수집
    # 2.2 web scraping을 이용하여 수집
# 3. 명사 추출
Sys.setenv ( JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_231' ) 

# 필요시 설치
# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("KoNLP")           # 한국어 처리
# install.packages("RColorBrewer")    # 색상 선택

library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)

library(dplyr)
library(ggplot2)

setwd('D:\New-one\BM_YounghoonKANG_R')
text <- readLines('mis_document.txt', encoding = 'UTF-8')
text
# txt 파일 맨마지막 줄에 공백 한 줄을 넣는 것이 R과 호환에 좋다.

# '우리말씀' 한글 사전 로딩 ( 62만 개의 우리말 단어 수록)
buildDictionary(ext_dic = 'woorimalsam')
# pal2 <- brewer.pal(8,'Dark2') # 색상 팔레트 생성
pal2 <- brewer.pal(9,'Blues')[5:9]
noun <- sapply(text, extractNoun, USE.NAMES = F) # 명사 추출
noun # class: list

# 4. 추출된 단어(주로 명사) 에 대한 빈도수 계산 및 시각화
noun2 <- unlist(noun) # list -> vector로 변환
noun2
wordcount <- table(noun2)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun2, y = df$Freq)) +
    geom_bar(stat = 'identity',
             width = 0.7,
             fill = 'steelblue') +
    ggtitle('빈도수 높은 단어') +
    theme(plot.title = element_text(size = 25,
                                    face = 'bold',
                                    colour = 'steelblue',
                                    hjust = 0,
                                    vjust = 1 )) +
    labs(x = '명사', y='단어빈도수') +
    geom_text(aes(label = df$Freq), hjust = -0.3) + # 빈도 표시
    coord_flip()
        
# 5. word cloud 작성
wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    # 단어 색

# 6. 전처리 과정 수행
#   6.1 생략된 단어를 사전에 등재
buildDictionary(ext_dic = 'woorimalsam',
                user_dic = data.frame('정치', 'ncn'), # 사전에 없는 정치라는 단어를 명사(ncn)로 추가 
                replace_usr_dic = T)
noun <- sapply(text, extractionNoun, USE.NAMES = F)
noun2 <- unlist(noun)

# 6.2 불필요한 단어 삭제: 일일이 찾아봐라
noun2 <- noun2[nchar(noun2) > 1] # 글자수 1 초과
noun2 <- gsub('하지', '', noun2) # '하지'를 ''로 바꿈
noun2 <- gsub('때문', '', noun2)
wordcount <- table(noun2)

# 7. 다시 워드클라우드 실행하면 등장하는 단어가 바뀜
wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    # 단어 색


# 애국가 형태소 분석
library(KoNLP)
useSystemDic() # 28만 단어 수록 - 아무 사전이나 쓰면 됨
useSejongDic() # 37만 단어 수록
useNIADic()    # 98만 단어 수록

# 애국가 가사
# https://mois.go.kr/frt/sub/a06/b08/nationalIcon_3/screen.do

# 1. 사전 설정
useSejongDic()

# 2. 텍스트 데이터 가져오기
setwd('D:/New-one/BM_ClassMaterial_R')
word_data <- readLines('애국가(가사).txt')
word_data

# 3. 명사 추출
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
word_data2 # list type | "남산 위에", "닳도", "록" 등 사전에 없는 단어가 일부 추출됨
#   3.1 제대로 추출되지 않은 단어를 사용자 사전에 등록
add_words <- c('백두산','남산','철값','가을','하늘','달')
buildDictionary(user_dic = data.frame(add_words,
                                      rep('ncn', length(add_words))),
                replace_usr_dic = T)
get_dictionary('user_dic')
#   3.2 단어 추가 후 다시 명사 추출
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
word_data2 # list type | "남산 위에"가 "남산", "위"로 바뀜

# 4. 행렬을 벡터로 전환
undata <- unlist(word_data2)
undata

# 5. 사용 빈도 확인
word_table <- table(undata)
word_table

# 6. 필터링: 두 글자 이상 단어만 선별, 공백이나 한 자리 문자를 걸러냄
undata2 <- undata[nchar(undata) >= 2]
undata2
word_table2 <- table(undata2)
word_table2

# 7. 데이터 정렬
sort(word_table2, decreasing = T)
# 애국가 형태 분석 완료. 가장 기본적인 전처리만 수행, 100% 정확한 데이터라 볼 수 없음.

#8. word cloud 작성후 분석
library(wordcloud2)
wordcloud2(word_table2)
#   8.1 배경 및 색상 변경
wordcloud2(word_table2,
           color = 'random-light',
           backgroundColor = 'purple')
#   8.2 모양 변경
wordcloud2(word_table2,
           fontFamily = '맑은고딕',
           size = 1.2, color = 'random-light',
           backgroundColor = 'black',
           shape = 'triangle') # pentagon, triangle, triangle-forwad, circle, cardioid, star, diamond, ...
#   8.3 선택 색상 반복
wordcloud2(word_table2, size = 1.6,
           color = rep_len(c('red', 'blue'),
                           nrow(word_table2)))
wordcloud2(demoFreq, size = 1.6,
           color = rep_len(c('crimson', 'navy'),
                           nrow(word_table2)),
           shape = 'circle')
help(wordcloud2)
#   8.4 일정 방향(각도) 정렬
wordcloud2(word_table2,
           minRotation = -pi / 6,
           maxRotation = -pi / 6,
           rotateRatio = 1)
wordcloud2(demoFreq,
           minRotation = -pi / 6,
           maxRotation = -pi / 15,
           rotateRatio = 0.3)


