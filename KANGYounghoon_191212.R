# 강영훈 191211/191212

# 문1)20대 국회 개원 여·야 3당 대표 국회연설문에 대해 각각 워드클라우드를 작성하시오.
# 예제소스 파일은 ‘ex_10-1.txt’, ‘ex_10-2.txt’, ‘ex_10-3.txt’이다.
Sys.setenv ( JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_231' ) 
library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)

library(dplyr)
library(ggplot2)

setwd('D:/New-one/BM_ClassMaterial_R')
speech1 <- readLines('ex_10-1.txt', encoding = 'UTF-8')
speech2 <- readLines('ex_10-2.txt', encoding = 'UTF-8')
speech3 <- readLines('ex_10-3.txt', encoding = 'UTF-8')
buildDictionary(ext_dic = 'woorimalsam')
pal2 <- brewer.pal(9,'Blues')[5:9]
noun1 <- sapply(speech1, extractNoun, USE.NAMES = F)
noun2 <- sapply(speech2, extractNoun, USE.NAMES = F)
noun3 <- sapply(speech3, extractNoun, USE.NAMES = F)

### ex_10-1.txt
noun11 <- unlist(noun1)
wordcount <- table(noun11)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun11, y = df$Freq)) +
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

wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    #

### ex_10-2.txt
noun22 <- unlist(noun2)
wordcount <- table(noun22)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun22, y = df$Freq)) +
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

wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    #

### ex_10-3.txt
noun33 <- unlist(noun3)
wordcount <- table(noun33)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun33, y = df$Freq)) +
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

wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    #


# 문2)스티브 잡스의 스탠포드 대학 졸업식 연설문에 대해 워드클라우드를 작성하시오.
# Tip. 예제소스 파일은 ‘ex_10-4.txt’이다.
Sys.setenv ( JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_231' ) 

setwd('D:/New-one/BM_ClassMaterial_R')
speech4 <- readLines('ex_10-4.txt', encoding = 'UTF-8')
buildDictionary(ext_dic = 'woorimalsam')
pal2 <- brewer.pal(9,'Blues')[5:9]
noun <- sapply(speech4, extractNoun, USE.NAMES = F) # 명사 추출
noun4 <- unlist(noun)

wordcount <- table(noun4)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun4, y = df$Freq)) +
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

wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    # 단어 색

# 문3) 오바마 대통령의 데통령 당선 연설문에 대해 워드클라우드를 작성하시오 Tip. 예제소스 파일은 ‘ex_10-5.txt’이다.
Sys.setenv ( JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_231' )

setwd('D:/New-one/BM_ClassMaterial_R')
speech5 <- readLines('ex_10-5.txt', encoding = 'UTF-8')
buildDictionary(ext_dic = 'woorimalsam')
pal2 <- brewer.pal(9,'Blues')[5:9]
noun5 <- sapply(speech5, extractNoun, USE.NAMES = F) # 명사 추출
noun5 <- unlist(noun5)

wordcount <- table(noun5)
wordcount
sort.noun <- sort(wordcount, decreasing = T)[1:20]
sort.noun
sort.noun <- sort.noun[-1]
barplot (sort.noun, names.arg = names(sort.noun),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수')

df <- as.data.frame(sort.noun)
df
ggplot ( df, aes(x = df$noun5, y = df$Freq)) +
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

wordcloud (names(wordcount), # 단어
           freq = wordcount, # 단어 빈도
           scale = c(6, 0.7),# 단어 폰트 크기(최대, 최소)
           min.freq = 3,     # 단어 최소 빈도
           random.order = F, # 단어 출력 위치
           rot.er = .1,      # 90도 회전 단어 비율
           colors = pal2)    # 단어 색
