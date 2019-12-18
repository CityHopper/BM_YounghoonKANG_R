# 강영훈 191218/191218

# 문1)R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# • 군집의 수는 5로 한다.
clust <- state.x77
fit <- kmeans(x = clust, center = 5)
fit
fit$cluster
fit$centers
library(cluster) # 차원 축소 후 군집 시각화 패키지

# • state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.
std <- function(x){
  return(( x - min(x)) / (max(x) - min(x))) }
clust <- apply(state.x77, 2, std)
fit <- kmeans(x = state.x77, center = 5)
fit

clusplot(clust, # 군집 대상
         fit$cluster, # 군집 번호
         color = TRUE, # 원의 색
         shade = TRUE, # 원의 빗급 표시 유무
         labels = 2, # 관측값 출력 형태
         lines = 1 ) # 중심선 연결 표시: 0으로 지정하면 중심선이 사라짐

# 문2)mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# • 군집의 수는 2로 한다.
# • Sonar 데이터셋에서 마지막에 있는 Class 열은 제외하고 군집화를 실행한다.
library( mlbench )
data( "Sonar" ) 			# 데이터셋 불러오기
mydata <- Sonar[ , -61]
mydata
fit <- kmeans(x = mydata, center = 2)
fit
clusplot(mydata, # 군집 대상
         fit$cluster, # 군집 번호
         color = TRUE, # 원의 색
         shade = TRUE, # 원의 빗급 표시 유무
         labels = 2, # 관측값 출력 형태
         lines = 1 ) # 중심선 연결 표시: 0으로 지정하면 중심선이 사라짐

# 문3) mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.

library(class)
nrow(Sonar)
odd <- seq(from = 1, to = nrow(Sonar), by =2) ;odd
even <- seq(from = 2, to = nrow(Sonar), by =2); even

tr.idx <- odd
ds.tr <- Sonar[tr.idx, 1:60]; ds.tr # 훈련용
ds.ts <- Sonar[-tr.idx, 1:60]; ds.ts # 테스트용
cl.tr <- factor(Sonar[tr.idx,61]); cl.tr # 훈련용 그룹 정보
cl.ts <- factor(Sonar[-tr.idx,61]); cl.ts # 테스트용 그룹 정보
pred3 <- knn(ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE); pred3
pred5 <- knn(ds.tr, ds.ts, cl.tr, k = 5, prob = TRUE); pred5
pred7 <- knn(ds.tr, ds.ts, cl.tr, k = 7, prob = TRUE); pred7
acc3 <- mean(pred3 == cl.ts); acc3
acc5 <- mean(pred5 == cl.ts); acc5
acc7 <- mean(pred7 == cl.ts); acc7
table(pred3, cl.ts) # 관측치와 변수가 일치하지 않은 데이터들
table(pred5, cl.ts)
table(pred7, cl.ts)

# 문4) mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
pred <- knn(ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE)
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.
# install.packages("cvTools")
library(cvTools)
k=5
folds <- cvFolds(nrow(Sonar), K = k)
folds
str(folds)
acc <- c()

for (i in 1:k){
  ts.idx <- folds$which == i
  ds.tr <- Sonar[-ts.idx, 1:60]
  ds.ts <- Sonar[ts.idx, 1:60]
  cl.tr <- factor(Sonar[-ts.idx,61])
  cl.ts <- factor(Sonar[ts.idx,61])
  pred <- knn(ds.tr, ds.ts, cl.tr, k=3)
  acc[i] <- mean(pred == cl.ts)}
acc
mean(acc)

