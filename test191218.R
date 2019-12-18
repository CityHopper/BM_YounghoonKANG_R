# day 16
# 군집화(Clustering) / 분류(Classfication)

# 군집화(Clustering)
# 지도 학습(Supervised Learning) 모델: 학습 데이터에 답이 포함(독립변수[예측/분류] / 종속변수)
# - Linear Regression
# - Logistic Regression
# - KNN(K-Nearest Neighbor) 분류 알고리즘
# - Random Forest
# - Decision treemap
# - SVM(Support Vector Machine)

# 비지도 학습(Unsupervised Learning) 모델(군집화): 학습 데이터에 답이 미포함(독립변수)
# - K-means (K-평균 군집화알고리즘)
# - 딥러닝
# - Neural Network

# 군집화(Clustering): 데이터를 유사한 것끼리 묶는 것 (군집, 범주, 그룹) [주어진 데이터에 그룹 정보가 없다]
# K-means (K-평균 군집화알고리즘)
mydata <- iris[, 1:4]
fit <- kmeans(x = mydata, center = 3) # 3개의 군집으로 나눔
fit

# K-means clustering with 3 clusters of sizes 62, 38, 50 [3개 군집에 속한 데이터 개수]
# 
# Cluster means: [3개 군집의 중심점 좌표]
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.901613    2.748387     4.393548    1.433871
# 2     6.850000    3.073684     5.742105    2.071053
# 3     5.006000    3.428000     1.462000    0.246000
# 
# Clustering vector: [각 데이터에 대한 군집 번호]
#   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 1
# [55] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 1 2
# [109] 2 2 2 2 2 1 1 2 2 2 2 1 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 2 2 1 2 2 1
# 
# Within cluster sum of squares by cluster:
#   [1] 39.82097 23.87947 15.15100
# (between_SS / total_SS =  88.4 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
# [8] "iter"         "ifault"  

fit$cluster
fit$centers

library(cluster) # 차원 축소 후 군집 시각화 패키지

clusplot(mydata, # 군집 대상
         fit$cluster, # 군집 번호
         color = TRUE, # 원의 색
         shade = TRUE, # 원의 빗급 표시 유무
         labels = 2, # 관측값 출력 형태
         lines = 1 ) # 중심선 연결 표시: 0으로 지정하면 중심선이 사라짐
subset(mydata, fit$cluster == 2)

# 대상 데이터 표준화 후 군집화
# : 데이터와 데이터의 거리를 계산할 때 발생하는 문제. 모든 변수가 거리 계산에 동등한 영향을 갖도록 하기 위해서 모든 변수의 자료 범위를 0~1 사이로 표준화한 후 거리 계산을 한다.
# ( x - min(A)) / (max(A)) - min(A))
# x : 변수 A의 임의의 관측값
# max(A), min(A)는 변수 A 관측값 중 최대/최소값

std <- function(x){
  return(( x -min(x)) / (max(x) - min(x))) }
mydata <- apply(iris [, 1:4], 2, std)
fit <- kmeans(x = mydata, center = 3)
fit

# KNN (K-Nearest Neighbor, K-최근접 이웃) 분류 알고리즘
library(class)
# 훈련용/테스트용 데이터 준비
tr.idx <- c(1:25, 51:75, 101:125)
ds.tr <- iris[tr.idx, 1:4]; ds.tr # 훈련용
ds.ts <- iris[-tr.idx, 1:4]; ds.ts # 테스트용
cl.tr <- factor(iris[tr.idx,5]); cl.tr # 훈련용 그룹 정보
cl.ts <- factor(iris[-tr.idx,5]); cl.ts # 테스트용 그룹 정보
pred <- knn(ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE)
pred
acc <- mean(pred == cl.ts)
acc 
table(pred, cl.ts) # 관측치와 변수가 일치하지 않은 데이터들이 나옴 = 93%

# 교차 검증 방법(K-fold cross validation): 실제 데이터에서도 잘 작동하게 하기 위해 훈련을 여러번 함 (overfitting, 과적합 방지)
install.packages("cvTools")
library(cvTools)
iris
k = 10
folds <- cvFolds(nrow(iris), K = k)
folds
str(folds)
acc <- c()
for (i in 1:k){
  ts.idx <- folds$which == i
  ds.tr <- iris[-ts.idx, 1:4]
  ds.ts <- iris[ts.idx, 1:4]
  cl.tr <- factor(iris[-ts.idx, 5])
  cl.ts <- factor(iris[ts.idx, 5])
  pred <- knn(ds.tr, ds.ts, cl.tr, k=5)
  acc[i] <- mean(pred == cl.ts)}
acc
mean(acc)
