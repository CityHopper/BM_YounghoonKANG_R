# 14일차 p.491
# 데이터 모델링: 현실 세계를 표현하는 수식[y = wx + b, 통계적 관점]을 도출하는 과정
# x(독립변수) | y(반응변수) | w(weight), b(bias, 경향, 매개변수) - 찾아야 하는 값
# 모델 과정
# 1. 모델 선택: 수식 결정 - y = wx+b
# 2. 실제 데이터(x, y)를 이용하여 w, b 값(상수)을 결정(훈련 과정)
# 3. 실제 데이터를 통한 예측
# 4. 모델 평가

# 단순선형 회귀분석(Simple Linear Regression Analysis) | y = wx + b
# Modeling: 현실 세계에서 일어나는 현상을 수학식으로 표현하는 행위
# p.341-365, 371-485, 489-636
# 데이터 과학에서 독립변수 x를 설면변수(explanatory variable), 특징(feature)
# 종속변수 y를 반응변수(response variable), 레이블(label)
# x가 입력되면 y를 맞혀야 하는 문제, y를 ground truth로 간주
# 데이터 과학에서 modeling이란 수집한 데이터(훈련 데이터)를 이용하여 최적의 모델을 찾아내는 과정

# 최적의 모델을 찾는 과정
# 모델: y = wx + b

# 1. 모델 선택 -> 선형 방정식 선택
# 2. 주어진 데이터(훈련 데이터)를 적용하여 매개변수 결정 - lm() [linear model]
# 3. 예측 훈련 데이터에 없는 새로운 데이터로 모델이 레이블을 추정하는 과정 - predict()
# 4. 완성된 모델에 대한 품질 평가 - summaryt() : 결과이해

# 회귀분석(Regression Analysis)
# 관찰된 연속형 변수들에 대해 두 변수 사이의 모형을 구한뒤 적합도를 측정해 내는 분석 방법
# 시간에 따라 변화하는 데이터나 어떤 영향, 가설적 실험, 인과관계의 모델링 등의 통계적 예측에 이용될 수 있다.

# 단순선형 회귀분석(Simple Linear Regression Analysis) | y = wx + b
# 독립변수와 종속변수의 관계가 선형으로 표현
# 하나의 독립변수를 다루는 분석방법

# 단순선형 회귀모델의 회귀식 : y = wx + b(w,b는 상수)
# w, b는 어떻게 찾을 수 있을까? x,y로 구성된 데이터를 이용하여 w,b를 찾아내는 모델

# 주행거리와 제동거리 사이의 회귀모델
str(cars)
head(cars)

# 산점도를 통한 선형관계 확인
plot(dist~speed, data = cars)
plot(cars)

# 회귀모델 구하기
# 종속(반응) 변수~독립 (설명) 변수 순서로 지정
model <- lm(dist~speed, cars)
model

# Call:
#   lm(formula = dist ~ speed, data = cars)
# 
# Coefficients:
#   (Intercept)        speed  
# -17.579        3.932  
# b값             w값
# y = 3.932*x - 17.579
abline(model)
coef(model) # 매개변수(계수) - w, b값 출력
cars

fitted(model) # 훈련 데이터에 있는 샘플에 대한 예측값 | 회귀식의 y값
residuals(model) # 잔차: 회귀식으로 추정된 값과의 차이(오차) | 다 제곱해서 더하면 deviance
# 잔차 제곱합을 평균제곱오차(MES-mean squared error)로 변환
deviance(model) / length(cars$speed)

b <- coef(model)[1]
w <- coef(model)[2]

speed <- 21.5
dist <- w * speed + b
dist

df <- data.frame(speed = c(21.5, 25.0,25.5,26.0,26.5,27.0,27.5,28.0))
predict(model, df) # 예측 수행 함수
plot(df$speed, predict(model, df), col = 'red',
     cex = 2, pch = 20)
abline(model)

speed <- cars[, 1]
pred <- w * speed + b
pred
compare <- data.frame(pred, cars [,2],
                      pred-cars[,2])
colnames(compare) <- c("예상","실제","오차")
compare
head(fitted(model),3) # 예측
head(residuals(model), 3) # 추정된 값과의 차이
head(compare, 3)

summary(model)
# 평균은 클수록, 분산은 작을수록 데이터 크기가 클수록 신뢰가 커진다. -> t-통계량(t-statistics)/t-값(t-value)
# t-값이 크면 대립가설에 대한 신뢰가 높아짐
# t-값이 작으면 대립가설에 대한 신뢰 낮아짐
# 데이터를 통해 '대립가설이 통계학 적으로 유의미하다'라는 것을 증명하고 확인하는 작업을 t-검정(t-test)라 한다.
# '귀무가설이 참이라고 가정했을때, 표본으로부터 얻어지는 통계치가 나타날(관측될) 확률'을 계산하는데 이 때 계산된 확률 값을 p값이라 한다.
# p값이 매우 낮으면, 이러한 표본 통계값은 우연히 나타나기 어려운 케이스이기 때문에, 우리는 귀무가설을 채택하지 않고(기각하고), 대안적인 가설인 가설, 즉 대립가설을 채택한다.
# 


# Call:
#   lm(formula = dist ~ speed, data = cars)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -29.069  -9.525  -2.272   9.215  43.201 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#   speed         3.9324     0.4155   9.464 1.49e-12 *** | *이 많이 나올수록 스피드가 제동거리에 영향을 많이 미친다는 뜻
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

str(cars)
head(cars)
car_model <- lm(dist~speed, data = cars); car_model
coef(car_model)
plot(car_model); 
abline(car_model, col = 'red')
summary(car_model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  #################  0.05를 기준으로 낮으면 회귀선 기준으로 모여있다는 뜻. 회귀선으로부터 퍼져있는 값 | women보다 값이 큼 -> 오차날 가능성 큼
#   speed         3.9324     0.4155   9.464 1.49e-12 *** ## P값
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

str(women)
head(women)
women_model <- lm(weight~height, data = women); women_model
coef(women_model)
plot(women_model); 
abline(women_model, col = 'red')
summary(women_model)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -87.51667    5.93694  -14.74 1.71e-09 *** ################# 회귀선으로부터 퍼져있는 값
#   height        3.45000    0.09114   37.85 1.09e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.525 on 13 degrees of freedom
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.9903 
# F-statistic:  1433 on 1 and 13 DF,  p-value: 1.091e-14

# Residuals: 잔차를 의미합니다. 회귀식에 의해 추정된 값과 실제값(입력값)의 차이 입니다.
# 
# 
# 
# Min        1Q    Median        3Q        Max
# 
# -7.2552   -4.6554   -0.7552    3.8965   12.3392
# 
# 
# 차례로 잔차의 최소값 1사분위수, 중앙값, 3사분위수, 최대값을 의미합니다.
# 
# 1사분위수는 크기 순으로 자료(데이터)를 나열 했을 때 25%에 해당하는 값, 3사분위수는 75%에 해당하는 값 입니다.
# 
# 
# 
# 
# 
# Coefficients: 추정된 회귀식의 계수를 의미 합니다.
# 
# 
# 
# Estimate      Std. Error   t value       Pr(>|t|)   
# 
# 추정된계수 / 표준오차 / t 값(점수) / p-value 를 의미 합니다.
# 
# (Intercept)  -9.1503       2.9394      -3.113        0.0067 **
#   
#   
#   Intercept는 절편을 의미 합니다.
# (y절편) 회귀식에서 계수의 유의성을 판단하기 위해 t분포를 이용합니다.
# 
# 
# t분포는 -3.113 입니다.
# 이 값에 대한 p-value는 0.0067로 alpha=0.05하에 귀무가설을 기각하게 됩니다.
# 
# 이는 곳 추정된 회귀식의 절편은 유의함을 의미 합니다.
# 
# 
# days          2.6006     0.2716   9.577    5e-08 ***
#   
#   추정된 회귀계수 day는 p-value의 수치상 유의합니다.
# 
# ---
#   
#   
#   
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# 
# Residual standard error: 5.977 on 16 degrees of freedom
# 
# 잔차의 표준오차를 의미합니다. 자유도는 16, 관측값에서 -1을 한 값이 자유도가 됩니다.
# 
# 
# Multiple R-squared:  0.8515,   Adjusted R-squared:  0.8422
# 
# R-squared는 결정계수 Adjusted R-squared는 수정된 결정계수를 의미 합니다.
# 
# 두 수치 모두 1에 가까울 수록 회귀계수의 설명력이 높음을 의미 합니다.
# 
# 결정계수는 독립변수가 많아질 수 록 증가하는 특징을 가지고 있습니다.
# 
# 이에 수정된 결정계수를 중심으로 회귀모형의 설명력을 측정 합니다.
# 
# 단 이 모형은 독립변수가 하나 이므로 큰 의미는 없습니다.
# 
# 
# F-statistic: 91.71 on 1 and 16 DF,  p-value: 4.997e-08
# 
# F통계량은 모형 전체의 유의성을 판단하기 위한 통계량 입니다.
# 
# p-vlaue를 보시면 됩니다. alpha=0.05하에 모형은 유의합니다.
# 
# 
# > coef(model1)
# 
# (Intercept)        days
# 
# -9.150327    2.600619
# 
# coef는 모형의 계수를 출력하는 함수 입니다.
# 
# 절편은 -9.150327 독립변수 days의 계수는 2.600619 입니다.
# 
# 식으로 적으면
# 
# y = -9.150327 + 2.600619 * x_1
# 
# 단, x_1 : days 입니다.
# 
# 
# 
# t값, F값 모두 통계량이며
# 
# t분포, F분포에 기초해서 나온 수치 입니다.
# 
# t값은 각 독립변수의 유의성을 판단하기 위한 통계량
# 
# F값은 모형의 유의성을 판단하기 위한 통계량 입니다.
# 
# p값(p-value)은 분포에서 통계량이 확률적으로 봤을 때 어떤 값을 가지는지 '통계량을 확률로 환산한 수치' 입니다.
