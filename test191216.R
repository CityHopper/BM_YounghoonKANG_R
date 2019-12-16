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