# day 15
# 다중선형 회귀분석(mulitple linear regression analysis)
# 회귀식
# y = B0 + B1X1 + B2X2 + B3X3 + ... +BnXn
# 독립변수가 n개인 다중선형 회귀에서 주어진 자료를 이용해 B0, B1, B2, ... , Bn의 값을 알아내는 회귀모델

library(tidyverse)
library(car)

str(Prestige)
head(Prestige)
Prestige

newdata <- Prestige[ , c(1:4)]
head(newdata)
plot(newdata, pch = 16, col = 'blue', main = 'Matrix Scatterplot')

model <- lm(income~education + prestige + women, data = newdata)
model

coef(model) #Intercept == B0
income = (-253.8497) + (177.1990*newdata$education) + (141.4354*newdata$prestige) + (-50.8957*newdata$women)
income # 아래 fitted 값과 같으나 변수 이름이 안나옴
fitted(model)
residuals(model)
deviance(model) # 편차?
deviance(model) / length(newdata$education)
summary(model)
2.2e-16 < 0.05

newdata2 <- Prestige[ , c(1:5)]; newdata2
model2 <- lm (income~., data = newdata2); model2 # 점 찍으면 변수 다 들어감
summary(model2)

library(MASS)
model3 <- stepAIC(model2)
summary(model3) # 영향력이 높은 변수만 골라 출력함 -> model2보다 영향력(Adjusted R-squared, 수정설명력)이 높다
# Mutiple R-squared(결정계수): 종속변수의 변화(변동)을 얼마나 설명하는지나타내는 지표이다(0~1).
# Adjusted R-squared(수정 결정계수): 결정계수(Multiple R-squared)와 차이가 크면 회귀모형을 재검토해야 한다.

# Linear Regression: 연속형 데이터에 대한 예측
# Logistic Regression: 범주형 데이터에 대한 예측 [결과값을 범주형태로 변환해야 함] one-hot encoding(단 하나만 TRUE)

# Logistic Regression (로지스틱 회귀분석)
# 회귀 모델에서 종속변수 값의 형태가 범주형인 경우우
# 예측 모델
# 주어진 데이터로부터 어떤 범주를 예측하는 분야를 회귀와 구분하여 분류(classfication)
# 로지스틱 회귀도 기본적으로 회귀 기법이기 때문에 종속변수는 숫자로 표현되어야 한다.
# 예) YES와 NO는 0과 1로 iris 데이터셋의 setosa, versicolor, virginaca는 1,2,3과 같이 숫자로 바꾼 후에 로지스틱 회계 적용

iris.new <- iris
iris.new$Species <- as.integer(iris.new$Species) #setosa가 1이 됨
head(iris.new)

iris_model <- glm(Species~., data = iris.new) # glm: Logistic Regression에 사용하는 함수
iris_model
coef(iris_model) # 계수를 찾아내는건 linear와 같지만, y값이 범주형이어야 한다
summary(iris_model)

unknown <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2)))
names(unknown) <- names(iris)[1:4]
unknown

pred <- predict(iris_model, unknown)
pred # 품종 1이 뭔지 모르기에 one-hot 인코딩을 해줘야 함.
pred <- round(pred, 0)
levels(iris$Species)[pred]

test <- iris[, 1:4]

pred <- predict(iris_model, test); pred
pred <- round(pred, 0)

answer <- as.integer(iris$Species); answer
pred == answer
acc <- mean(pred == answer)
acc
