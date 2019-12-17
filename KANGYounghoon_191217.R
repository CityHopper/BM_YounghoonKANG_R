# 강영훈 191217/191217

# 문1)trees 데이터셋에 대해 다음의 문제를 해결하는 R 코드를 작성하시오.
# (1) 나무 둘레(Girth)와 나무의 키(Height)로 나무의 볼륨을 예측하는 다중선형 회귀 모델을 만드시오.
library(tidyverse)
trees1 <-trees; trees1
model1 <- lm(Volume~Girth + Height, data = trees)
model1

# (2) 다중선형 회귀모델을 이용하여 trees 데이터셋의 나무 둘레(Girth)와 나무의 키 (Height)로 나무의 볼륨을 예측하시오.
coef(model1)
Volume = (-57.9876589) + (4.7081605*trees1$Girth) + (0.3392512*trees1$Height)
Volume # 아래 fitted 값과 같으나 변수 이름이 안나옴
fitted(model1)

# (3) (2)에서 예측한 볼륨과 실제 trees 데이터셋의 볼륨(Volume)이 얼마나 차이가 나는지 보이시오. (예측값, 실제값, 예측값-실제값을 나타낸다.)
residuals(model1) # 잔차: 회귀식으로 추정된 값과의 차이(오차)
trees['Volume'] - fitted(model1)
df <- data.frame(trees$Volume, fitted(model1), (trees['Volume'] - fitted(model1)))
names(df) <- c('실제값','예측값','실제값-예측값')
df

# 문2)mtcars 데이터셋에서 다른 변수들을 이용하여 연비(mpg)를 예측하는 다중 회귀모델을 만드시오.
mtcars1 <- mtcars
model2 <- lm(mpg~., data = mtcars1)
model2
summary(model2)

# (1) 전체 변수를 이용하여 연비(mpg)를 예측하는 회귀모델을 만들고 회귀식을 나타내시오.
coef(model2)
mpg = 12.30337416 + (-0.11144048*mtcars1$cyl) + (0.01333524*mtcars1$disp) + (-0.02148212*mtcars1$drat) + (-3.71530393*mtcars1$wt) + (0.82104075*mtcars1$qsec) + (0.31776281*mtcars1$vs) + (2.52022689*mtcars1$am) + (0.65541302*mtcars1$gear) +(-0.19941925*mtcars$carb) 
mpg

# (2) 연비(mpg)를 예측하는 데 도움이 되는 변수들만 사용하여 예측하는 회귀모델을 만들고 회귀식을 나타내시오.
model3 <- stepAIC(model2)
summary(model3)
mpg2 = 12.30337416 + (-3.71530393*mtcars1$wt) + (0.82104075*mtcars1$qsec) + (2.52022689*mtcars1$am)
mpg2

# (3) (1), (2)에서 만든 예측모델의 설명력(Adjusted R-squared)을 비교하시오.
# model1의 Adjusted R-squared: 0.8066 
# model2의 Adjusted R-squared: 0.8336 


# 문3) UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.
mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )
head(mydata)

# (1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
ucla_model <- glm(admit~., data = mydata) # glm: Logistic Regression에 사용하는 함수
ucla_model
coef(ucla_model)

summary(ucla_model)

# (2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여 
# 합격 여부를 예측하고 실제값과 예측값을 나타내시오.
head(mydata)
str(mydata)
mydata2 <- mydata[, c(2:4)]
mydata2
pred <- predict(ucla_model, mydata2)
pred <- round(pred, 0)
pred

mydata$admit # 실제값

# (3) 만들어진 모델의 예측 정확도를 나타내시오.
mydata$admit == pred
mean(pred == mydata$admit)
