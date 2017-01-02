library(rmdformats)
library(ggplot2)
library(MASS)
library(dplyr)
library(caret)
library(ISLR)
library(e1071)
data(Auto)
head(Auto)
dim(Auto)
Auto <- Auto %>%
  mutate(mileage = factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0)))
set.seed(123)
split <- createDataPartition(y = Auto$mileage, p = 0.7, list = FALSE)
train <- Auto[split, ]
test <- Auto[-split, ]
train <- train[-c(1, 9)]
test <- test[-c(1, 9)]

# 데이터 가공
train_label = train[ ,8]
test_label = test[, 8]
train$origin = factor(train$origin)
test$origin = factor(test$origin)


## 데이터 시각화
ggplot(Auto, aes(x = mileage, y = cylinders, fill = mileage)) + 
  geom_boxplot()

ggplot(Auto, aes(x = mileage, y = year, fill = mileage)) + 
  geom_boxplot()

ggplot(Auto, aes(x = origin, fill = mileage)) + 
  geom_bar(stat = "count", position = "dodge")


# 서포트 벡터 머신 '리니어' 모델링 
linear.tune = tune.svm(mileage ~ ., data = train, kernel = "linear", cost = c(0.001, 0.01, 0.02, 0.03, 0.05 ,0.1))
summary(linear.tune)

best.linear = linear.tune$best.model
linear.test = predict(best.linear, test)
confusionMatrix(linear.test, test_label)

#파라미터 : cost = 0.01 / accuracy = 94.83

# 서포트 벡터 머신
poly.tune = tune.svm(mileage ~ ., data = train, kernel = "polynomial", 
                     cost = c(0.001, 0.005, 0.01, 0.1),
                     degree = c(2,3,4),
                     coef0 = c(0.5, 1, 2, 2.5, 3),
                     gamma = c(2,3,4,5))

best.poly = poly.tune$best.model
summary(best.poly)
poly.test = predict(best.poly, test)
confusionMatrix(poly.test, test_label)

# 파라미터 : cost = 0.001, degree = 3, gamma = 3, coef.0 = 2
# accuracy = 95.69


# radial
radial.tune = tune.svm(mileage ~ ., data = train, kernel = 'radial',
                       gamma = c(0.1, 0.5, 0.7, 1, 1.1 ,1.5),
                       cost = c(0.001, 0.01, 0.1, 1, 1.1, 1.5))

best.radial = radial.tune$best.model
summary(best.radial)
radial.test = predict(best.radial, test)
confusionMatrix(radial.test, test_label)

# 파라미터 : cost : 1, gamma : 0.5
# accuracy : 95.69

# sigmoid
sig.tune <- tune.svm(mileage ~., data = train, kernel = "sigmoid", 
                     gamma = c(0.01, 0.05, 0.1, 0.5, 1),
                     coef0 = c(0.01, 0.1, 0.5, 0.7, 1), 
                     cost = c(0.001, 0.01, 0.1, 0.5, 1))

best.sig = sig.tune$best.model
summary(best.sig)
sig.test = predict(best.sig, test)
confusionMatrix(sig.test, test_label)

# 파라미터 : cost : 0.1, gamma : 0.1, coef.0 : 0.01
# accuracy : 94.83

# min-max scaling
normalize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

total = bind_rows(train, test)
total_n = data.frame(apply(total[,-c(7,8)], 2, normalize))
total_n$origin = total$origin
total_n$mileage = total$mileage
train_n = total_n[1:276, ]
test_n = total_n[277:nrow(total_n), ]


## 리니어
linear.tune = tune.svm(mileage ~ ., data = train_n, kernel = "linear", cost = c(0.001, 0.01, 0.02, 0.03, 0.05 ,0.1))
summary(linear.tune)

best.linear = linear.tune$best.model
linear.test = predict(best.linear, test_n)
confusionMatrix(linear.test, test_label)

# cost : 0.01 
# accuracy : 94.83


## poly
poly.tune = tune.svm(mileage ~ ., data = train_n, kernel = "polynomial", 
                     cost = c(0.001, 0.005, 0.01, 0.1),
                     degree = c(2,3,4),
                     coef0 = c(0.5, 1, 2, 2.4, 2.5, 3),
                     gamma = c(1, 2, 3))

best.poly = poly.tune$best.model
summary(best.poly)
poly.test = predict(best.poly, test_n)
confusionMatrix(poly.test, test_label)

# 파라미터 : cost = 0.001, degree = 3, gamma = 3, coef.0 = 2
# accuracy : 95.69

## radial
radial.tune = tune.svm(mileage ~ ., data = train_n, kernel = 'radial',
                       gamma = c(0.1, 0.5, 0.7, 1, 1.1 ,1.5),
                       cost = c(0.001, 0.01, 0.1, 1, 1.1, 1.5))

best.radial = radial.tune$best.model
summary(best.radial)
radial.test = predict(best.radial, test_n)
confusionMatrix(radial.test, test_label)

# 파라미터 : cost = 1, gamma = 1
# accuracy : 95.69

radial.tune <- tune.svm(mileage ~ . , data=train, kernel="radial", 
                        gamma=c(0.01,0.1,1,5,10),
                        cost=c(0.001,0.01,0.1,1,5))

best.radial = radial.tune$best.model
summary(best.radial)
radial.test = predict(best.radial, test)
confusionMatrix(radial.test, test_label)
