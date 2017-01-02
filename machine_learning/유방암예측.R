library(MASS)
library(dplyr)
library(class)
library(caret)
library(ggplot2)

train = Pima.tr
test = Pima.te

# 결측값 제거
na_del_train = train
na_del_test = test

# 데이터의 집합 분리
train_label = train[ ,8]
test_label = test[ ,8]

# 집합을 제외한 데이터셋
train = train[ ,-8]
test = test[ ,-8]

predicted_diabete = knn(train = train, test = test, cl = train_label, k = 17)
confusionMatrix(predicted_diabete, test_label, positive = "Yes")
#76.81

# 데이터 정교화를 위한 시각화
ggplot(data = na_del_train, aes(x = npreg, fill = type)) +
  geom_bar(stat = "count", position = position_dodge())

ggplot(data = na_del_train, aes(x = type, y = npreg, fill = type)) +
  geom_boxplot()

ggplot(data = na_del_train, aes(x = type, y = glu, fill = type)) +
  geom_boxplot()

ggplot(data = na_del_train, aes(x = type, y = skin, fill = type)) +
  geom_boxplot()

ggplot(data = na_del_train, aes(x = type, y = bmi, fill = type)) +
  geom_boxplot()

ggplot(data = na_del_train, aes(x = type, y = ped, fill = type)) +
  geom_boxplot()

ggplot(data = na_del_train, aes(x = type, y = age, fill = type)) +
  geom_boxplot()

### skin, bmi, ped 차이가 없어보임 // ped의 경우 소숫점이기 때문에 판단이 어려움
refine_train = train[ , -c(4,5)]
refine_test = test[ , -c(4,5)]

predicted_diabete = knn(train = refine_train, test = refine_test, cl = train_label, k = 19)
confusionMatrix(predicted_diabete, test_label, positive = "Yes")
# 78.61


### normalization 시도
normalize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

# 전체데이터로 변경
total_data = rbind(refine_train, refine_test)
total_data_2 = rbind(train, test)

# normalize 적용
normalize_total_data = as.data.frame(lapply(total_data_2, normalize))

# 트레이닝 / 테스트로 분리
normalize_train = normalize_total_data[1:200, ]
normalize_test = normalize_total_data[201:nrow(normalize_total_data), ]

predicted_diabete = knn(train = normalize_train, test = normalize_test, cl = train_label, k = 17)
confusionMatrix(predicted_diabete, test_label, positive = "Yes")
## 75.9

# z-scaling 적용
scale_total_data = as.data.frame(lapply(total_data, scale))

# 트레이닝 / 테스트로 분리
scale_train = scale_total_data[1:200, ]
scale_test = scale_total_data[201:nrow(scale_total_data), ]

predicted_diabete = knn(train = scale_train, test = scale_test, cl = train_label, k = 17)
confusionMatrix(predicted_diabete, test_label, positive = "Yes")

## K - 값을 찾아라

Result <- data.frame(k = NULL, Accuracy = NULL, Sensitivity = NULL,
                     Specificity = NULL, PosPredValue = NULL, NegPredValue = NULL)
sequence <- seq(1, 50, by = 2)
for (i in sequence) {
  predicted_diabete <- knn(train = refine_train, test = refine_test,
              cl = train_label, k = i)
  confMat <- confusionMatrix(predicted_diabete, test_label)
  currentResult <- data.frame(k = i, Accuracy = confMat$overall[1],
                              Sensitivity = confMat$byClass[1],
                              Specificity = confMat$byClass[2],
                              PosPredValue = confMat$byClass[3], 
                              NegPredValue = confMat$byClass[4])
  Result <- bind_rows(Result, currentResult)
}
Result
## k값 15로 조정
predicted_diabete = knn(train = normalize_train, test = normalize_test, cl = train_label, k = 15)
confusionMatrix(predicted_diabete, test_label, positive = "Yes")
