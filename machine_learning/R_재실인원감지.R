library(lubridate)
library(MASS)
library(dplyr)
library(caret)
library(ggplot2)

train <- read.csv("https://github.com/otzslayer/KHURStudy/raw/master/2016%20Big%20Leader%204th/Data/occupancy_train.csv")
test <- read.csv("https://github.com/otzslayer/KHURStudy/raw/master/2016%20Big%20Leader%204th/Data/occupancy_test.csv")

train$date = as.POSIXct(train$date)
test$date = as.POSIXct(test$date)
train$Occupancy = as.factor(train$Occupancy)
test$Occupancy = as.factor(test$Occupancy)
date = train$date

#데이터 탐색 - 시각화
head(train)
weekdays(date)

# 온도 시각화
ggplot(train, aes(x = Occupancy, y = Temperature, color = Occupancy)) +
  geom_boxplot()

# 습도 시각화
ggplot(train, aes(x = Occupancy, y = Humidity, color = Occupancy)) +
  geom_boxplot()

# 빛 시각화
ggplot(train, aes(x = Occupancy, y = Light, color = Occupancy)) +
  geom_boxplot()

# 습도비율
ggplot(train, aes(x = Occupancy, y = HumidityRatio, color = Occupancy)) +
  geom_boxplot()

# 변수간 상관관계
train %>% 
  select(-Occupancy, -date) %>%
  cor()

# 날짜로 시각화
# 주말은 쉽니다
ggplot(train, aes(x = wday(date), fill = Occupancy)) +
  geom_bar(stat="count", position=position_dodge())

# 근무시간 6 ~ 18
ggplot(train, aes(x = hour(date), fill = Occupancy)) +
  geom_bar(stat="count", position=position_dodge())

# 주중, 주말변수 생성
train_2 = train %>%
  mutate(weekday = ifelse(wday(date) > 1 & wday(date) < 7, 1, 0)) %>%
  mutate(worktime = ifelse(hour(date) >= 7 & hour(date) <= 17, 1, 0))

test_2 = test %>%
  mutate(weekday = ifelse(wday(date) > 1 & wday(date) < 7, 1, 0)) %>%
  mutate(worktime = ifelse(hour(date) >= 7 & hour(date) <= 17, 1, 0))

# 야근여부를 정확히 측정할 수 있다면 좋을듯
ggplot(train, aes(x = factor(hour(date)), y = Light, color = hour(date))) + 
  geom_boxplot()

# 모델생성
train_2$weekday = factor(train_2$weekday)
train_2$worktime = factor(train_2$worktime)
test_2$weekday = factor(test_2$weekday)
test_2$worktime = factor(test_2$worktime)
model = glm(Occupancy ~ Temperature + Light + CO2 + HumidityRatio + weekday + worktime, data = train_2, family = "binomial")
summary(model)

# 모델평가
predict_occupancy = predict(model, test_2, type = "response")
head(predict_occupancy)
binary_predict_occupancy = ifelse(predict_occupancy > 0.5, 1, 0)

confusionMatrix(binary_predict_occupancy, test_2$Occupancy, positive = "1")

# 야근 가능성을 추가하여 모델구축
# 18시로 고정한 이유, 19, 20시의 소수값까지 처리할경우 과적합가능성
overwork = test %>%
  filter(hour(date) == 18)

# 사무실에서 사용하는 조도는 대부분 200~300 lux
# 18시는 해가 질 시간이기 때문에 외부에 의한 빛은 적을 가능성
ggplot(overwork, aes(x = minute(date), color = Occupancy)) +
  geom_bar()

ggplot(overwork, aes(x = minute(date), y = Light,color = Occupancy)) +
  geom_point()

train_3 = train_2 %>%
  mutate(nightwork = ifelse(hour(date) == 18 & Light > 400, 1, 0))

test_3 = test_2 %>%
  mutate(nightwork = ifelse(hour(date) == 18 & Light > 400, 1, 0))

head(train_3)

train_3$nightwork = factor(train_3$nightwork)
test_3$nightwork = factor(test_3$nightwork)
model_2 = glm(Occupancy ~ Temperature + Light + CO2 + HumidityRatio + weekday + worktime + nightwork, data = train_3, family = "binomial")

predict_occupancy = predict(model_2, test_3, type = "response")
head(predict_occupancy)
binary_predict_occupancy = ifelse(predict_occupancy > 0.5, 1, 0)

confusionMatrix(binary_predict_occupancy, test_3$Occupancy, positive = "1")

# temperature 와 light
ggplot(train_4, aes(x = HumidityRatio, y = Light, color = Occupancy)) +
  geom_point()

train_4 = train_2
test_4 = test_2

train_4$Light[train_4$Light < 300] = 0
test_4$Light[test_4$Light < 300] = 0

model_3 = glm(Occupancy ~ Temperature + Light + CO2 + HumidityRatio + weekday + worktime, data = train_4, family = "binomial")

predict_occupancy = predict(model_3, test_4, type = "response")
head(predict_occupancy)
binary_predict_occupancy = ifelse(predict_occupancy > 0.5, 1, 0)

confusionMatrix(binary_predict_occupancy, test_2$Occupancy, positive = "1")

#이상치 제거
par(mfrow = c(2,2))
plot(model)

leverage = hatvalues(model)
leverage_point = which(leverage > 0.05)
stdlier = studres(model)
outliers = which(stdlier < -20)

train_5 = train_2[-leverage_point, ]
train_5 = train_5[-outliers, ]

model_4 = glm(Occupancy ~ Temperature + Light + CO2 + HumidityRatio + weekday + worktime, data = train_5, family = "binomial")

# 모델평가
predict_occupancy = predict(model_4, test_2, type = "response")
head(predict_occupancy)
binary_predict_occupancy = ifelse(predict_occupancy > 0.5, 1, 0)

confusionMatrix(binary_predict_occupancy, test_2$Occupancy, positive = "1")

