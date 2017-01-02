library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)

letter <- read.csv("https://github.com/otzslayer/KHURStudy/raw/master/2016%20Big%20Leader%204th/Data/letter.csv")
head(letter)

set.seed(123)
trainIdx <- sort(sample(1:nrow(letter), size = 0.7 * nrow(letter)))
letterTrain <- letter[trainIdx, ]
letterTest <- letter[-trainIdx, ]
accuracy <- function(actual, predict){
  sum(actual == predict) / length(actual)
}

## CART 알고리즘을 통한 기본 의사결정나무모델 만들기
letterTree = rpart(letter ~ ., data = letterTrain, method = "class")
predictLetter = predict(letterTree, letterTest, type = "class")

accuracy(letterTest$letter, predictLetter) # 48.5%

rpart.plot(letterTree)

## Randomforest를 통해 모델 일반화
RF_letter = randomForest(letter ~ ., data = letterTrain, ntree = 500, inportance = TRUE,
                         replace = FALSE, proximity = TRUE)
varImpPlot(RF_letter)
pred_RF_letter = predict(RF_letter, letterTest)
accuracy(letterTest$letter, pred_RF_letter) # 96.05%


## xgboost를 통한 모델개선
trainLabel = as.numeric(letterTrain$letter) - 1
testLabel = as.numeric(letterTest$letter) - 1
trainMat = model.matrix( ~ ., data = letterTrain[ , -1])
testMat = model.matrix( ~ ., data = letterTest[ , -1])

letter_xgboost = xgboost(data = trainMat, label = trainLabel, max.depth = 9,
                         eta = 0.3, nrounds = 35, subsample = 1, num_class = 26, objective = "multi:softmax",
                         eval_metric = "merror")

xgboost_pred = predict(letter_xgboost, testMat)
accuracy(testLabel, xgboost_pred) # 95.5%


