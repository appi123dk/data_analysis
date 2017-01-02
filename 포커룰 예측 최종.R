library(MASS)
library(dplyr)
library(randomForest)

train = read.csv("../data_analysis/train.csv")
test = read.csv("../data_analysis/test.csv")
test = test[, -1]
trainLabel = train[, ncol(train)]

train_shape = train[, c(1,3,5,7,9)]
train_number = train[, c(2,4,6,8,10)]

test_shape = test[, c(1,3,5,7,9)]
test_number = test[, c(2,4,6,8,10)]

count_func = function(x){
  return(max(table(x)))
}

length_table = function(x){
  return(length(table(x)))
}


consecutive_num = function(x){
  if (sum(x %in% c(1,13)) >= 2) {
    x = c(x, 14)
  }
  differ = diff(sort(x))
  max_count = 0
  count = 0
  for (i in 1:length(differ)) {
    if (differ[i] == 1) {
      count = count + 1
    } else {
      count = 0
    }
    if (count > max_count) {
      max_count = count
    }
  }
  return(max_count)
}


shape_count = apply(train_shape, 1, count_func)
number_count = apply(train_number, 1, count_func)
not_dup_card = apply(train_number, 1, length_table)
differ_number = apply(train_number, 1, consecutive_num)
not_dup_shape = apply(train_shape, 1, length_table)

shape_count_te = apply(test_shape, 1, count_func)
number_count_te = apply(test_number, 1, count_func)
not_dup_card_te = apply(test_number, 1, length_table)
differ_number_te = apply(test_number, 1, consecutive_num)
not_dup_shape_te = apply(test_shape, 1, length_table)

train_df = data.frame(shape_count, number_count, not_dup_card, differ_number, not_dup_shape, train_label = factor(trainLabel))
test_df = data.frame(shape_count = shape_count_te, number_count = number_count_te,
                     not_dup_card = not_dup_card_te, differ_number = differ_number_te,
                     not_dup_shape = not_dup_shape_te)

row.names(train_df) <- NULL
row.names(test_df) <- NULL

RF_poker = randomForest(train_label ~ ., data = train_df, ntree = 500, importance = TRUE,
                        replace = FALSE, proximity = TRUE)

pred_RF_poker = predict(RF_poker, test_df)

submission_df = data.frame(1:1000000, pred_RF_poker)
colnames(submission_df) = c("id", "hand")
head(submission_df)

write.csv(submission_df, file = "submission.csv", row.names=FALSE)

### xgboost
poker_trainMat <- model.matrix(~., data = train_df[, -ncol(train_df)])
poker_testMat <- model.matrix(~., data = test_df)

poker_Trainlabel<-as.numeric(train_df$train_label)-1
head(poker_trainMat)
library(xgboost)
poker_xgboost <- xgboost(data = poker_trainMat, label = poker_Trainlabel,
                         max.depth = 10, eta = 0.3, subsample = 0.7, nrounds =100,
                         objective = "multi:softmax",num_class=10,eval_metric = "merror")
summary(poker_xgboost)
xgb_pred <- predict(poker_xgboost, poker_testMat)


submission_df = data.frame(1:1000000, xgb_pred)
colnames(submission_df) = c("id", "hand")
write.csv(submission_df,"poker(xgb).csv")



