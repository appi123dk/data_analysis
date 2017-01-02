library(MASS)
library(dplyr)
library(randomForest)

train = read.csv("../data_analysis/train.csv")
test = read.csv("../data_analysis/test.csv")
trainLabel = train[, ncol(train)]
head(trainLabel)

set.seed(1)
train_train = sample_frac(train, 0.7) 
train_train_label = train_train[ , ncol(train_train)]
trainIdx = as.numeric(row.names(train_train))
train_test = train[-trainIdx, ]
train_test_label = train_test[ , ncol(train_test)]

train_shape = train_train[, c(1,3,5,7,9)]
train_number = train_train[, c(2,4,6,8,10)]

test_shape = train_test[, c(1,3,5,7,9)]
test_number = train_test[, c(2,4,6,8,10)]

maxCount = function(x){
  count = c(0,0,0,0)
  for( i in 1:length(x)){
    if(x[i] == 1)
      count[1] = count[1] + 1
    else if(x[i] == 2)
      count[2] = count[2] + 1
    else if(x[i] == 3)
      count[3] = count[3] + 1
    else if(x[i] == 4)
      count[4] = count[4] + 1
  }
  return(max(count))
}

count_func = function(x){
  return(max(table(x)))
}

length_table = function(x){
  return(length(table(x)))
}

differ_minmax = function(x){
  return(max(x) - min(x))
}
differ_number = apply(train_number, 1, differ_minmax)

accuracy = function(actual, predict){
  return(sum(actual == predict)/length(actual))
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


train_df = data.frame(shape_count, number_count, not_dup_card, differ_number, not_dup_shape, train_train_label = factor(train_train_label))
test_df = data.frame(shape_count = shape_count_te, number_count = number_count_te,
                     not_dup_card = not_dup_card_te, differ_number = differ_number_te,
                     not_dup_shape = not_dup_shape_te, train_test_label = factor(train_test_label))
row.names(train_df) <- NULL
row.names(test_df) <- NULL


# randomForest
RF_poker = randomForest(train_train_label ~ ., data = train_df, ntree = 500, importance = TRUE,
                         replace = FALSE, proximity = TRUE)
varImpPlot(RF_poker)
pred_RF_poker = predict(RF_poker, test_df)
accuracy(test_df$train_test_label, pred_RF_poker) # 96.05%

RF_poker
