############################################################
# Machine learning model development
# Models:
# Logistic regression
# SVM
# Neural Network
# XGBoost
# AdaBoost
############################################################


#=========================
# 1. 加载包
#=========================

library(caret)
library(pROC)
library(xgboost)
library(kernlab)
library(nnet)
library(ada)
library(dplyr)



#=========================
# 2. 设置最终预测变量
#=========================


final_features <- c(
  "GDF15",
  "CA-199",
  "GGT",
  "CTP score",
  "eGFR",
  "TyG index"
)



outcome <- "RFH-NPT criteria"



#=========================
# 3. 构建训练数据
#=========================


train_ml <- train_data %>%
  select(
    all_of(final_features),
    all_of(outcome)
  )



# 因变量factor

train_ml[[outcome]] <-
  factor(
    train_ml[[outcome]],
    levels=c(0,1),
    labels=c("Low","High")
  )



#=========================
# 4. 数据标准化
#=========================


preprocess <- preProcess(
  train_ml %>% select(-all_of(outcome)),
  method=c(
    "center",
    "scale"
  )
)


train_x <- predict(
  preprocess,
  train_ml %>% select(-all_of(outcome))
)


train_y <- train_ml[[outcome]]



#=========================
# 5. 交叉验证设置
#=========================


set.seed(123)


ctrl <- trainControl(
  method="repeatedcv",
  number=10,
  repeats=3,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  savePredictions=TRUE
)



#=========================
# 6. Logistic regression
#=========================


set.seed(123)


model_glm <- train(
  x=train_x,
  y=train_y,
  method="glm",
  family="binomial",
  metric="ROC",
  trControl=ctrl
)



#=========================
# 7. SVM
#=========================


set.seed(123)


model_svm <- train(
  x=train_x,
  y=train_y,
  method="svmRadial",
  metric="ROC",
  trControl=ctrl,
  tuneLength=10
)



#=========================
# 8. Neural network
#=========================


set.seed(123)


model_nnet <- train(
  x=train_x,
  y=train_y,
  method="nnet",
  metric="ROC",
  trControl=ctrl,
  tuneLength=10,
  trace=FALSE
)



#=========================
# 9. XGBoost
#=========================


set.seed(123)


model_xgb <- train(
  x=train_x,
  y=train_y,
  method="xgbTree",
  metric="ROC",
  trControl=ctrl,
  tuneLength=10
)



#=========================
# 10. AdaBoost
#=========================


set.seed(123)


model_ada <- train(
  x=train_x,
  y=train_y,
  method="AdaBoost.M1",
  metric="ROC",
  trControl=ctrl,
  tuneLength=10
)



#=========================
# 11. 汇总模型
#=========================


models <- list(
  Logistic=model_glm,
  SVM=model_svm,
  NeuralNetwork=model_nnet,
  XGBoost=model_xgb,
  AdaBoost=model_ada
)



#=========================
# 12. 查看模型性能
#=========================


model_compare <- resamples(models)


summary(model_compare)



# 保存

save(
  models,
  preprocess,
  file="RFH_NPT_ML_training_models.RData"
)



############################################################
# END
############################################################