############################################################
# External validation
# Dataset: data1
# Models:
# Logistic regression
# SVM
# Neural network
# XGBoost
# AdaBoost
############################################################


#=========================
# 1. 加载包
#=========================

library(caret)
library(pROC)
library(dplyr)
library(ggplot2)



#=========================
# 2. 加载训练模型
#=========================

load(
  "RFH_NPT_ML_training_models.RData"
)



#=========================
# 3. 设置变量
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
# 4. 准备外部验证数据
#=========================


external_ml <- data1 %>%
  select(
    all_of(final_features),
    all_of(outcome)
  )



external_ml[[outcome]] <-
  factor(
    external_ml[[outcome]],
    levels=c(0,1),
    labels=c("Low","High")
  )



#=========================
# 5. 使用训练集标准化参数
#=========================


external_x <- predict(
  preprocess,
  external_ml %>%
    select(-all_of(outcome))
)



external_y <- external_ml[[outcome]]



#=========================
# 6. 五个模型预测
#=========================


pred_glm <- predict(
  model_glm,
  external_x,
  type="prob"
)$High



pred_svm <- predict(
  model_svm,
  external_x,
  type="prob"
)$High



pred_nnet <- predict(
  model_nnet,
  external_x,
  type="prob"
)$High



pred_xgb <- predict(
  model_xgb,
  external_x,
  type="prob"
)$High



pred_ada <- predict(
  model_ada,
  external_x,
  type="prob"
)$High



#=========================
# 7. ROC分析
#=========================


roc_glm <- roc(
  external_y,
  pred_glm
)


roc_svm <- roc(
  external_y,
  pred_svm
)


roc_nnet <- roc(
  external_y,
  pred_nnet
)


roc_xgb <- roc(
  external_y,
  pred_xgb
)


roc_ada <- roc(
  external_y,
  pred_ada
)



#=========================
# 8. AUC + 95% CI
#=========================


auc_results <- data.frame(
  
  Model=c(
    "Logistic regression",
    "SVM",
    "Neural network",
    "XGBoost",
    "AdaBoost"
  ),
  
  
  AUC=c(
    auc(roc_glm),
    auc(roc_svm),
    auc(roc_nnet),
    auc(roc_xgb),
    auc(roc_ada)
  ),
  
  
  CI_low=c(
    ci.auc(roc_glm)[1],
    ci.auc(roc_svm)[1],
    ci.auc(roc_nnet)[1],
    ci.auc(roc_xgb)[1],
    ci.auc(roc_ada)[1]
  ),
  
  
  CI_high=c(
    ci.auc(roc_glm)[3],
    ci.auc(roc_svm)[3],
    ci.auc(roc_nnet)[3],
    ci.auc(roc_xgb)[3],
    ci.auc(roc_ada)[3]
  )
  
)



print(auc_results)



write.csv(
  auc_results,
  "External_validation_AUC.csv",
  row.names=FALSE
)



#=========================
# 9. 外部验证ROC曲线
#=========================


roc_list <- list(
  
  "Logistic regression"=roc_glm,
  "SVM"=roc_svm,
  "Neural network"=roc_nnet,
  "XGBoost"=roc_xgb,
  "AdaBoost"=roc_ada
  
)



pdf(
  "External_validation_ROC.pdf",
  width=7,
  height=7
)


plot(
  roc_glm,
  legacy.axes=TRUE,
  main="External validation ROC",
  lwd=2
)


plot(
  roc_svm,
  add=TRUE,
  lwd=2
)


plot(
  roc_nnet,
  add=TRUE,
  lwd=2
)


plot(
  roc_xgb,
  add=TRUE,
  lwd=2
)


plot(
  roc_ada,
  add=TRUE,
  lwd=2
)



legend(
  "bottomright",
  legend=paste0(
    names(roc_list),
    " AUC=",
    round(
      sapply(
        roc_list,
        auc
      ),
      3)
  ),
  lwd=2
)



dev.off()



############################################################
# END
############################################################