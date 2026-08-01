############################################################
# Internal validation
# Predict using trained models
############################################################


library(caret)
library(pROC)
library(dplyr)
library(ggplot2)



#=========================
# 1. 加载训练模型
#=========================

load(
  "RFH_NPT_ML_training_models.RData"
)



#=========================
# 2. 设置变量
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
# 3. 准备内部验证数据
#=========================


internal_ml <- internal_data %>%
  select(
    all_of(final_features),
    all_of(outcome)
  )



internal_ml[[outcome]] <-
  factor(
    internal_ml[[outcome]],
    levels=c(0,1),
    labels=c("Low","High")
  )



#=========================
# 4. 使用训练集标准化参数
#=========================


internal_x <- predict(
  preprocess,
  internal_ml %>%
    select(-all_of(outcome))
)


internal_y <- internal_ml[[outcome]]



#=========================
# 5. 五模型预测概率
#=========================


pred_glm <- predict(
  model_glm,
  internal_x,
  type="prob"
)$High


pred_svm <- predict(
  model_svm,
  internal_x,
  type="prob"
)$High


pred_nnet <- predict(
  model_nnet,
  internal_x,
  type="prob"
)$High


pred_xgb <- predict(
  model_xgb,
  internal_x,
  type="prob"
)$High


pred_ada <- predict(
  model_ada,
  internal_x,
  type="prob"
)$High



#=========================
# 6. ROC分析
#=========================


roc_glm <- roc(
  internal_y,
  pred_glm
)


roc_svm <- roc(
  internal_y,
  pred_svm
)


roc_nnet <- roc(
  internal_y,
  pred_nnet
)


roc_xgb <- roc(
  internal_y,
  pred_xgb
)


roc_ada <- roc(
  internal_y,
  pred_ada
)



#=========================
# 7. 输出AUC
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
  )
  
)



print(auc_results)



write.csv(
  auc_results,
  "Internal_validation_AUC.csv",
  row.names=FALSE
)



#=========================
# 8. ROC曲线绘制
#=========================


roc_list <- list(
  
  "Logistic regression"=roc_glm,
  "SVM"=roc_svm,
  "Neural network"=roc_nnet,
  "XGBoost"=roc_xgb,
  "AdaBoost"=roc_ada
  
)



pdf(
  "Internal_validation_ROC.pdf",
  width=7,
  height=7
)


plot(
  roc_glm,
  col=1,
  legacy.axes=TRUE,
  main="Internal validation ROC"
)


plot(
  roc_svm,
  add=TRUE,
  col=2
)


plot(
  roc_nnet,
  add=TRUE,
  col=3
)


plot(
  roc_xgb,
  add=TRUE,
  col=4
)


plot(
  roc_ada,
  add=TRUE,
  col=5
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
  col=1:5,
  lwd=2
)


dev.off()



############################################################
# END
############################################################