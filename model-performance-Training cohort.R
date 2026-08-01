############################################################
# Training cohort model performance heatmap
# 5 models × 6 metrics
############################################################


library(caret)
library(pROC)
library(dplyr)
library(ggplot2)
library(tidyr)



#=========================
# 1. 准备训练集
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



train_ml <- train_data %>%
  select(
    all_of(final_features),
    all_of(outcome)
  )



train_ml[[outcome]] <-
  factor(
    train_ml[[outcome]],
    levels=c(0,1),
    labels=c("Low","High")
  )



train_x <- predict(
  preprocess,
  train_ml %>%
    select(-all_of(outcome))
)


train_y <- train_ml[[outcome]]



#=========================
# 2. 五模型预测概率
#=========================


prob_list <- list(
  
  "Logistic regression" =
    predict(model_glm, train_x, type="prob")$High,
  
  
  "SVM" =
    predict(model_svm, train_x, type="prob")$High,
  
  
  "Neural network" =
    predict(model_nnet, train_x, type="prob")$High,
  
  
  "XGBoost" =
    predict(model_xgb, train_x, type="prob")$High,
  
  
  "AdaBoost" =
    predict(model_ada, train_x, type="prob")$High
  
)



#=========================
# 3. 计算六个指标
#=========================


performance <- data.frame()



for(i in names(prob_list)){
  
  
  # ROC
  roc_obj <- roc(
    train_y,
    prob_list[[i]]
  )
  
  
  auc_value <- as.numeric(
    auc(roc_obj)
  )
  
  
  # 分类阈值0.5
  
  pred_class <- ifelse(
    prob_list[[i]] >=0.5,
    "High",
    "Low"
  )
  
  
  pred_class <- factor(
    pred_class,
    levels=c("Low","High")
  )
  
  
  cm <- confusionMatrix(
    pred_class,
    train_y,
    positive="High"
  )
  
  
  temp <- data.frame(
    
    Model=i,
    
    AUC=auc_value,
    
    Accuracy=
      cm$overall["Accuracy"],
    
    Sensitivity=
      cm$byClass["Sensitivity"],
    
    Specificity=
      cm$byClass["Specificity"],
    
    Precision=
      cm$byClass["Precision"],
    
    F1_Score=
      cm$byClass["F1"]
    
  )
  
  
  performance <-
    rbind(
      performance,
      temp
    )
  
}



print(performance)



write.csv(
  performance,
  "Training_model_performance.csv",
  row.names=FALSE
)



#=========================
# 4. 转换热图格式
#=========================


heat_data <- performance %>%
  pivot_longer(
    cols=c(
      AUC,
      Accuracy,
      Sensitivity,
      Specificity,
      Precision,
      F1_Score
    ),
    names_to="Metric",
    values_to="Value"
  )



#=========================
# 5. 绘制热图
#=========================


p <- ggplot(
  heat_data,
  aes(
    x=Model,
    y=Metric,
    fill=Value
  )
)+
  
  geom_tile(
    color="white"
  )+
  
  geom_text(
    aes(
      label=round(Value,3)
    ),
    size=4
  )+
  
  scale_fill_gradient(
    name="Performance",
    limits=c(0,1)
  )+
  
  theme_classic()+
  
  theme(
    axis.text.x=
      element_text(
        angle=45,
        hjust=1
      )
  )+
  
  labs(
    title="Training cohort performance",
    x=NULL,
    y=NULL
  )



print(p)



#=========================
# 6. 保存图片
#=========================


ggsave(
  "Training_model_performance_heatmap.pdf",
  p,
  width=7,
  height=5
)



############################################################
# END
############################################################