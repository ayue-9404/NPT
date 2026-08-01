############################################################
# Random Forest Recursive Feature Elimination (RF-RFE)
# After univariate logistic regression
# Outcome: RFH-NPT criteria
############################################################


#========================
# 1. 加载包
#========================

library(caret)
library(randomForest)
library(dplyr)
library(ggplot2)



#========================
# 2. 提取单因素显著变量
#========================


rfe_vars <- univ_results %>%
  filter(p.value < 0.05) %>%
  pull(Variable) %>%
  unique()


print(rfe_vars)



#========================
# 3. 构建RFE数据
#========================


rfe_data <- data %>%
  select(
    all_of(rfe_vars),
    `RFH-NPT criteria`
  )


# 因变量转factor

rfe_data$`RFH-NPT criteria` <-
  factor(
    rfe_data$`RFH-NPT criteria`,
    levels=c(0,1),
    labels=c("Low","High")
  )



#========================
# 4. 设置RFE控制参数
#========================


set.seed(123)


ctrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  verbose = TRUE
)



#========================
# 5. 执行RF-RFE
#========================


rfe_model <- rfe(
  x = rfe_data %>%
    select(-`RFH-NPT criteria`),
  
  y = rfe_data$`RFH-NPT criteria`,
  
  sizes = c(
    1:length(rfe_vars)
  ),
  
  rfeControl = ctrl
)



# 查看结果

print(rfe_model)



#========================
# 6. 最佳变量
#========================


selected_rfe <- predictors(rfe_model)


print(selected_rfe)



write.csv(
  data.frame(
    Variable=selected_rfe
  ),
  "RF_RFE_selected_variables.csv",
  row.names=FALSE
)



#========================
# 图1
# RFE性能曲线
#========================


pdf(
  "RF_RFE_performance_curve.pdf",
  width=7,
  height=6
)


plot(
  rfe_model,
  type=c("g","o")
)


dev.off()



#========================
# 7. 随机森林变量重要性
#========================


rf_importance <- varImp(
  rfe_model$fit,
  scale=TRUE
)


importance_data <- data.frame(
  Variable=rownames(rf_importance),
  Importance=rf_importance$Overall
)


importance_data <- importance_data %>%
  arrange(desc(Importance))



write.csv(
  importance_data,
  "RF_RFE_variable_importance.csv",
  row.names=FALSE
)



#========================
# 图2
# 变量重要性图
#========================


p <- ggplot(
  importance_data,
  aes(
    x=reorder(Variable,Importance),
    y=Importance
  )
)+
  
  geom_col()+
  
  coord_flip()+
  
  theme_classic()+
  
  labs(
    x=NULL,
    y="Random forest importance"
  )



print(p)



ggsave(
  "RF_RFE_variable_importance.pdf",
  p,
  width=7,
  height=6
)



############################################################
# END
############################################################