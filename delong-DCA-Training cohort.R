############################################################
# Training cohort Decision Curve Analysis (DCA)
# Model A-F
############################################################


library(rmda)
library(dplyr)



#=========================
# 1. 获取训练集预测概率
#=========================


train_pred_A <- predict(
  model_A,
  newdata=train_data,
  type="response"
)


train_pred_B <- predict(
  model_B,
  newdata=train_data,
  type="response"
)


train_pred_C <- predict(
  model_C,
  newdata=train_data,
  type="response"
)


train_pred_D <- predict(
  model_D,
  newdata=train_data,
  type="response"
)


train_pred_E <- predict(
  model_E,
  newdata=train_data,
  type="response"
)


train_pred_F <- predict(
  model_F,
  newdata=train_data,
  type="response"
)



#=========================
# 2. DCA数据
#=========================


dca_train_data <- data.frame(
  
  Outcome =
    as.numeric(
      train_data$`RFH-NPT criteria`
    ),
  
  Model_A =
    train_pred_A,
  
  Model_B =
    train_pred_B,
  
  Model_C =
    train_pred_C,
  
  Model_D =
    train_pred_D,
  
  Model_E =
    train_pred_E,
  
  Model_F =
    train_pred_F
  
)



#=========================
# 3. 六个模型DCA
#=========================


dca_A <- decision_curve(
  Outcome ~ Model_A,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_B <- decision_curve(
  Outcome ~ Model_B,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_C <- decision_curve(
  Outcome ~ Model_C,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_D <- decision_curve(
  Outcome ~ Model_D,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_E <- decision_curve(
  Outcome ~ Model_E,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_F <- decision_curve(
  Outcome ~ Model_F,
  data=dca_train_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 4. 合并DCA
#=========================


dca_models <- list(
  
  dca_A,
  dca_B,
  dca_C,
  dca_D,
  dca_E,
  dca_F
  
)



#=========================
# 5. 绘制DCA曲线
#=========================


plot_decision_curve(
  
  dca_models,
  
  curve.names=c(
    "Model A: CTP score",
    "Model B: CTP score + TyG",
    "Model C: CTP score + GDF15",
    "Model D: CTP score + GGT + CA19-9 + GDF15 + eGFR",
    "Model E: CTP score + GGT + CA19-9 + TyG + eGFR + GDF15",
    "Model F: Full model"
  ),
  
  xlab="Threshold probability",
  
  ylab="Net benefit",
  
  standardize=FALSE,
  
  legend.position="bottom"
)



#=========================
# 6. 保存
#=========================


pdf(
  "Training_DCA_Model_A_F.pdf",
  width=8,
  height=6
)


plot_decision_curve(
  
  dca_models,
  
  curve.names=c(
    "Model A",
    "Model B",
    "Model C",
    "Model D",
    "Model E",
    "Model F"
  ),
  
  xlab="Threshold probability",
  
  ylab="Net benefit",
  
  standardize=FALSE,
  
  legend.position="bottom"
)


dev.off()



############################################################
# END
############################################################