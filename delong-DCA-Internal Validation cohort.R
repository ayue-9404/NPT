############################################################
# Internal validation cohort Decision Curve Analysis (DCA)
# Model A-F
############################################################


library(rmda)
library(dplyr)



#=========================
# 1. 内部验证集预测概率
#=========================


internal_pred_A <- predict(
  model_A,
  newdata=internal_data,
  type="response"
)


internal_pred_B <- predict(
  model_B,
  newdata=internal_data,
  type="response"
)


internal_pred_C <- predict(
  model_C,
  newdata=internal_data,
  type="response"
)


internal_pred_D <- predict(
  model_D,
  newdata=internal_data,
  type="response"
)


internal_pred_E <- predict(
  model_E,
  newdata=internal_data,
  type="response"
)


internal_pred_F <- predict(
  model_F,
  newdata=internal_data,
  type="response"
)



#=========================
# 2. 构建DCA数据
#=========================


dca_internal_data <- data.frame(
  
  Outcome =
    as.numeric(
      internal_data$`RFH-NPT criteria`
    ),
  
  Model_A =
    internal_pred_A,
  
  Model_B =
    internal_pred_B,
  
  Model_C =
    internal_pred_C,
  
  Model_D =
    internal_pred_D,
  
  Model_E =
    internal_pred_E,
  
  Model_F =
    internal_pred_F
  
)



#=========================
# 3. 计算DCA
#=========================


dca_A <- decision_curve(
  Outcome ~ Model_A,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_B <- decision_curve(
  Outcome ~ Model_B,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_C <- decision_curve(
  Outcome ~ Model_C,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_D <- decision_curve(
  Outcome ~ Model_D,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_E <- decision_curve(
  Outcome ~ Model_E,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_F <- decision_curve(
  Outcome ~ Model_F,
  data=dca_internal_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 4. 合并模型
#=========================


internal_dca_models <- list(
  
  dca_A,
  dca_B,
  dca_C,
  dca_D,
  dca_E,
  dca_F
  
)



#=========================
# 5. 绘制内部验证DCA
#=========================


plot_decision_curve(
  
  internal_dca_models,
  
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
  "Internal_validation_DCA_Model_A_F.pdf",
  width=8,
  height=6
)


plot_decision_curve(
  
  internal_dca_models,
  
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