############################################################
# External validation cohort Decision Curve Analysis (DCA)
# Model A-F
############################################################


library(rmda)
library(dplyr)



#=========================
# 1. 外部验证集预测概率
#=========================


external_pred_A <- predict(
  model_A,
  newdata=data1,
  type="response"
)


external_pred_B <- predict(
  model_B,
  newdata=data1,
  type="response"
)


external_pred_C <- predict(
  model_C,
  newdata=data1,
  type="response"
)


external_pred_D <- predict(
  model_D,
  newdata=data1,
  type="response"
)


external_pred_E <- predict(
  model_E,
  newdata=data1,
  type="response"
)


external_pred_F <- predict(
  model_F,
  newdata=data1,
  type="response"
)



#=========================
# 2. 构建DCA数据
#=========================


dca_external_data <- data.frame(
  
  Outcome =
    as.numeric(
      data1$`RFH-NPT criteria`
    ),
  
  Model_A =
    external_pred_A,
  
  Model_B =
    external_pred_B,
  
  Model_C =
    external_pred_C,
  
  Model_D =
    external_pred_D,
  
  Model_E =
    external_pred_E,
  
  Model_F =
    external_pred_F
  
)



#=========================
# 3. 六个模型DCA
#=========================


dca_A <- decision_curve(
  Outcome ~ Model_A,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_B <- decision_curve(
  Outcome ~ Model_B,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_C <- decision_curve(
  Outcome ~ Model_C,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_D <- decision_curve(
  Outcome ~ Model_D,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_E <- decision_curve(
  Outcome ~ Model_E,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_F <- decision_curve(
  Outcome ~ Model_F,
  data=dca_external_data,
  family="binomial",
  thresholds=seq(0,1,0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 4. 合并DCA模型
#=========================


external_dca_models <- list(
  
  dca_A,
  dca_B,
  dca_C,
  dca_D,
  dca_E,
  dca_F
  
)



#=========================
# 5. 绘制外部验证DCA
#=========================


plot_decision_curve(
  
  external_dca_models,
  
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
# 6. 保存PDF
#=========================


pdf(
  "External_validation_DCA_Model_A_F.pdf",
  width=8,
  height=6
)


plot_decision_curve(
  
  external_dca_models,
  
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