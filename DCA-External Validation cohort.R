############################################################
# External validation cohort Decision Curve Analysis (DCA)
# Five machine learning models
############################################################


library(rmda)
library(dplyr)
library(ggplot2)



#=========================
# 1. 准备外部验证集DCA数据
#=========================


external_dca_data <- data.frame(
  
  outcome = ifelse(
    external_y=="High",
    1,
    0
  ),
  
  Logistic_regression =
    external_prob_list[["Logistic regression"]],
  
  SVM =
    external_prob_list[["SVM"]],
  
  Neural_network =
    external_prob_list[["Neural network"]],
  
  XGBoost =
    external_prob_list[["XGBoost"]],
  
  AdaBoost =
    external_prob_list[["AdaBoost"]]
  
)



#=========================
# 2. 五个模型DCA计算
#=========================


dca_logistic <- decision_curve(
  outcome ~ Logistic_regression,
  data=external_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_svm <- decision_curve(
  outcome ~ SVM,
  data=external_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_nn <- decision_curve(
  outcome ~ Neural_network,
  data=external_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_xgb <- decision_curve(
  outcome ~ XGBoost,
  data=external_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_ada <- decision_curve(
  outcome ~ AdaBoost,
  data=external_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 3. 合并DCA模型
#=========================


external_dca_models <- list(
  
  Logistic=dca_logistic,
  SVM=dca_svm,
  Neural_network=dca_nn,
  XGBoost=dca_xgb,
  AdaBoost=dca_ada
  
)



#=========================
# 4. 绘制外部验证DCA
#=========================


plot_decision_curve(
  
  external_dca_models,
  
  curve.names=c(
    "Logistic regression",
    "SVM",
    "Neural network",
    "XGBoost",
    "AdaBoost"
  ),
  
  xlab="Threshold probability",
  
  ylab="Net benefit",
  
  standardize=FALSE,
  
  legend.position="bottom"
)



#=========================
# 5. 保存PDF
#=========================


pdf(
  "External_validation_DCA_five_models.pdf",
  width=7,
  height=6
)


plot_decision_curve(
  
  external_dca_models,
  
  curve.names=c(
    "Logistic regression",
    "SVM",
    "Neural network",
    "XGBoost",
    "AdaBoost"
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