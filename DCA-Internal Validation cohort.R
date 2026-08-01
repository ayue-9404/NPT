############################################################
# Internal validation cohort Decision Curve Analysis (DCA)
# Five machine learning models
############################################################


library(rmda)
library(dplyr)
library(ggplot2)



#=========================
# 1. 准备内部验证集预测概率
#=========================


internal_dca_data <- data.frame(
  
  outcome = ifelse(
    internal_y=="High",
    1,
    0
  ),
  
  Logistic_regression =
    internal_prob_list[["Logistic regression"]],
  
  SVM =
    internal_prob_list[["SVM"]],
  
  Neural_network =
    internal_prob_list[["Neural network"]],
  
  XGBoost =
    internal_prob_list[["XGBoost"]],
  
  AdaBoost =
    internal_prob_list[["AdaBoost"]]
  
)



#=========================
# 2. 五个模型DCA
#=========================


dca_logistic <- decision_curve(
  outcome ~ Logistic_regression,
  data=internal_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_svm <- decision_curve(
  outcome ~ SVM,
  data=internal_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_nn <- decision_curve(
  outcome ~ Neural_network,
  data=internal_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_xgb <- decision_curve(
  outcome ~ XGBoost,
  data=internal_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_ada <- decision_curve(
  outcome ~ AdaBoost,
  data=internal_dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 3. 合并模型
#=========================


internal_dca_models <- list(
  
  Logistic=dca_logistic,
  SVM=dca_svm,
  Neural_network=dca_nn,
  XGBoost=dca_xgb,
  AdaBoost=dca_ada
  
)



#=========================
# 4. 绘制内部验证DCA
#=========================


plot_decision_curve(
  
  internal_dca_models,
  
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
  "Internal_validation_DCA_five_models.pdf",
  width=7,
  height=6
)


plot_decision_curve(
  
  internal_dca_models,
  
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