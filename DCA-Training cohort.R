############################################################
# Training cohort Decision Curve Analysis (DCA)
# Five machine learning models
############################################################


library(rmda)
library(dplyr)
library(ggplot2)



#=========================
# 1. 准备DCA数据
#=========================


dca_data <- data.frame(
  
  outcome = ifelse(
    train_y=="High",
    1,
    0
  ),
  
  Logistic_regression =
    prob_list[["Logistic regression"]],
  
  SVM =
    prob_list[["SVM"]],
  
  Neural_network =
    prob_list[["Neural network"]],
  
  XGBoost =
    prob_list[["XGBoost"]],
  
  AdaBoost =
    prob_list[["AdaBoost"]]
  
)



#=========================
# 2. 分别建立DCA模型
#=========================


dca_logistic <- decision_curve(
  outcome ~ Logistic_regression,
  data=dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)


dca_svm <- decision_curve(
  outcome ~ SVM,
  data=dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_nn <- decision_curve(
  outcome ~ Neural_network,
  data=dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_xgb <- decision_curve(
  outcome ~ XGBoost,
  data=dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



dca_ada <- decision_curve(
  outcome ~ AdaBoost,
  data=dca_data,
  family="binomial",
  thresholds=seq(0,1,by=0.01),
  confidence.intervals=FALSE,
  study.design="cohort"
)



#=========================
# 3. 合并DCA结果
#=========================


dca_models <- list(
  Logistic=dca_logistic,
  SVM=dca_svm,
  Neural_network=dca_nn,
  XGBoost=dca_xgb,
  AdaBoost=dca_ada
)



#=========================
# 4. 绘制DCA曲线
#=========================


plot_decision_curve(
  
  dca_models,
  
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
# 5. 保存图片
#=========================


pdf(
  "Training_DCA_five_models.pdf",
  width=7,
  height=6
)


plot_decision_curve(
  
  dca_models,
  
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