############################################################
# Training cohort calibration curves
# Five machine learning models
############################################################


library(ggplot2)
library(dplyr)



#=========================
# 1. 准备校准数据
#=========================


calibration_data <- data.frame(
  
  Outcome =
    ifelse(
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
# 2. 校准曲线函数
#=========================


calibration_curve <- function(prob, outcome, model_name){
  
  
  cal_df <- data.frame(
    pred = prob,
    obs = outcome
  )
  
  
  fit <- loess(
    obs ~ pred,
    data = cal_df,
    span = 0.75
  )
  
  
  newdata <- data.frame(
    pred = seq(0,1,length.out=100)
  )
  
  
  newdata$obs <- predict(
    fit,
    newdata
  )
  
  
  newdata$Model <- model_name
  
  return(newdata)
  
}



#=========================
# 3. 五模型校准数据
#=========================


cal_all <- bind_rows(
  
  calibration_curve(
    calibration_data$Logistic_regression,
    calibration_data$Outcome,
    "Logistic regression"
  ),
  
  
  calibration_curve(
    calibration_data$SVM,
    calibration_data$Outcome,
    "SVM"
  ),
  
  
  calibration_curve(
    calibration_data$Neural_network,
    calibration_data$Outcome,
    "Neural network"
  ),
  
  
  calibration_curve(
    calibration_data$XGBoost,
    calibration_data$Outcome,
    "XGBoost"
  ),
  
  
  calibration_curve(
    calibration_data$AdaBoost,
    calibration_data$Outcome,
    "AdaBoost"
  )
  
)



#=========================
# 4. 绘制校准曲线
#=========================


p <- ggplot(
  cal_all,
  aes(
    x=pred,
    y=obs,
    color=Model
  )
)+
  
  geom_line(
    linewidth=1.1
  )+
  
  geom_abline(
    intercept=0,
    slope=1,
    linetype="dashed"
  )+
  
  scale_x_continuous(
    limits=c(0,1)
  )+
  
  scale_y_continuous(
    limits=c(0,1)
  )+
  
  theme_classic()+
  
  labs(
    title="Training cohort calibration curves",
    x="Predicted probability",
    y="Observed probability"
  )+
  
  theme(
    legend.position="bottom"
  )



print(p)



#=========================
# 5. 保存
#=========================


ggsave(
  "Training_calibration_curve_five_models.pdf",
  p,
  width=7,
  height=6
)



############################################################
# END
############################################################