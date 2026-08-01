############################################################
# Training cohort calibration curves
# Model A-F
############################################################


library(ggplot2)
library(dplyr)



#=========================
# 1. 获取训练集预测概率
#=========================


train_prob_A <- predict(
  model_A,
  newdata=train_data,
  type="response"
)


train_prob_B <- predict(
  model_B,
  newdata=train_data,
  type="response"
)


train_prob_C <- predict(
  model_C,
  newdata=train_data,
  type="response"
)


train_prob_D <- predict(
  model_D,
  newdata=train_data,
  type="response"
)


train_prob_E <- predict(
  model_E,
  newdata=train_data,
  type="response"
)


train_prob_F <- predict(
  model_F,
  newdata=train_data,
  type="response"
)



#=========================
# 2. 校准数据
#=========================


calibration_data <- data.frame(
  
  Outcome =
    as.numeric(
      train_data$`RFH-NPT criteria`
    ),
  
  Model_A=train_prob_A,
  Model_B=train_prob_B,
  Model_C=train_prob_C,
  Model_D=train_prob_D,
  Model_E=train_prob_E,
  Model_F=train_prob_F
  
)



#=========================
# 3. 校准曲线函数
#=========================


calibration_curve <- function(prob, outcome, model_name){
  
  
  df <- data.frame(
    Predicted=prob,
    Observed=outcome
  )
  
  
  # loess平滑
  fit <- loess(
    Observed ~ Predicted,
    data=df,
    span=0.75
  )
  
  
  newdata <- data.frame(
    Predicted=seq(0,1,length.out=100)
  )
  
  
  newdata$Observed <-
    predict(
      fit,
      newdata
    )
  
  
  newdata$Model <- model_name
  
  return(newdata)
  
}



#=========================
# 4. 生成六模型校准曲线数据
#=========================


cal_all <- bind_rows(
  
  calibration_curve(
    calibration_data$Model_A,
    calibration_data$Outcome,
    "Model A: CTP score"
  ),
  
  
  calibration_curve(
    calibration_data$Model_B,
    calibration_data$Outcome,
    "Model B: CTP score + TyG"
  ),
  
  
  calibration_curve(
    calibration_data$Model_C,
    calibration_data$Outcome,
    "Model C: CTP score + GDF15"
  ),
  
  
  calibration_curve(
    calibration_data$Model_D,
    calibration_data$Outcome,
    "Model D"
  ),
  
  
  calibration_curve(
    calibration_data$Model_E,
    calibration_data$Outcome,
    "Model E"
  ),
  
  
  calibration_curve(
    calibration_data$Model_F,
    calibration_data$Outcome,
    "Model F: Full model"
  )
  
)



#=========================
# 5. 绘制校准曲线
#=========================


p <- ggplot(
  cal_all,
  aes(
    x=Predicted,
    y=Observed,
    color=Model
  )
)+
  
  geom_line(
    linewidth=1
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
    title="Calibration curves of six models (Training cohort)",
    x="Predicted probability",
    y="Observed probability",
    color=NULL
  )+
  
  theme(
    legend.position="right"
  )


print(p)



#=========================
# 6. 保存
#=========================


ggsave(
  "Training_calibration_Model_A_F.pdf",
  p,
  width=8,
  height=6
)



############################################################
# END
############################################################