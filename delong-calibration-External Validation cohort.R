############################################################
# External validation calibration curves
# Model A-F
############################################################


library(ggplot2)
library(dplyr)



#=========================
# 1. 外部验证集预测概率
#=========================


external_prob_A <- predict(
  model_A,
  newdata=data1,
  type="response"
)


external_prob_B <- predict(
  model_B,
  newdata=data1,
  type="response"
)


external_prob_C <- predict(
  model_C,
  newdata=data1,
  type="response"
)


external_prob_D <- predict(
  model_D,
  newdata=data1,
  type="response"
)


external_prob_E <- predict(
  model_E,
  newdata=data1,
  type="response"
)


external_prob_F <- predict(
  model_F,
  newdata=data1,
  type="response"
)



#=========================
# 2. 校准数据
#=========================


calibration_data <- data.frame(
  
  Outcome =
    as.numeric(
      data1$`RFH-NPT criteria`
    ),
  
  Model_A=external_prob_A,
  Model_B=external_prob_B,
  Model_C=external_prob_C,
  Model_D=external_prob_D,
  Model_E=external_prob_E,
  Model_F=external_prob_F
  
)



#=========================
# 3. 校准曲线函数
#=========================


calibration_curve <- function(prob, outcome, model_name){
  
  
  df <- data.frame(
    Predicted=prob,
    Observed=outcome
  )
  
  
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
# 4. 生成六模型校准数据
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
# 5. 绘制外部验证校准曲线
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
    title="Calibration curves of six models (External validation cohort)",
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
  "External_validation_calibration_Model_A_F.pdf",
  p,
  width=8,
  height=6
)



############################################################
# END
############################################################