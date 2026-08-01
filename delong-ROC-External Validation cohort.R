############################################################
# External validation ROC curves
# Model A-F
############################################################


library(pROC)
library(ggplot2)
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
# 2. 真实结局
#=========================


external_y_roc <- as.numeric(
  data1$`RFH-NPT criteria`
)



#=========================
# 3. ROC对象
#=========================


roc_A_external <- roc(
  external_y_roc,
  external_pred_A
)


roc_B_external <- roc(
  external_y_roc,
  external_pred_B
)


roc_C_external <- roc(
  external_y_roc,
  external_pred_C
)


roc_D_external <- roc(
  external_y_roc,
  external_pred_D
)


roc_E_external <- roc(
  external_y_roc,
  external_pred_E
)


roc_F_external <- roc(
  external_y_roc,
  external_pred_F
)



#=========================
# 4. AUC结果
#=========================


external_auc <- data.frame(
  
  Model=c(
    "Model A",
    "Model B",
    "Model C",
    "Model D",
    "Model E",
    "Model F"
  ),
  
  AUC=c(
    auc(roc_A_external),
    auc(roc_B_external),
    auc(roc_C_external),
    auc(roc_D_external),
    auc(roc_E_external),
    auc(roc_F_external)
  )
)


print(external_auc)



#=========================
# 5. ROC数据整理
#=========================


roc_list_external <- list(
  
  "Model A: CTP score"=
    roc_A_external,
  
  "Model B: CTP score + TyG"=
    roc_B_external,
  
  "Model C: CTP score + GDF15"=
    roc_C_external,
  
  "Model D: CTP score + GGT + CA19-9 + GDF15 + eGFR"=
    roc_D_external,
  
  "Model E: CTP score + GGT + CA19-9 + TyG + eGFR + GDF15"=
    roc_E_external,
  
  "Model F: Full model"=
    roc_F_external
  
)



roc_data_external <- data.frame()



for(i in names(roc_list_external)){
  
  
  temp <- data.frame(
    
    specificity =
      roc_list_external[[i]]$specificities,
    
    sensitivity =
      roc_list_external[[i]]$sensitivities,
    
    Model=i
    
  )
  
  
  roc_data_external <-
    rbind(
      roc_data_external,
      temp
    )
  
}



#=========================
# 6. 绘制ROC曲线
#=========================


p <- ggplot(
  roc_data_external,
  aes(
    x=1-specificity,
    y=sensitivity,
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
  
  theme_classic()+
  
  scale_x_continuous(
    limits=c(0,1)
  )+
  
  scale_y_continuous(
    limits=c(0,1)
  )+
  
  labs(
    title="ROC curves for six models (External validation cohort)",
    x="1 - Specificity",
    y="Sensitivity",
    color=NULL
  )+
  
  theme(
    legend.position="right",
    legend.text=element_text(size=9)
  )



print(p)



#=========================
# 7. 保存
#=========================


ggsave(
  "External_validation_ROC_Model_A_F.pdf",
  p,
  width=8,
  height=6
)



write.csv(
  external_auc,
  "External_validation_Model_A_F_AUC.csv",
  row.names=FALSE
)



############################################################
# END
############################################################