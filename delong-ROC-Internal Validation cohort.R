############################################################
# Internal validation ROC curves
# Model A-F
############################################################


library(pROC)
library(ggplot2)
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
# 2. 真实结局
#=========================


internal_y_roc <- as.numeric(
  internal_data$`RFH-NPT criteria`
)



#=========================
# 3. ROC对象
#=========================


roc_A_internal <- roc(
  internal_y_roc,
  internal_pred_A
)


roc_B_internal <- roc(
  internal_y_roc,
  internal_pred_B
)


roc_C_internal <- roc(
  internal_y_roc,
  internal_pred_C
)


roc_D_internal <- roc(
  internal_y_roc,
  internal_pred_D
)


roc_E_internal <- roc(
  internal_y_roc,
  internal_pred_E
)


roc_F_internal <- roc(
  internal_y_roc,
  internal_pred_F
)



#=========================
# 4. AUC结果
#=========================


internal_auc <- data.frame(
  
  Model=c(
    "Model A",
    "Model B",
    "Model C",
    "Model D",
    "Model E",
    "Model F"
  ),
  
  AUC=c(
    auc(roc_A_internal),
    auc(roc_B_internal),
    auc(roc_C_internal),
    auc(roc_D_internal),
    auc(roc_E_internal),
    auc(roc_F_internal)
  )
)


print(internal_auc)



#=========================
# 5. ROC数据
#=========================


roc_list_internal <- list(
  
  "Model A: CTP score"=roc_A_internal,
  
  "Model B: CTP score + TyG"=roc_B_internal,
  
  "Model C: CTP score + GDF15"=roc_C_internal,
  
  "Model D: CTP score + GGT + CA19-9 + GDF15 + eGFR"=
    roc_D_internal,
  
  "Model E: CTP score + GGT + CA19-9 + TyG + eGFR + GDF15"=
    roc_E_internal,
  
  "Model F: Full model"=
    roc_F_internal
  
)



roc_data_internal <- data.frame()



for(i in names(roc_list_internal)){
  
  
  temp <- data.frame(
    
    specificity =
      roc_list_internal[[i]]$specificities,
    
    sensitivity =
      roc_list_internal[[i]]$sensitivities,
    
    Model=i
    
  )
  
  
  roc_data_internal <-
    rbind(
      roc_data_internal,
      temp
    )
  
}



#=========================
# 6. 绘制ROC曲线
#=========================


p <- ggplot(
  roc_data_internal,
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
    title="ROC curves for six models (Internal validation cohort)",
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
  "Internal_validation_ROC_Model_A_F.pdf",
  p,
  width=8,
  height=6
)



write.csv(
  internal_auc,
  "Internal_validation_Model_A_F_AUC.csv",
  row.names=FALSE
)



############################################################
# END
############################################################