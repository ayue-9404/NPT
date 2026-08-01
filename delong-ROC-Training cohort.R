############################################################
# Training cohort ROC curves
# Model A-F
############################################################


library(pROC)
library(ggplot2)



#=========================
# 1. ROC对象列表
#=========================


roc_list <- list(
  
  "Model A: CTP score" = roc_A,
  
  "Model B: CTP score + TyG" = roc_B,
  
  "Model C: CTP score + GDF15" = roc_C,
  
  "Model D: CTP score + GGT + CA19-9 + GDF15 + eGFR" = roc_D,
  
  "Model E: CTP score + GGT + CA19-9 + TyG + eGFR + GDF15" = roc_E,
  
  "Model F: Full model" = roc_F
  
)



#=========================
# 2. 提取ROC曲线数据
#=========================


roc_data <- data.frame()



for(i in names(roc_list)){
  
  
  temp <- data.frame(
    
    specificity = roc_list[[i]]$specificities,
    
    sensitivity = roc_list[[i]]$sensitivities,
    
    Model = i
    
  )
  
  
  roc_data <- rbind(
    roc_data,
    temp
  )
  
}



#=========================
# 3. AUC标签
#=========================


auc_labels <- sapply(
  
  roc_list,
  
  function(x){
    
    paste0(
      names(x),
      " (AUC=",
      round(as.numeric(auc(x)),3),
      ")"
    )
    
  }
  
)



#=========================
# 4. 绘制ROC
#=========================


p <- ggplot(
  roc_data,
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
    title="ROC curves for six prediction models (Training cohort)",
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
# 5. 保存
#=========================


ggsave(
  "Training_ROC_Model_A_F.pdf",
  p,
  width=8,
  height=6
)



############################################################
# END
############################################################