# =====================================================
# Confusion matrices under alternative RFH-NPT thresholds
# XGBoost model
# =====================================================


# =========================
# 1. 加载包
# =========================

library(readxl)
library(xgboost)
library(pROC)
library(ggplot2)
library(dplyr)



# =========================
# 2. 读取数据
# =========================

data <- read_excel(
  "六变量敏感性.xlsx"
)



# =========================
# 3. 数据整理
# =========================


# 六变量

x <- data %>%
  select(
    `CTP score`,
    GDF15,
    CA_199,
    GGT,
    GFR,
    TyG
  )


x_matrix <- as.matrix(x)



# 三个结局

y_primary <- as.integer(
  data$Primary
)


y_s1 <- as.integer(
  data$`Sensitivity 1`
)


y_s2 <- as.integer(
  data$`Sensitivity 2`
)



# =========================
# 4. XGBoost训练函数
# =========================


train_xgb <- function(x, y){
  
  
  dtrain <- xgb.DMatrix(
    
    data = x,
    
    label = y
    
  )
  
  
  
  model <- xgb.train(
    
    params = list(
      
      objective = "binary:logistic",
      
      eval_metric = "logloss",
      
      max_depth = 3,
      
      learning_rate = 0.05,
      
      subsample = 0.8,
      
      colsample_bytree = 0.8
      
    ),
    
    data = dtrain,
    
    nrounds = 100,
    
    verbose = 0
    
  )
  
  
  return(model)
  
}



# =========================
# 5. 三个阈值分别建模
# =========================


model_primary <- train_xgb(
  
  x_matrix,
  
  y_primary
  
)



model_s1 <- train_xgb(
  
  x_matrix,
  
  y_s1
  
)



model_s2 <- train_xgb(
  
  x_matrix,
  
  y_s2
  
)



# =========================
# 6. 获取预测概率
# =========================


data$Pred_Primary <- predict(
  
  model_primary,
  
  x_matrix
  
)



data$Pred_Sensitivity1 <- predict(
  
  model_s1,
  
  x_matrix
  
)



data$Pred_Sensitivity2 <- predict(
  
  model_s2,
  
  x_matrix
  
)



# =========================
# 7. Youden index确定最佳cutoff
# =========================


roc_primary <- roc(
  
  data$Primary,
  
  data$Pred_Primary
  
)


cut_primary <- coords(
  
  roc_primary,
  
  x="best",
  
  best.method="youden",
  
  ret="threshold"
  
)



roc_s1 <- roc(
  
  data$`Sensitivity 1`,
  
  data$Pred_Sensitivity1
  
)


cut_s1 <- coords(
  
  roc_s1,
  
  x="best",
  
  best.method="youden",
  
  ret="threshold"
  
)



roc_s2 <- roc(
  
  data$`Sensitivity 2`,
  
  data$Pred_Sensitivity2
  
)


cut_s2 <- coords(
  
  roc_s2,
  
  x="best",
  
  best.method="youden",
  
  ret="threshold"
  
)



# 查看cutoff

cut_primary

cut_s1

cut_s2



# =========================
# 8. 根据cutoff分类
# =========================


data$Class_primary <- ifelse(
  
  data$Pred_Primary >= cut_primary,
  
  1,
  
  0
  
)



data$Class_s1 <- ifelse(
  
  data$Pred_Sensitivity1 >= cut_s1,
  
  1,
  
  0
  
)



data$Class_s2 <- ifelse(
  
  data$Pred_Sensitivity2 >= cut_s2,
  
  1,
  
  0
  
)



# =========================
# 9. 创建混淆矩阵函数
# =========================


make_cm <- function(pred, actual, model){
  
  
  TN <- sum(
    pred==0 & actual==0
  )
  
  
  FP <- sum(
    pred==1 & actual==0
  )
  
  
  FN <- sum(
    pred==0 & actual==1
  )
  
  
  TP <- sum(
    pred==1 & actual==1
  )
  
  
  df <- data.frame(
    
    Predicted=c(
      0,
      1,
      0,
      1
    ),
    
    
    Actual=c(
      0,
      0,
      1,
      1
    ),
    
    
    Value=c(
      TN,
      FP,
      FN,
      TP
    ),
    
    
    Type=c(
      "TN",
      "FP",
      "FN",
      "TP"
    ),
    
    
    Model=model
    
  )
  
  
  return(df)
  
}



# =========================
# 10. 三个混淆矩阵
# =========================


cm_all <- rbind(
  
  
  make_cm(
    
    data$Class_primary,
    
    data$Primary,
    
    "RFH-NPT ≥2"
    
  ),
  
  
  
  make_cm(
    
    data$Class_s1,
    
    data$`Sensitivity 1`,
    
    "RFH-NPT ≥1"
    
  ),
  
  
  
  make_cm(
    
    data$Class_s2,
    
    data$`Sensitivity 2`,
    
    "RFH-NPT ≥3"
    
  )
  
  
)



# =========================
# 11. 因子顺序
# =========================


cm_all$Type <- factor(
  
  cm_all$Type,
  
  levels=c(
    "TN",
    "FP",
    "FN",
    "TP"
  )
  
)



cm_all$Model <- factor(
  
  cm_all$Model,
  
  levels=c(
    "RFH-NPT ≥2",
    "RFH-NPT ≥1",
    "RFH-NPT ≥3"
  )
  
)



# =========================
# 12. SCI风格圆形混淆矩阵
# =========================


p_cm <- ggplot(
  
  cm_all,
  
  aes(
    
    x=factor(Predicted),
    
    y=factor(Actual)
    
  )
  
)+
  
  
  
  geom_point(
    
    aes(fill=Type),
    
    shape=21,
    
    size=16,
    
    color="black",
    
    stroke=0.8
    
  )+
  
  
  
  geom_text(
    
    aes(label=Value),
    
    size=6,
    
    fontface="bold",
    
    color="black"
    
  )+
  
  
  
  facet_wrap(
    
    ~Model,
    
    nrow=1
    
  )+
  
  
  
  scale_fill_manual(
    
    values=c(
      
      "TP"="#3A9D8F",
      
      "TN"="#457B9D",
      
      "FP"="#E76F51",
      
      "FN"="#B56576"
      
    )
    
  )+
  
  
  
  scale_x_discrete(
    
    labels=c(
      "0",
      "1"
    )
    
  )+
  
  
  
  scale_y_discrete(
    
    labels=c(
      "0",
      "1"
    )
    
  )+
  
  
  
  coord_fixed()+
  
  
  
  labs(
    
    title="Confusion matrices under alternative RFH-NPT thresholds",
    
    x="Predicted",
    
    y="Actual"
    
  )+
  
  
  
  theme_classic(
    
    base_size=16
    
  )+
  
  
  
  theme(
    
    legend.position="none",
    
    
    strip.background=
      element_blank(),
    
    
    strip.text=
      element_text(
        
        face="bold",
        
        size=15
        
      ),
    
    
    plot.title=
      element_text(
        
        hjust=0.5,
        
        face="bold",
        
        size=16
        
      ),
    
    
    axis.title=
      element_text(
        
        face="bold"
        
      ),
    
    
    panel.border=
      element_rect(
        
        color="black",
        
        fill=NA,
        
        linewidth=1
        
      )
    
  )



# 显示

print(p_cm)



# =========================
# 13. 保存
# =========================


ggsave(
  
  "RFHNPT_confusion_matrix_bubble.pdf",
  
  p_cm,
  
  width=8,
  
  height=3.8
  
)