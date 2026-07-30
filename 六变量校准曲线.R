# =====================================================
# XGBoost Calibration curves
# Sensitivity analysis of alternative RFH-NPT thresholds
# =====================================================


# =========================
# 1. 加载包
# =========================

library(readxl)
library(xgboost)
library(ggplot2)
library(dplyr)



# =========================
# 2. 读取数据
# =========================

data <- read_excel(
  "六变量敏感性.xlsx"
)


# 查看变量

names(data)



# =========================
# 3. 数据整理
# =========================


# 六个预测变量

x <- data %>%
  select(
    `CTP score`,
    GDF15,
    CA_199,
    GGT,
    GFR,
    TyG
  )


# 转矩阵

x_matrix <- as.matrix(x)



# =========================
# 4. 结局变量转换
# =========================


# 确保为0/1整数

y_primary <- as.integer(
  data$Primary
)


y_s1 <- as.integer(
  data$`Sensitivity 1`
)


y_s2 <- as.integer(
  data$`Sensitivity 2`
)



# 检查

table(y_primary)

table(y_s1)

table(y_s2)



# =========================
# 5. XGBoost训练函数
# （适用于xgboost 2.x）
# =========================


train_xgb <- function(x, y){
  
  
  # 建立DMatrix
  
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
# 6. 三个RFH-NPT定义分别训练
# =========================


# RFH-NPT ≥2

model_primary <- train_xgb(
  
  x_matrix,
  
  y_primary
  
)



# RFH-NPT ≥1

model_s1 <- train_xgb(
  
  x_matrix,
  
  y_s1
  
)



# RFH-NPT ≥3

model_s2 <- train_xgb(
  
  x_matrix,
  
  y_s2
  
)




# =========================
# 7. 获取预测概率
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



# 查看预测概率

head(
  data[
    ,c(
      "Pred_Primary",
      "Pred_Sensitivity1",
      "Pred_Sensitivity2"
    )
  ]
)




# =====================================================
# 8. 生成Calibration数据
# =====================================================


get_calibration <- function(
    
  outcome,
  prediction,
  model_name
  
){
  
  
  cal <- data.frame(
    
    Observed = outcome,
    
    Predicted = prediction
    
  )
  
  
  
  # 十分位分组
  
  cal$group <- cut(
    
    cal$Predicted,
    
    breaks = quantile(
      cal$Predicted,
      probs = seq(0,1,0.1),
      na.rm = TRUE
    ),
    
    include.lowest = TRUE
    
  )
  
  
  
  result <- cal %>%
    
    group_by(group) %>%
    
    summarise(
      
      Mean_Predicted =
        mean(Predicted),
      
      Mean_Observed =
        mean(Observed),
      
      Number =
        n(),
      
      .groups="drop"
      
    )
  
  
  
  result$Model <- model_name
  
  
  return(result)
  
}




# 三个模型校准数据

cal_df <- rbind(
  
  
  get_calibration(
    
    data$Primary,
    
    data$Pred_Primary,
    
    "RFH-NPT ≥2"
    
  ),
  
  
  
  get_calibration(
    
    data$`Sensitivity 1`,
    
    data$Pred_Sensitivity1,
    
    "RFH-NPT ≥1"
    
  ),
  
  
  
  get_calibration(
    
    data$`Sensitivity 2`,
    
    data$Pred_Sensitivity2,
    
    "RFH-NPT ≥3"
    
  )
  
  
)



# =====================================================
# 9. 绘制Calibration curve
# =====================================================


colors <- c(
  
  "RFH-NPT ≥2"="#D62728",
  
  "RFH-NPT ≥1"="#1F77B4",
  
  "RFH-NPT ≥3"="#2CA02C"
  
)



p_cal <- ggplot(
  
  cal_df,
  
  aes(
    
    x = Mean_Predicted,
    
    y = Mean_Observed,
    
    color = Model
    
  )
  
)+
  
  
  # 理想校准线
  
  geom_abline(
    
    slope = 1,
    
    intercept = 0,
    
    linetype = "dashed",
    
    color = "grey70",
    
    linewidth = 0.8
    
  )+
  
  
  
  # 校准曲线
  
  geom_line(
    
    linewidth = 1.3
    
  )+
  
  
  
  # 校准点
  
  geom_point(
    
    size = 3
    
  )+
  
  
  
  scale_color_manual(
    
    values = colors
    
  )+
  
  
  
  coord_fixed(
    
    ratio = 1
    
  )+
  
  
  
  labs(
    
    x = "Predicted probability",
    
    y = "Observed probability",
    
    title =
      "Calibration of XGBoost models under alternative RFH-NPT thresholds"
    
  )+
  
  
  
  theme_classic(
    
    base_size = 15
    
  )+
  
  
  
  theme(
    
    panel.border =
      element_rect(
        
        color = "black",
        
        fill = NA,
        
        linewidth = 1
        
      ),
    
    
    plot.title =
      element_text(
        
        hjust = 0.5,
        
        face = "bold"
        
      ),
    
    
    legend.position = "bottom"
    
  )



# 显示

print(p_cal)



# =====================================================
# 10. 保存图片
# =====================================================


ggsave(
  
  "XGBoost_RFHNPT_Calibration_curve.pdf",
  
  p_cal,
  
  width = 6,
  
  height = 5
  
)