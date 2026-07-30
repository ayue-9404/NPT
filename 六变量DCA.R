# =====================================================
# D. Decision Curve Analysis (DCA)
# XGBoost sensitivity analysis under alternative RFH-NPT thresholds
# =====================================================


# =========================
# 1. 加载包
# =========================

library(rmda)



# =========================
# 2. 数据整理
# =========================

# rmda要求结局为0/1

data$Primary <- as.numeric(data$Primary)

data$`Sensitivity 1` <- as.numeric(data$`Sensitivity 1`)

data$`Sensitivity 2` <- as.numeric(data$`Sensitivity 2`)



# =========================
# 3. 三个XGBoost模型DCA
# =========================



# RFH-NPT ≥2

dca_primary <- decision_curve(
  
  Primary ~ Pred_Primary,
  
  data = data,
  
  family = binomial(link="logit"),
  
  thresholds = seq(
    0,
    1,
    by = 0.01
  ),
  
  confidence.intervals = FALSE,
  
  study.design = "cohort"
  
)




# RFH-NPT ≥1


dca_s1 <- decision_curve(
  
  `Sensitivity 1` ~ Pred_Sensitivity1,
  
  data = data,
  
  family = binomial(link="logit"),
  
  thresholds = seq(
    0,
    1,
    by = 0.01
  ),
  
  confidence.intervals = FALSE,
  
  study.design = "cohort"
  
)





# RFH-NPT ≥3


dca_s2 <- decision_curve(
  
  `Sensitivity 2` ~ Pred_Sensitivity2,
  
  data = data,
  
  family = binomial(link="logit"),
  
  thresholds = seq(
    0,
    1,
    by = 0.01
  ),
  
  confidence.intervals = FALSE,
  
  study.design = "cohort"
  
)




# =========================
# 4. 绘制DCA
# =========================


plot_decision_curve(
  
  list(
    
    dca_primary,
    
    dca_s1,
    
    dca_s2
    
  ),
  
  
  curve.names = c(
    
    "RFH-NPT ≥2",
    
    "RFH-NPT ≥1",
    
    "RFH-NPT ≥3"
    
  ),
  
  
  colors = c(
    
    "#D62728",
    
    "#1F77B4",
    
    "#2CA02C"
    
  ),
  
  
  standardize = FALSE,
  
  
  confidence.intervals = FALSE,
  
  
  legend.position = "bottom"
  
)