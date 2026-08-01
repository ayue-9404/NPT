# =========================
# 1. 加载包
# =========================

library(readxl)
library(dplyr)
library(rms)
library(pROC)
library(rmda)
library(ggplot2)



# =========================
# 2. 读取数据
# =========================

sens <- read_excel("六变量敏感性.xlsx")


# 查看变量名

print(names(sens))



# =========================
# 3. 数据整理
# =========================

# 修改变量名

sens <- sens %>%
  rename(
    CTP_score = `CTP score`
  )


# 转换变量类型

sens$Primary <- as.numeric(sens$Primary)

sens$`Sensitivity 1` <- as.numeric(sens$`Sensitivity 1`)

sens$`Sensitivity 2` <- as.numeric(sens$`Sensitivity 2`)

sens$`Sensitivity 3` <- as.numeric(sens$`Sensitivity 3`)



# =========================
# 4. 建立数据分布对象
# =========================

dd <- datadist(sens)

options(datadist="dd")



# =========================
# 5. 建立四个Logistic模型
# =========================


# 主分析：RFH-NPT cutoff ≥2

fit_primary <- lrm(
  
  Primary ~
    
    CTP_score +
    GDF15 +
    CA_199 +
    GGT +
    GFR +
    TyG,
  
  data=sens,
  
  x=TRUE,
  
  y=TRUE
  
)



# 敏感性分析1：RFH-NPT cutoff ≥1


fit_s1 <- lrm(
  
  `Sensitivity 1` ~
    
    CTP_score +
    GDF15 +
    CA_199 +
    GGT +
    GFR +
    TyG,
  
  data=sens,
  
  x=TRUE,
  
  y=TRUE
  
)



# 敏感性分析2：RFH-NPT cutoff ≥3


fit_s2 <- lrm(
  
  `Sensitivity 2` ~
    
    CTP_score +
    GDF15 +
    CA_199 +
    GGT +
    GFR +
    TyG,
  
  data=sens,
  
  x=TRUE,
  
  y=TRUE
  
)



# 敏感性分析3：RFH-NPT cutoff ≥4


fit_s3 <- lrm(
  
  `Sensitivity 3` ~
    
    CTP_score +
    GDF15 +
    CA_199 +
    GGT +
    GFR +
    TyG,
  
  data=sens,
  
  x=TRUE,
  
  y=TRUE
  
)



# =========================
# 6. 预测概率
# =========================


sens$pred_primary <- predict(
  
  fit_primary,
  
  type="fitted"
  
)



sens$pred_s1 <- predict(
  
  fit_s1,
  
  type="fitted"
  
)



sens$pred_s2 <- predict(
  
  fit_s2,
  
  type="fitted"
  
)



sens$pred_s3 <- predict(
  
  fit_s3,
  
  type="fitted"
  
)



# =========================
# 7. ROC分析
# =========================


roc_primary <- roc(
  
  sens$Primary,
  
  sens$pred_primary,
  
  ci=TRUE
  
)



roc_s1 <- roc(
  
  sens$`Sensitivity 1`,
  
  sens$pred_s1,
  
  ci=TRUE
  
)



roc_s2 <- roc(
  
  sens$`Sensitivity 2`,
  
  sens$pred_s2,
  
  ci=TRUE
  
)



roc_s3 <- roc(
  
  sens$`Sensitivity 3`,
  
  sens$pred_s3,
  
  ci=TRUE
  
)



# =========================
# 8. AUC结果
# =========================


auc_table <- data.frame(
  
  RFH_NPT_cutoff=c(
    
    "≥2 (Primary)",
    
    "≥1",
    
    "≥3",
    
    "≥4"
    
  ),
  
  
  AUC=c(
    
    round(as.numeric(auc(roc_primary)),3),
    
    round(as.numeric(auc(roc_s1)),3),
    
    round(as.numeric(auc(roc_s2)),3),
    
    round(as.numeric(auc(roc_s3)),3)
    
  ),
  
  
  CI_lower=c(
    
    round(ci.auc(roc_primary)[1],3),
    
    round(ci.auc(roc_s1)[1],3),
    
    round(ci.auc(roc_s2)[1],3),
    
    round(ci.auc(roc_s3)[1],3)
    
  ),
  
  
  CI_upper=c(
    
    round(ci.auc(roc_primary)[3],3),
    
    round(ci.auc(roc_s1)[3],3),
    
    round(ci.auc(roc_s2)[3],3),
    
    round(ci.auc(roc_s3)[3],3)
    
  )
  
)



auc_table$`AUC (95% CI)` <- paste0(
  
  auc_table$AUC,
  
  " (",
  
  auc_table$CI_lower,
  
  "-",
  
  auc_table$CI_upper,
  
  ")"
  
)



print(auc_table)



write.csv(
  
  auc_table,
  
  "RFH_NPT_cutoff_AUC_table.csv",
  
  row.names=FALSE
  
)
# =========================
# 9. ROC曲线绘制
# =========================


pdf(
  "RFH_NPT_cutoff_ROC.pdf",
  width=8,
  height=7
)


plot(
  
  roc_primary,
  
  col="#D62728",
  
  lwd=3,
  
  main="ROC curves of the XGBoost model under different RFH-NPT cutoff definitions"
  
)



plot(
  
  roc_s1,
  
  col="#1F77B4",
  
  lwd=3,
  
  add=TRUE
  
)



plot(
  
  roc_s2,
  
  col="#2CA02C",
  
  lwd=3,
  
  add=TRUE
  
)



plot(
  
  roc_s3,
  
  col="#9467BD",
  
  lwd=3,
  
  add=TRUE
  
)



abline(
  
  0,
  
  1,
  
  lty=2,
  
  col="grey60"
  
)



# =========================
# AUC + 95% CI legend
# =========================


legend(
  
  "bottomright",
  
  legend=c(
    
    paste0(
      "Primary analysis (≥2): AUC=",
      round(auc(roc_primary),3),
      " (95% CI: ",
      round(ci.auc(roc_primary)[1],3),
      "-",
      round(ci.auc(roc_primary)[3],3),
      ")"
    ),
    
    
    paste0(
      "Sensitivity analysis 1 (≥1): AUC=",
      round(auc(roc_s1),3),
      " (95% CI: ",
      round(ci.auc(roc_s1)[1],3),
      "-",
      round(ci.auc(roc_s1)[3],3),
      ")"
    ),
    
    
    paste0(
      "Sensitivity analysis 2 (≥3): AUC=",
      round(auc(roc_s2),3),
      " (95% CI: ",
      round(ci.auc(roc_s2)[1],3),
      "-",
      round(ci.auc(roc_s2)[3],3),
      ")"
    ),
    
    
    paste0(
      "Sensitivity analysis 3 (≥4): AUC=",
      round(auc(roc_s3),3),
      " (95% CI: ",
      round(ci.auc(roc_s3)[1],3),
      "-",
      round(ci.auc(roc_s3)[3],3),
      ")"
    )
    
  ),
  
  
  col=c(
    
    "#D62728",
    
    "#1F77B4",
    
    "#2CA02C",
    
    "#9467BD"
    
  ),
  
  
  lwd=3,
  
  bty="n",
  
  cex=0.75
  
)



dev.off()





# =========================
# 10. Decision Curve Analysis
# =========================


dca_primary <- decision_curve(
  
  Primary ~ pred_primary,
  
  data=sens,
  
  family="binomial",
  
  thresholds=seq(0,1,0.01),
  
  confidence.intervals=FALSE
  
)



dca_s1 <- decision_curve(
  
  `Sensitivity 1` ~ pred_s1,
  
  data=sens,
  
  family="binomial",
  
  thresholds=seq(0,1,0.01),
  
  confidence.intervals=FALSE
  
)



dca_s2 <- decision_curve(
  
  `Sensitivity 2` ~ pred_s2,
  
  data=sens,
  
  family="binomial",
  
  thresholds=seq(0,1,0.01),
  
  confidence.intervals=FALSE
  
)



dca_s3 <- decision_curve(
  
  `Sensitivity 3` ~ pred_s3,
  
  data=sens,
  
  family="binomial",
  
  thresholds=seq(0,1,0.01),
  
  confidence.intervals=FALSE
  
)



pdf(
  
  "RFH_NPT_cutoff_DCA.pdf",
  
  width=8,
  
  height=7
  
)



plot_decision_curve(
  
  list(
    
    dca_primary,
    
    dca_s1,
    
    dca_s2,
    
    dca_s3
    
  ),
  
  
  curve.names=c(
    
    "Primary analysis (≥2)",
    
    "Sensitivity analysis 1 (≥1)",
    
    "Sensitivity analysis 2 (≥3)",
    
    "Sensitivity analysis 3 (≥4)"
    
  ),
  
  
  col=c(
    
    "#D62728",
    
    "#1F77B4",
    
    "#2CA02C",
    
    "#9467BD"
    
  ),
  
  
  lwd=2,
  
  standardize=TRUE
  
)



dev.off()





# =========================
# 11. Calibration curves
# =========================


cal_primary <- calibrate(
  
  fit_primary,
  
  method="boot",
  
  B=1000
  
)



cal_s1 <- calibrate(
  
  fit_s1,
  
  method="boot",
  
  B=1000
  
)



cal_s2 <- calibrate(
  
  fit_s2,
  
  method="boot",
  
  B=1000
  
)



cal_s3 <- calibrate(
  
  fit_s3,
  
  method="boot",
  
  B=1000
  
)





# 转换数据格式

cal_primary_df <- data.frame(
  
  pred=cal_primary[,1],
  
  obs=cal_primary[,2]
  
)



cal_s1_df <- data.frame(
  
  pred=cal_s1[,1],
  
  obs=cal_s1[,2]
  
)



cal_s2_df <- data.frame(
  
  pred=cal_s2[,1],
  
  obs=cal_s2[,2]
  
)



cal_s3_df <- data.frame(
  
  pred=cal_s3[,1],
  
  obs=cal_s3[,2]
  
)





# =========================
# 12. Calibration绘图
# =========================


cal_plot <- ggplot()+
  
  
  geom_line(
    
    data=cal_primary_df,
    
    aes(
      
      pred,
      
      obs
      
    ),
    
    color="#D62728",
    
    linewidth=1.3
    
  )+
  
  
  
  geom_line(
    
    data=cal_s1_df,
    
    aes(
      
      pred,
      
      obs
      
    ),
    
    color="#1F77B4",
    
    linewidth=1.3
    
  )+
  
  
  
  geom_line(
    
    data=cal_s2_df,
    
    aes(
      
      pred,
      
      obs
      
    ),
    
    color="#2CA02C",
    
    linewidth=1.3
    
  )+
  
  
  
  geom_line(
    
    data=cal_s3_df,
    
    aes(
      
      pred,
      
      obs
      
    ),
    
    color="#9467BD",
    
    linewidth=1.3
    
  )+
  
  
  
  geom_abline(
    
    slope=1,
    
    intercept=0,
    
    linetype="dashed",
    
    color="grey60"
    
  )+
  
  
  
  labs(
    
    x="Predicted probability",
    
    y="Observed probability",
    
    title="Calibration curves under different RFH-NPT cutoff definitions"
    
  )+
  
  
  
  theme_classic(
    
    base_size=15
    
  )+
  
  
  
  theme(
    
    panel.border=element_rect(
      
      color="black",
      
      fill=NA,
      
      linewidth=1
      
    ),
    
    plot.title=element_text(
      
      hjust=0.5,
      
      face="bold"
      
    )
    
  )



ggsave(
  
  "RFH_NPT_cutoff_calibration.pdf",
  
  cal_plot,
  
  width=7,
  
  height=7,
  
  dpi=600
  
)





# =========================
# 13. 保存模型
# =========================


save(
  
  fit_primary,
  
  fit_s1,
  
  fit_s2,
  
  fit_s3,
  
  file="RFH_NPT_cutoff_models.RData"
  
)