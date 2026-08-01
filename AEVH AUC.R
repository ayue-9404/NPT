library(readxl)
library(pROC)
library(ggplot2)

# =========================
# 1. 读取数据
# =========================
df <- read_excel("食管胃底静脉曲张.xlsx")

# =========================
# 2. 数据处理
# =========================
df$PRE_1 <- as.numeric(df$PRE_1)
df$AEVH <- as.numeric(df$AEVH)

# 检查0/1分布（很重要）
table(df$AEVH)

# =========================
# 3. ROC分析
# =========================
roc_obj <- roc(
  response = df$AEVH,
  predictor = df$PRE_1,
  ci = TRUE
)

auc_val <- auc(roc_obj)
ci_val <- ci.auc(roc_obj)

print(auc_val)
print(ci_val)

# =========================
# 4. ROC数据
# =========================
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

roc_df <- na.omit(roc_df)

# =========================
# 5. SCI ROC图（标准美化版）
# =========================
p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  
  # 曲线
  geom_line(color = "#2C7FB8", linewidth = 1.4) +
  
  # 填充面积
  geom_ribbon(
    aes(ymin = 0, ymax = TPR),
    fill = "#4DBBD5",
    alpha = 0.20
  ) +
  
  # 对角线
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  # AUC标注
  annotate(
    "text",
    x = 0.65,
    y = 0.20,
    label = paste0(
      "AUC = ", round(auc_val, 3),
      "\n95% CI: ",
      round(ci_val[1], 3), "-",
      round(ci_val[3], 3)
    ),
    size = 5,
    fontface = "bold"
  ) +
  
  # 坐标优化（避免压缩）
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  # SCI风格
  theme_classic(base_size = 14) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.9)
  ) +
  
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
    title = "ROC of Esophagogastric Varices"
  )

# 显示图
p

# =========================
# 6. 保存PDF
# =========================
ggsave(
  "ROC_AEVH_SCI.pdf",
  p,
  width = 7.5,
  height = 5.5,
  dpi = 300
)