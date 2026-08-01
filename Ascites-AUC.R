# =========================
# 0. 加载包
# =========================
library(readxl)
library(pROC)
library(ggplot2)

# =========================
# 1. 读取数据
# =========================
df <- read_excel("腹水.xlsx")

# =========================
# 2. 数据处理
# =========================
df$PRE_1 <- as.numeric(df$PRE_1)
df$Ascites <- as.numeric(df$Ascites)

# 确保结局是0/1
table(df$Ascites)

# =========================
# 3. ROC分析
# =========================
roc_obj <- roc(
  response = df$Ascites,
  predictor = df$PRE_1,
  ci = TRUE
)

auc_val <- auc(roc_obj)
ci_val  <- ci.auc(roc_obj)

print(auc_val)
print(ci_val)

# =========================
# 4. 最佳cutoff（Youden）
# =========================
coords(
  roc_obj,
  "best",
  ret = c("threshold", "sensitivity", "specificity"),
  best.method = "youden"
)

# =========================
# 5. ROC数据
# =========================
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

# =========================
# 6. SCI优化ROC图
# =========================
p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  
  # 面积填充
  geom_area(
    fill = "#4DBBD5",
    alpha = 0.20
  ) +
  
  # ROC曲线
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.4
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
  
  # =========================
# ⭐关键：解决横轴压缩
# =========================
scale_x_continuous(
  limits = c(0, 1),
  expand = c(0, 0)
) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  
  # =========================
# SCI风格
# =========================
theme_classic(base_size = 14) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.9)
  ) +
  
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
    title = "Prediction of Ascites"
  )

# 显示图
p

# =========================
# 7. 保存PDF（横向加宽）
# =========================
ggsave(
  "ROC_Ascites_SCI.pdf",
  p,
  width = 7.5,
  height = 5.5,
  dpi = 300
)