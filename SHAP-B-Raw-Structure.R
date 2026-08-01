
# =========================================================
# 🟢 SHAP Parallel Coordinates Plot（B图：真实结构版）
# ✔ 不做任何增强
# ✔ 不破坏SHAP分布
# ✔ 用于判断是否存在真实倒三角
# =========================================================

library(readxl)
library(xgboost)
library(fastshap)
library(GGally)
library(ggplot2)

# =========================
# 1. 读取数据
# =========================
data <- read_excel("shap.xlsx")

names(data) <- c(
  "RFH_NPT_score",
  "CTP_score",
  "GDF15",
  "CA199",
  "GGT",
  "GFR",
  "TyG"
)

xvars <- c("CTP_score", "GDF15", "CA199", "GGT", "GFR", "TyG")

data <- na.omit(data)
data$RFH_NPT_score <- as.numeric(data$RFH_NPT_score)

# =========================
# 2. XGBoost模型
# =========================
X <- as.matrix(data[, xvars])
y <- data$RFH_NPT_score

dtrain <- xgb.DMatrix(X, label = y)

set.seed(1234)

xgb_model <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 5,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain,
  nrounds = 250,
  verbose = 0
)

# =========================
# 3. SHAP计算
# =========================
pred_fun <- function(object, newdata) {
  predict(object, xgb.DMatrix(as.matrix(newdata)))
}

shap_df <- fastshap::explain(
  object = xgb_model,
  X = data[, xvars],
  pred_wrapper = pred_fun,
  nsim = 100
)

shap_df <- as.data.frame(shap_df)

# =========================
# 4. SHAP重要性排序（仅排序，不改结构）
# =========================
feature_order <- names(sort(colMeans(abs(shap_df)), decreasing = TRUE))
ordered_features <- rev(feature_order)

# =========================
# 5. 🔴 B图核心：原始SHAP（不标准化、不变换）
# =========================
plot_B <- shap_df[, ordered_features]

# 添加预测风险（仅用于颜色）
plot_B$Risk <- predict(xgb_model, dtrain)

# =========================
# 6. 绘图函数
# =========================
p_B <- GGally::ggparcoord(
  data = plot_B,
  columns = 1:(ncol(plot_B) - 1),
  groupColumn = "Risk",
  alphaLines = 0.28,
  scale = "globalminmax",
  showPoints = FALSE
) +
  coord_flip() +
  
  # 连续风险渐变
  scale_color_gradientn(
    colours = c(
      "#2C7BB6",  # low risk
      "#FFFFBF",  # mid
      "#D7191C"   # high risk
    ),
    values = c(0, 0.5, 1),
    limits = c(0, 1),
    name = "Predicted Risk"
  ) +
  
  labs(
    title = "SHAP Parallel Coordinates Plot (Raw Structure)",
    subtitle = "True SHAP Distribution Without Transformation",
    x = "SHAP Value",
    y = NULL
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 13,
      color = "gray40"
    ),
    legend.position = "top"
  )

# =========================
# 7. 输出
# =========================
print(p_B)

# =========================
# 8. 保存
# =========================
ggsave(
  "SHAP_B_Raw_Structure.pdf",
  p_B,
  width = 7,
  height = 12
)

ggsave(
  "SHAP_B_Raw_Structure.tiff",
  p_B,
  width = 7,
  height = 12,
  dpi = 600,
  compression = "lzw"
)

# =========================
# 9. 结构诊断（判断倒三角强度）
# =========================
cat("\n=====================\n")
cat("SHAP结构诊断（B图）\n")

sd_check <- apply(abs(shap_df), 2, sd)
print(sd_check)

cat("\n说明：\n")
cat("- 这是未处理真实SHAP结构\n")
cat("- 用于判断是否存在自然倒三角\n")
cat("=====================\n")