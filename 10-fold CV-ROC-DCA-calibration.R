# ================================
# 0️⃣ 加载包
# ================================
library(readxl)
library(xgboost)
library(pROC)
library(ggplot2)
library(dplyr)
library(rmda)

set.seed(123)

# ================================
# 1️⃣ 读取数据 + 清洗列名
# ================================
df <- read_excel("训练集.xlsx")
names(df) <- make.names(names(df))

# ================================
# 2️⃣ 自动匹配变量
# ================================
ca_var <- grep("^CA", names(df), value = TRUE)[1]

features <- c("CTP.score", "GDF15", ca_var, "GGT", "GFR", "TyG")

# ================================
# 3️⃣ 数据预处理
# ================================
df[, features] <- lapply(df[, features], as.numeric)
df$RFH.NPT.score <- as.numeric(df$RFH.NPT.score)

df <- na.omit(df)

X <- as.matrix(df[, features])
y <- df$RFH.NPT.score

# ================================
# 4️⃣ 10折交叉验证（生成无偏预测）
# ================================
k <- 10
folds <- sample(rep(1:k, length.out = nrow(X)))

pred_all <- rep(NA, nrow(X))
auc_cv <- c()

for (i in 1:k) {
  
  train_idx <- which(folds != i)
  test_idx  <- which(folds == i)
  
  model <- xgb.train(
    params = list(objective = "binary:logistic"),
    data = xgb.DMatrix(X[train_idx, ], label = y[train_idx]),
    nrounds = 100,
    verbose = 0
  )
  
  pred <- predict(model, xgb.DMatrix(X[test_idx, ]))
  
  pred_all[test_idx] <- pred
  
  auc_cv[i] <- as.numeric(auc(roc(y[test_idx], pred, quiet = TRUE)))
}

# ================================
# ✅ 输出AUC
# ================================
cat("Mean AUC:", round(mean(auc_cv), 3), "\n")
cat("SD:", round(sd(auc_cv), 3), "\n")

# ================================
# 5️⃣ ROC曲线（主图，SCI风格）
# ================================
roc_obj <- roc(y, pred_all)

roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#D7191C", linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  annotate("text",
           x = 0.6, y = 0.2,
           label = paste0("AUC = ", round(auc(roc_obj), 3)),
           size = 5) +
  theme_classic(base_size = 14) +
  labs(title = "ROC Curve (10-fold CV)",
       x = "False Positive Rate",
       y = "True Positive Rate")

print(p_roc)

# ================================
# 6️⃣ DCA（用CV预测）
# ================================
df_dca <- data.frame(outcome = y, pred = pred_all)

dca_model <- decision_curve(
  outcome ~ pred,
  data = df_dca,
  family = binomial(link = "logit"),
  thresholds = seq(0, 1, by = 0.01)
)

plot_decision_curve(
  dca_model,
  curve.names = "XGBoost",
  xlab = "Threshold Probability",
  ylab = "Net Benefit",
  legend.position = "topright"
)

# ================================
# 7️⃣ 校准曲线（Calibration）
# ================================
df_cal <- data.frame(pred = pred_all, y = y)

df_cal <- df_cal %>%
  mutate(group = ntile(pred, 10)) %>%
  group_by(group) %>%
  summarise(
    mean_pred = mean(pred),
    obs = mean(y),
    .groups = "drop"
  )

p_cal <- ggplot(df_cal, aes(x = mean_pred, y = obs)) +
  geom_point(size = 3, color = "#2C7BB6") +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  theme_classic(base_size = 14) +
  labs(title = "Calibration Curve (10-fold CV)",
       x = "Predicted Probability",
       y = "Observed Probability")

print(p_cal)