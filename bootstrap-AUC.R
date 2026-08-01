# ================================
# 0️⃣ 加载包（没有就先install.packages）
# ================================
library(readxl)
library(xgboost)
library(pROC)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("训练集.xlsx")

# ================================
# 2️⃣ 清洗列名（关键！）
# ================================
names(df) <- make.names(names(df))

# ================================
# 3️⃣ 数据预处理
# ================================
# 结局变量
df$RFH.NPT.score <- as.numeric(df$RFH.NPT.score)

# 自变量
features <- c("CTP.score", "GDF15", "CA_199", "GGT", "GFR", "TyG")

# 确保都是数值型
df[, features] <- lapply(df[, features], as.numeric)

# 删除缺失值（很重要）
df <- na.omit(df)

# ================================
# 4️⃣ 构建X和y
# ================================
X <- as.matrix(df[, features])
y <- df$RFH.NPT.score

# 检查
cat("样本量:", nrow(df), "\n")
print(table(y))

# ================================
# 5️⃣ Bootstrap + XGBoost
# ================================
set.seed(123)

n_boot <- 1000
auc_values <- numeric(n_boot)

for (i in 1:n_boot) {
  
  # Bootstrap抽样
  idx <- sample(1:nrow(df), replace = TRUE)
  
  X_boot <- X[idx, ]
  y_boot <- y[idx]
  
  dtrain <- xgb.DMatrix(data = X_boot, label = y_boot)
  
  # XGBoost模型（你可以换成最优参数）
  model <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 3,
      eta = 0.1
    ),
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # 在原始数据上预测（关键）
  pred <- predict(model, xgb.DMatrix(X))
  
  # 计算AUC
  roc_obj <- roc(y, pred, quiet = TRUE)
  auc_values[i] <- as.numeric(auc(roc_obj))
}

# ================================
# 6️⃣ 结果输出
# ================================
mean_auc <- mean(auc_values)
ci <- quantile(auc_values, c(0.025, 0.975))

cat("\n===== Bootstrap结果 =====\n")
cat("Mean AUC:", round(mean_auc, 3), "\n")
cat("95% CI:", round(ci[1], 3), "-", round(ci[2], 3), "\n")

# ================================
# 7️⃣ 画图
# ================================
hist(auc_values,
     breaks = 30,
     main = "Bootstrap AUC Distribution (XGBoost)",
     xlab = "AUC")

abline(v = mean_auc, lwd = 2, lty = 2)

# ================================
# 8️⃣ 保存结果（可选）
# ================================
write.csv(auc_values, "bootstrap_auc_1000.csv", row.names = FALSE)