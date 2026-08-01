# ================================
# 加载包
# ================================
library(xgboost)
library(pROC)

set.seed(123)

n_boot <- 1000

# 存储
tpr_list <- list()
mean_fpr <- seq(0, 1, length = 100)

# ================================
# 画空图
# ================================
plot(0, 0, type = "n",
     xlim = c(0,1), ylim = c(0,1),
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "Bootstrap ROC Curves (XGBoost)")

abline(0,1,lty=2,col="gray")

# ================================
# Bootstrap
# ================================
for (i in 1:n_boot) {
  
  idx <- sample(1:nrow(X), replace = TRUE)
  
  X_boot <- X[idx, ]
  y_boot <- y[idx]
  
  dtrain <- xgb.DMatrix(data = X_boot, label = y_boot)
  
  model <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc"
    ),
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  pred <- predict(model, xgb.DMatrix(X))
  
  roc_obj <- roc(y, pred, quiet = TRUE)
  
  # 提取曲线
  fpr <- 1 - roc_obj$specificities
  tpr <- roc_obj$sensitivities
  
  # 插值到统一坐标
  tpr_interp <- approx(fpr, tpr, xout = mean_fpr)$y
  
  tpr_list[[i]] <- tpr_interp
  
  # 画每一条（浅灰色）
  lines(mean_fpr, tpr_interp, col = rgb(0.7,0.7,0.7,0.2))
}

# ================================
# 平均ROC
# ================================
tpr_matrix <- do.call(rbind, tpr_list)
mean_tpr <- colMeans(tpr_matrix, na.rm = TRUE)

# 画平均曲线（粗线）
lines(mean_fpr, mean_tpr, col = "red", lwd = 3)

# ================================
# AUC（平均）
# ================================
auc_values <- apply(tpr_matrix, 1, function(tpr){
  trapz <- sum(diff(mean_fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
  return(trapz)
})

mean_auc <- mean(auc_values, na.rm = TRUE)

legend("bottomright",
       legend = paste0("Mean AUC = ", round(mean_auc, 3)),
       col = "red",
       lwd = 3,
       bty = "n")