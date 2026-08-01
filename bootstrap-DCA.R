library(xgboost)
library(rmda)

set.seed(123)

# ================================
# 生成 OOB 预测
# ================================
pred_oob <- rep(NA, nrow(X))

for (i in 1:200) {   # 不用1000，200够了
  
  idx <- sample(1:nrow(X), replace = TRUE)
  oob_idx <- setdiff(1:nrow(X), unique(idx))
  
  if (length(oob_idx) < 10) next
  
  model <- xgb.train(
    params = list(objective = "binary:logistic"),
    data = xgb.DMatrix(X[idx, ], label = y[idx]),
    nrounds = 100,
    verbose = 0
  )
  
  pred <- predict(model, xgb.DMatrix(X[oob_idx, ]))
  
  pred_oob[oob_idx] <- pred
}

# 去掉NA
df_dca <- data.frame(
  outcome = y,
  pred = pred_oob
)

df_dca <- na.omit(df_dca)

# ================================
# DCA
# ================================
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