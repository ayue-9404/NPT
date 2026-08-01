# ================================
# 0️⃣ 加载包
# ================================
library(readxl)
library(xgboost)
library(pROC)
library(ggplot2)
library(scales)

set.seed(123)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("训练集.xlsx")

# 清洗列名（关键）
names(df) <- make.names(names(df))

# ================================
# 2️⃣ 自动匹配变量
# ================================
ca_var <- grep("^CA", names(df), value = TRUE)[1]

features <- c("CTP.score", "GDF15", ca_var, "GGT", "GFR", "TyG")

print(features)

# ================================
# 3️⃣ 数据处理
# ================================
df[, features] <- lapply(df[, features], as.numeric)
df$RFH.NPT.score <- as.numeric(df$RFH.NPT.score)

df <- na.omit(df)

X <- as.matrix(df[, features])
y <- df$RFH.NPT.score

# ================================
# 4️⃣ 10折交叉验证（生成roc_list）
# ================================
k <- 10
folds <- sample(rep(1:k, length.out = nrow(X)))

roc_list <- list()

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
  
  roc_list[[i]] <- roc(y[test_idx], pred, quiet = TRUE)
}

# ================================
# 5️⃣ 提取ROC数据 + AUC
# ================================
roc_df <- data.frame()
auc_labels <- c()
fold_colors <- hue_pal()(k)

for(i in 1:k){
  
  roc_obj <- roc_list[[i]]
  
  auc_val <- auc(roc_obj)
  ci_val <- ci.auc(roc_obj)
  
  auc_labels[i] <- paste0(
    "Fold", i,
    ": AUC=", round(auc_val,3),
    " (95%CI: ", round(ci_val[1],3), "-", round(ci_val[3],3), ")"
  )
  
  temp <- data.frame(
    TPR = roc_obj$sensitivities,
    FPR = 1 - roc_obj$specificities,
    Fold = paste0("Fold", i)
  )
  
  roc_df <- rbind(roc_df, temp)
}

# ================================
# 6️⃣ 文字和颜色块位置
# ================================
y_bottom <- seq(0.02, 0.42, length.out = k)

text_df <- data.frame(
  x = rep(0.33, k),
  y = y_bottom,
  label = auc_labels,
  Fold = paste0("Fold", 1:k)
)

tile_df <- data.frame(
  x = 0.30,
  y = y_bottom,
  Fold = paste0("Fold", 1:k),
  width = 0.015,
  height = 0.012
)

# ================================
# 7️⃣ 绘图（SCI风格）
# ================================
p <- ggplot(roc_df, aes(x=FPR, y=TPR, color=Fold)) +
  
  geom_line(linewidth=1) +
  
  geom_abline(linetype="dashed", color="grey40") +
  
  geom_tile(data=tile_df,
            aes(x=x, y=y, fill=Fold, width=width, height=height),
            inherit.aes = FALSE) +
  
  geom_text(data=text_df,
            aes(x=x, y=y, label=label),
            inherit.aes = FALSE,
            hjust=0, size=3, color="black") +
  
  labs(title="10-Fold Cross-validated ROC Curve",
       x="1 - Specificity",
       y="Sensitivity") +
  
  coord_fixed(ratio=1) +
  
  scale_color_manual(values = setNames(fold_colors, paste0("Fold",1:k))) +
  scale_fill_manual(values = setNames(fold_colors, paste0("Fold",1:k))) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill="white", color="black"),
    panel.grid = element_blank(),
    panel.border = element_rect(fill=NA, color="black", linewidth=1),
    legend.position = "none",
    plot.title = element_text(hjust=0.5, face="bold")
  )

print(p)

# ================================
# 8️⃣ 输出平均AUC（用于论文）
# ================================
auc_values <- sapply(roc_list, auc)

cat("Mean AUC:", round(mean(auc_values), 3), "\n")
cat("SD:", round(sd(auc_values), 3), "\n")