# =========================
# 1. 加载包
# =========================
library(readxl)
library(pROC)
library(reshape2)
library(ggplot2)

# =========================
# 2. 读取数据
# =========================
df <- read_excel("delong训练集.xlsx")

# =========================
# 3. ROC计算
# =========================
roc_list <- list(
  ModA = roc(df$RFH_NPT_score, df$ModA),
  ModB = roc(df$RFH_NPT_score, df$ModB),
  ModC = roc(df$RFH_NPT_score, df$ModC),
  ModD = roc(df$RFH_NPT_score, df$ModD),
  ModE = roc(df$RFH_NPT_score, df$ModE),
  ModF = roc(df$RFH_NPT_score, df$ModF)
)

# =========================
# 4. DeLong P值矩阵
# =========================
n <- length(roc_list)

p_matrix <- matrix(NA, n, n)
rownames(p_matrix) <- names(roc_list)
colnames(p_matrix) <- names(roc_list)

for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      test <- roc.test(roc_list[[i]], roc_list[[j]], method = "delong")
      p_matrix[i, j] <- test$p.value
    }
  }
}

# 对称补全
p_matrix[lower.tri(p_matrix)] <- t(p_matrix)[lower.tri(p_matrix)]

# =========================
# 5. -log10(P)
# =========================
p_log <- -log10(p_matrix)

# =========================
# 6. 长格式 + upper triangle
# =========================
p_df <- melt(p_log, na.rm = TRUE)
p_df <- p_df[as.numeric(p_df$Var1) < as.numeric(p_df$Var2), ]

# =========================
# 7. 画图对象（关键）
# =========================
p <- ggplot(p_df, aes(x = Var1, y = Var2, fill = value)) +
  
  geom_tile(color = "white", width = 0.85, height = 0.85) +
  
  geom_text(aes(label = sprintf("%.1f", value)), size = 3) +
  
  scale_fill_gradient2(
    low = "#313695",
    mid = "white",
    high = "#A50026",
    midpoint = median(p_df$value, na.rm = TRUE),
    name = "-log10(P)"
  ) +
  
  scale_x_discrete(expand = expansion(add = 1)) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    
    plot.margin = margin(15, 40, 15, 15),
    
    text = element_text(family = "Arial")
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "DeLong Test Significance (-log10 P)",
    x = "",
    y = ""
  )

# =========================
# 8. 保存为PDF（SCI推荐🔥）
# =========================
ggsave(
  filename = "DeLong_PanelA.pdf",
  plot = p,
  width = 10,
  height = 8,
  device = cairo_pdf   # ⭐关键：矢量输出更稳定
)