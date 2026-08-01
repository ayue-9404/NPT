library(readxl)
library(survival)
library(survminer)
library(dplyr)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("CTP.xlsx")   # 改成你的文件名

# ================================
# 2️⃣ 分组（CTP A/B/C）
# ================================
df$group <- factor(df$`CTP score`,
                   levels = c(1,2,3),
                   labels = c("CTP A", "CTP B", "CTP C"))

table(df$group)

# ================================
# 3️⃣ KM模型
# ================================
fit <- survfit(Surv(time, status) ~ group, data = df)

# ================================
# 4️⃣ Cox模型
# ================================
cox_model <- coxph(Surv(time, status) ~ group, data = df)
cox_summary <- summary(cox_model)

hr <- round(cox_summary$coefficients[,"exp(coef)"], 2)
lower <- round(cox_summary$conf.int[,"lower .95"], 2)
upper <- round(cox_summary$conf.int[,"upper .95"], 2)

log_p <- signif(cox_summary$logtest["pvalue"], 3)

# ================================
# ⭐ HR文本（两组比较）
# ================================
label_text <- paste0(
  "CTP B vs A: HR=", hr[1],
  " (", lower[1], "-", upper[1], ")\n",
  "CTP C vs A: HR=", hr[2],
  " (", lower[2], "-", upper[2], ")"
)

# ================================
# 5️⃣ KM图（关闭自动p值🔥）
# ================================
p <- ggsurvplot(
  fit,
  data = df,
  
  conf.int = TRUE,
  pval = FALSE,   # ⭐关闭自动P值
  
  palette = c("#2C7BB6", "#FDAE61", "#D7191C"),
  
  xlab = "Time (days)",
  ylab = "Overall Survival / PFS",
  
  risk.table = TRUE,
  risk.table.height = 0.28,
  risk.table.fontsize = 4.2,
  risk.table.col = "strata",
  
  legend.title = "",
  legend = "top",
  
  ylim = c(0, 1.05),
  
  ggtheme = theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12),
      legend.title = element_blank()
    )
)

# ================================
# 6️⃣ ⭐P值（手动控制位置）
# ================================
p$plot <- p$plot +
  annotate(
    "text",
    x = max(df$time, na.rm = TRUE) * 0.05,
    y = 0.40,   # ⭐P值位置（可调）
    label = paste0("Log-rank P = ", log_p),
    size = 4.2,
    hjust = 0
  )

# ================================
# 7️⃣ ⭐HR（手动控制位置）
# ================================
p$plot <- p$plot +
  annotate(
    "text",
    x = max(df$time, na.rm = TRUE) * 0.05,
    y = 0.16,   # ⭐HR位置（可调）
    label = label_text,
    size = 4.1,
    hjust = 0,
    lineheight = 1.2
  )

# ================================
# 8️⃣ 防裁切 + 留白
# ================================
p$plot <- p$plot +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 15, 10, 10))

# ================================
# 9️⃣ 输出
# ================================
p