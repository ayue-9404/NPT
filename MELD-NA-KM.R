# ================================
# 0️⃣ 加载包
# ================================
library(readxl)
library(survival)
library(survminer)
library(dplyr)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel(file.choose())   # 选择“MELD-Na.xlsx”

# 查看列名
print(names(df))

# ================================
# 2️⃣ 数据整理
# ================================
df <- df %>%
  mutate(
    time   = as.numeric(time),
    status = as.numeric(status),
    MELD   = as.numeric(`MELD-Na score`)
  ) %>%
  filter(!is.na(time) & !is.na(status) & !is.na(MELD))

# ================================
# 3️⃣ 按中位数分组（关键🔥）
# ================================
cutoff <- median(df$MELD, na.rm = TRUE)

df$group <- ifelse(df$MELD <= cutoff, "Low MELD-Na", "High MELD-Na")
df$group <- factor(df$group, levels = c("Low MELD-Na", "High MELD-Na"))

table(df$group)

# ================================
# 4️⃣ KM模型
# ================================
fit <- survfit(Surv(time, status) ~ group, data = df)

# ================================
# 5️⃣ Cox模型
# ================================
cox_model <- coxph(Surv(time, status) ~ group, data = df)
cox_summary <- summary(cox_model)

hr    <- round(cox_summary$coefficients[,"exp(coef)"], 2)
lower <- round(cox_summary$conf.int[,"lower .95"], 2)
upper <- round(cox_summary$conf.int[,"upper .95"], 2)
pval  <- signif(cox_summary$coefficients[,"Pr(>|z|)"], 3)

# ================================
# ⭐ Log-rank P
# ================================
logrank <- survdiff(Surv(time, status) ~ group, data = df)
log_p <- signif(1 - pchisq(logrank$chisq, df = 1), 3)

# ================================
# 6️⃣ HR文本
# ================================
label_text <- paste0(
  "High vs Low: HR=", hr[1],
  " (", lower[1], "-", upper[1], "), P=", pval[1]
)

# ================================
# 7️⃣ KM图
# ================================
p <- ggsurvplot(
  fit,
  data = df,
  
  conf.int = TRUE,
  pval = FALSE,
  
  palette = c("#2C7BB6", "#D7191C"),
  
  xlab = "Time (days)",
  ylab = "Survival probability",
  
  risk.table = TRUE,
  risk.table.height = 0.28,
  risk.table.col = "strata",
  
  legend.title = "",
  legend = "top",
  
  ylim = c(0, 1.05),
  
  ggtheme = theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line()
    )
)

# ================================
# 8️⃣ 添加Log-rank P
# ================================
p$plot <- p$plot +
  annotate(
    "text",
    x = max(df$time) * 0.05,
    y = 0.45,
    label = paste0("Log-rank P = ", log_p),
    size = 4.2,
    hjust = 0
  )

# ================================
# 9️⃣ 添加HR
# ================================
p$plot <- p$plot +
  annotate(
    "text",
    x = max(df$time) * 0.05,
    y = 0.20,
    label = label_text,
    size = 4.2,
    hjust = 0
  )

# ================================
# 🔟 防裁切
# ================================
p$plot <- p$plot +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 20, 10, 10))

# ================================
# 1️⃣1️⃣ 输出
# ================================
p