library(readxl)
library(ggplot2)
library(ggpubr)

# =========================
# 1. 读取数据
# =========================
df <- read_excel("衰弱.xlsx")

# 确保是数值
df$`Frailty index` <- as.numeric(df$`Frailty index`)
df$PRE_1 <- as.numeric(df$PRE_1)

# =========================
# 2. 分组标签（关键）
# =========================
df$Frailty_group <- factor(
  df$`Frailty index`,
  levels = c(0, 1, 2),
  labels = c(
    "Robust (≤0.10)",
    "Pre-frail (0.10–0.25)",
    "Frail (≥0.25)"
  )
)

# =========================
# 3. y轴范围
# =========================
y_max <- max(df$PRE_1, na.rm = TRUE)

# =========================
# 4. 作图
# =========================
p <- ggplot(df, aes(x = Frailty_group, y = PRE_1, fill = Frailty_group)) +
  
  geom_boxplot(
    width = 0.6,
    alpha = 0.35,
    linewidth = 0.8,
    color = "grey25",
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.45,
    shape = 21,
    color = "black"
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white",
    color = "black"
  ) +
  
  # ⭐ 两两比较（Wilcoxon）
  stat_compare_means(
    comparisons = list(
      c("Robust (≤0.10)", "Pre-frail (0.10–0.25)"),
      c("Robust (≤0.10)", "Frail (≥0.25)"),
      c("Pre-frail (0.10–0.25)", "Frail (≥0.25)")
    ),
    method = "wilcox.test",
    label = "p.signif",
    size = 4,
    step.increase = 0.08
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.8)
  ) +
  
  labs(
    x = "Frailty category",
    y = "PRE_1"
  ) +
  
  expand_limits(y = y_max * 1.45)

p