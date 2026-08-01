library(readxl)
library(ggplot2)
library(ggpubr)
library(ggsignif)

# =========================
# 1. 读取数据
# =========================
df <- read_excel("门脉高压.xlsx")

# 重命名变量
names(df)[names(df) == "Portal Hypertension"] <- "PH"

# 转换数值型
df$PRE_1 <- as.numeric(df$PRE_1)
df$PH <- as.numeric(df$PH)

# =========================
# 2. 分组
# =========================
df$PH_group <- factor(
  df$PH,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# =========================
# 3. y轴最大值
# =========================
y_max <- max(df$PRE_1, na.rm = TRUE)

# =========================
# 4. 绘图（SCI水彩风）
# =========================
p <- ggplot(df, aes(x = PH_group, y = PRE_1, fill = PH_group)) +
  
  geom_boxplot(
    width = 0.55,
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
  
  # =========================
# 显著性检验
# =========================
geom_signif(
  comparisons = list(c("No", "Yes")),
  test = "wilcox.test",
  map_signif_level = TRUE,
  y_position = y_max * 1.05,
  tip_length = 0.01,
  textsize = 5
) +
  
  # =========================
# 主题
# =========================
theme_classic(base_size = 14) +
  
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.8),
    panel.grid = element_blank()
  ) +
  
  labs(
    x = "Portal hypertension",
    y = "Predicted probability"
  ) +
  
  expand_limits(y = y_max * 1.35)

# 显示图形
p

# =========================
# 5. 保存PDF
# =========================
ggsave(
  "Portal_Hypertension_PRE1_Boxplot.pdf",
  p,
  width = 5,
  height = 6,
  dpi = 300
)