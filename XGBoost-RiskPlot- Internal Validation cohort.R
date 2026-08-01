# =========================================================
# 🔥 Nature / TCGA 风格升级版
# XGBoost Risk Stratification Plot
# 内容不变，仅提升SCI美观度
# =========================================================

# =========================
# 1️⃣ 加载包
# =========================
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)

# =========================
# 2️⃣ 读取数据
# =========================
df <- read_excel("验证集概率.xlsx")

# =========================
# 3️⃣ 风险排序
# =========================
df <- df %>%
  arrange(pred_prob)

df$Index <- 1:nrow(df)

# =========================
# 4️⃣ 三风险分层
# =========================
q <- quantile(df$pred_prob,
              probs = c(0.33, 0.67))

df$risk_group <- cut(
  df$pred_prob,
  breaks = c(-Inf,
             q[1],
             q[2],
             Inf),
  labels = c("Low risk",
             "Medium risk",
             "High risk")
)

cut1 <- max(df$Index[df$risk_group == "Low risk"])
cut2 <- max(df$Index[df$risk_group == "Medium risk"])

# =========================
# 5️⃣ SCI高级配色
# =========================
risk_cols <- c(
  "Low risk" = "#3C5488",
  "Medium risk" = "#F2A654",
  "High risk" = "#C53A32"
)

outcome_cols <- c(
  "0" = "#cbdef3",
  "1" = "#f59790"
)

# =========================
# 6️⃣ 上图：
# Risk score distribution
# =========================
p1 <- ggplot(df,
             aes(Index,
                 pred_prob,
                 color = risk_group)) +
  
  # 风险背景
  annotate("rect",
           xmin = 0,
           xmax = cut1,
           ymin = -Inf,
           ymax = Inf,
           fill = "#3C5488",
           alpha = 0.05) +
  
  annotate("rect",
           xmin = cut1,
           xmax = cut2,
           ymin = -Inf,
           ymax = Inf,
           fill = "#F2A654",
           alpha = 0.05) +
  
  annotate("rect",
           xmin = cut2,
           xmax = max(df$Index),
           ymin = -Inf,
           ymax = Inf,
           fill = "#C53A32",
           alpha = 0.05) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.95
  ) +
  
  geom_point(
    size = 2.5,
    alpha = 0.95
  ) +
  
  scale_color_manual(values = risk_cols) +
  
  geom_vline(
    xintercept = cut1,
    linetype = 2,
    linewidth = 0.7,
    color = "grey55"
  ) +
  
  geom_vline(
    xintercept = cut2,
    linetype = 2,
    linewidth = 0.7,
    color = "grey55"
  ) +
  
  annotate(
    "text",
    x = cut1 / 2,
    y = 1.05,
    label = "Low risk",
    color = "#3C5488",
    size = 5.2,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = (cut1 + cut2) / 2,
    y = 1.05,
    label = "Medium risk",
    color = "#D98B2B",
    size = 5.2,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = (cut2 + max(df$Index)) / 2,
    y = 1.05,
    label = "High risk",
    color = "#C53A32",
    size = 5.2,
    fontface = "bold"
  ) +
  
  coord_cartesian(ylim = c(0, 1.08)) +
  
  labs(
    y = "Risk score",
    x = NULL,
    title = "Risk score distribution (Internal validation cohort)"
  ) +
  
  theme_classic(base_size = 15) +
  
  theme(
    legend.position = "none",
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.title.y = element_text(
      face = "bold",
      size = 12
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5
    ),
    
    plot.margin = margin(
      t = 12,
      r = 15,
      b = 5,
      l = 10
    )
  )

# =========================
# 7️⃣ 下图：
# RFH-NPT distribution
# =========================
p2 <- ggplot(df,
             aes(Index,
                 outcome,
                 color = factor(outcome),
                 shape = factor(outcome))) +
  
  geom_point(
    size = 2.5,
    alpha = 0.95
  ) +
  
  scale_color_manual(values = outcome_cols) +
  
  scale_shape_manual(values = c(
    16,
    17
  )) +
  
  geom_vline(
    xintercept = cut1,
    linetype = 2,
    linewidth = 0.7,
    color = "grey55"
  ) +
  
  geom_vline(
    xintercept = cut2,
    linetype = 2,
    linewidth = 0.7,
    color = "grey55"
  ) +
  
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c(
      "Low risk",
      "High risk"
    )
  ) +
  
  labs(
    x = "Patients (sorted by XGBoost risk score)",
    y = "RFH-NPT criteria"
  ) +
  
  theme_classic(base_size = 15) +
  
  theme(
    legend.position = "none",
    
    axis.title.x = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 12
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 17,
      hjust = 0.5
    ),
    
    plot.margin = margin(
      t = 5,
      r = 15,
      b = 10,
      l = 10
    )
  )

# =========================
# 8️⃣ 合并图形
# =========================
final_plot <- p1 / p2 +
  plot_layout(heights = c(2.3, 1))

# 显示
print(final_plot)

# =========================
# 9️⃣ 保存高清SCI图
# =========================
ggsave(
  "Nature_TCGA_XGBoost_RiskPlot.png",
  final_plot,
  width = 11.5,
  height = 7.8,
  dpi = 700,
  bg = "white"
)

# PDF投稿版
ggsave(
  "Nature_TCGA_XGBoost_RiskPlot.pdf",
  final_plot,
  width = 11.5,
  height = 7.8,
  bg = "white"
)