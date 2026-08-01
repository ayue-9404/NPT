# =========================================================
# 🔥 SCI最终版 Cox 森林图（带P值 + 投稿级排版）
# =========================================================

# ================================
# 0️⃣ 加载包
# ================================
library(readxl)
library(survival)
library(broom)
library(dplyr)
library(ggplot2)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("COX.xlsx")

# 删除空白列
df <- df[, !is.na(names(df))]

# 查看列名
names(df)

# ================================
# 2️⃣ 统一列名
# ================================
names(df) <- c(
  "time",
  "status",
  "CTP",
  "eGFR",
  "GDF15",
  "MELD_Na",
  "RFH_NPT",
  "TyG",
  "Frailty",
  "gender"
)

# ================================
# 3️⃣ 数据整理
# ================================
df <- df %>%
  mutate(
    time = as.numeric(time),
    status = as.numeric(status),
    
    CTP = as.numeric(CTP),
    eGFR = as.numeric(eGFR),
    GDF15 = as.numeric(GDF15),
    
    MELD_Na = as.numeric(MELD_Na),
    
    RFH_NPT = as.numeric(RFH_NPT),
    TyG = as.numeric(TyG),
    Frailty = as.numeric(Frailty),
    
    gender = as.numeric(gender)
  ) %>%
  na.omit()

# =========================================================
# 4️⃣ 单因素 Cox
# =========================================================
vars <- c(
  "CTP",
  "eGFR",
  "GDF15",
  "RFH_NPT",
  "TyG",
  "Frailty",
  "gender"
)

univ <- lapply(vars, function(v){
  
  model <- coxph(
    as.formula(
      paste("Surv(time,status) ~", v)
    ),
    data = df
  )
  
  tidy(
    model,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(model = "Univariate")
})

univ_df <- bind_rows(univ)

# =========================================================
# 5️⃣ 多因素 Cox（去掉 MELD-Na）
# =========================================================
multi_model <- coxph(
  Surv(time,status) ~
    CTP + eGFR + GDF15 +
    RFH_NPT + TyG +
    Frailty + gender,
  data = df
)

multi_df <- tidy(
  multi_model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(model = "Multivariate")

# =========================================================
# 6️⃣ 合并数据
# =========================================================
forest_df <- bind_rows(
  univ_df,
  multi_df
)

# =========================================================
# 7️⃣ 美化变量名（稳定版🔥）
# =========================================================
forest_df$term <- factor(
  forest_df$term,
  levels = c(
    "CTP",
    "eGFR",
    "GDF15",
    "RFH_NPT",
    "TyG",
    "Frailty",
    "gender"
  ),
  labels = c(
    "CTP score",
    "eGFR",
    "GDF15",
    "RFH-NPT score",
    "TyG",
    "Frailty index",
    "Gender"
  )
)

# =========================================================
# 8️⃣ 整理显示内容
# =========================================================
forest_df <- forest_df %>%
  mutate(
    
    HR = round(estimate, 2),
    
    lower = round(conf.low, 2),
    upper = round(conf.high, 2),
    
    P = signif(p.value, 3),
    
    CI = paste0(
      HR,
      " (",
      lower,
      "-",
      upper,
      ")"
    ),
    
    P_text = ifelse(
      P < 0.001,
      "<0.001",
      as.character(P)
    ),
    
    Label = paste0(
      CI,
      "\nP = ",
      P_text
    )
  )

# =========================================================
# 9️⃣ 排序（SCI更美观）
# =========================================================
forest_df$term <- factor(
  forest_df$term,
  levels = rev(c(
    "CTP score",
    "eGFR",
    "GDF15",
    "RFH-NPT score",
    "TyG",
    "Frailty index",
    "Gender"
  ))
)

# =========================================================
# 🔟 SCI最终森林图
# =========================================================
p <- ggplot(
  forest_df,
  aes(
    x = term,
    y = estimate,
    color = model
  )
) +
  
  # 95%CI
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.18,
    linewidth = 0.9,
    position = position_dodge(0.7)
  ) +
  
  # HR点
  geom_point(
    size = 3.5,
    position = position_dodge(0.7)
  ) +
  
  # HR=1参考线
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  # HR + P值文字
  geom_text(
    aes(label = Label),
    hjust = -0.05,
    size = 3.5,
    position = position_dodge(0.7),
    show.legend = FALSE
  ) +
  
  # 翻转
  coord_flip(clip = "off") +
  
  # SCI配色
  scale_color_manual(
    values = c(
      "Univariate" = "#2C7BB6",
      "Multivariate" = "#D7191C"
    )
  ) +
  
  # Y轴范围
  ylim(
    0,
    max(forest_df$upper) * 1.45
  ) +
  
  # 标题
  labs(
    x = "",
    y = "Hazard Ratio (95% CI)",
    color = "",
    title = "Univariate and Multivariate Cox Regression"
  ) +
  
  # SCI主题
  theme_classic(base_size = 13) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    ),
    
    axis.title.x = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text = element_text(
      size = 12,
      color = "black"
    ),
    
    legend.position = "top",
    
    legend.text = element_text(
      size = 12
    ),
    
    plot.margin = margin(
      10, 120, 10, 10
    )
  )

# =========================================================
# 1️⃣1️⃣ 显示图
# =========================================================
p

# =========================================================
# 1️⃣2️⃣ 保存PDF（SCI投稿）
# =========================================================
ggsave(
  "SCI_Cox_ForestPlot.pdf",
  p,
  width = 10,
  height = 6.5,
  dpi = 300
)

# =========================================================
# 1️⃣3️⃣ 保存TIFF（SCI投稿）
# =========================================================
ggsave(
  "SCI_Cox_ForestPlot.tiff",
  p,
  width = 10,
  height = 6.5,
  dpi = 600,
  compression = "lzw"
)