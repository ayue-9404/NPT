library(readxl)
library(survival)
library(survminer)
library(dplyr)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("性别.xlsx")

# ================================
# 2️⃣ 分组 Female / Male
# ================================
df <- df %>%
  mutate(
    group = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male",
      TRUE ~ NA_character_
    )
  )

df$group <- factor(
  df$group,
  levels = c("Female", "Male")
)

# ================================
# 3️⃣ 构建生存对象
# ================================
fit <- survfit(
  Surv(time, status) ~ group,
  data = df
)

# ================================
# 4️⃣ Cox 回归
# ================================
cox_model <- coxph(
  Surv(time, status) ~ group,
  data = df
)

cox_summary <- summary(cox_model)

hr <- round(
  cox_summary$coefficients[, "exp(coef)"],
  2
)

lower <- round(
  cox_summary$conf.int[, "lower .95"],
  2
)

upper <- round(
  cox_summary$conf.int[, "upper .95"],
  2
)

pval <- signif(
  cox_summary$coefficients[, "Pr(>|z|)"],
  3
)

# ================================
# 5️⃣ HR文字
# ================================
label_text <- paste0(
  "P = ", pval,
  "\nHR = ", hr,
  ", 95% CI (",
  lower, "-", upper, ")"
)

# ================================
# 6️⃣ KM曲线
# ================================
p <- ggsurvplot(
  
  fit,
  data = df,
  
  conf.int = TRUE,
  
  # SCI颜色
  palette = c(
    "#2C7BB6",
    "#D7191C"
  ),
  
  linetype = "strata",
  surv.median.line = NULL,
  
  xlab = "Time (days)",
  ylab = "Progression-Free Survival",
  
  xlim = c(
    0,
    max(df$time, na.rm = TRUE)
  ),
  
  break.time.by = 30,
  
  # Risk table
  risk.table = TRUE,
  risk.table.height = 0.35,
  risk.table.fontsize = 5,
  risk.table.y.text.col = TRUE,
  risk.table.y.text = TRUE,
  risk.table.title = "Number at risk",
  risk.table.col = "strata",
  
  risk.table.theme = theme(
    axis.text.y = element_text(
      margin = margin(t = 2, b = 2)
    )
  ),
  
  # 图例
  legend.title = "",
  
  legend.labs = c(
    "Female",
    "Male"
  ),
  
  legend = "top",
  
  # 主题
  ggtheme = theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      
      axis.line = element_line(
        linewidth = 0.8
      ),
      
      axis.title = element_text(
        size = 13,
        face = "bold"
      ),
      
      axis.text = element_text(
        size = 13,
        color = "black"
      ),
      
      legend.text = element_text(
        size = 13
      )
    )
)

# ================================
# 7️⃣ 添加 HR + P值
# ================================
p$plot <- p$plot +
  
  annotate(
    "text",
    
    x = max(df$time, na.rm = TRUE) * 0.05,
    y = 0.1,
    
    label = label_text,
    
    size = 5,
    hjust = 0,
    vjust = 0
  )

# ================================
# 8️⃣ 显示图
# ================================
p