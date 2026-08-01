# =========================================
# VIF分析完整代码（适用于你的 VIF.xlsx）
# 第一列：ID
# 其余列：进入多因素分析前的候选变量
# 例如：
# RFH-NPT score、Age、BMI、HGS、Frailty.index、
# MELD.score、CTP.score、MELD.Na.score 等
# =========================================

# =========================
# 0. 安装并加载包
# =========================

packages <- c(
  "car",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readxl",
  "stringr"
)

to_install <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# =========================
# 1. 读取 Excel 数据
# 文件名：VIF.xlsx
# =========================

dat <- read_excel("VIF.xlsx")

# 转为 data.frame
dat <- as.data.frame(dat)

# 非常重要！
# 自动把变量名改成R可识别格式
# 例如：
# RFH-NPT score → RFH.NPT.score
# Frailty index → Frailty.index
names(dat) <- make.names(names(dat))

# 查看数据结构
print(dim(dat))
print(names(dat))
print(head(dat))

# =========================
# 2. 删除第一列 ID
# 只保留数值型变量
# =========================

# 删除第一列（默认第一列是ID）
dat <- dat[, -1]

# 自动筛选数值型变量
num_vars <- dat %>%
  select(where(is.numeric))

# 查看进入VIF分析的变量
cat("\n进入VIF分析的变量：\n")
print(colnames(num_vars))

# 去除缺失值（VIF不能有NA）
num_vars <- na.omit(num_vars)

# 如果你想手动指定变量，可改用：
# num_vars <- dat %>%
#   select(
#     RFH.NPT.score,
#     Age,
#     BMI,
#     HGS,
#     Frailty.index,
#     CTP.score,
#     MELD.score,
#     MELD.Na.score
#   )

# =========================
# 3. 定义函数：
# 每个变量轮流作为因变量计算VIF
# =========================

calc_vif_table <- function(df_numeric) {
  
  vars <- names(df_numeric)
  res_list <- list()
  
  for (y in vars) {
    
    x_vars <- setdiff(vars, y)
    
    # 构建公式
    # y ~ x1 + x2 + ...
    fml <- as.formula(
      paste(y, "~", paste(x_vars, collapse = " + "))
    )
    
    fit <- lm(fml, data = df_numeric)
    
    # 提取VIF
    v <- car::vif(fit)
    
    tmp <- data.frame(
      response = y,
      predictor = names(v),
      VIF = as.numeric(v),
      stringsAsFactors = FALSE
    )
    
    res_list[[y]] <- tmp
  }
  
  bind_rows(res_list)
}

# =========================
# 4. 迭代删除高VIF变量
# =========================

iterative_vif_filter <- function(
    df_numeric,
    threshold = 5,
    trace = TRUE
) {
  
  current_df <- df_numeric
  
  removed <- data.frame(
    step = integer(),
    removed_var = character(),
    max_vif = numeric(),
    stringsAsFactors = FALSE
  )
  
  step <- 0
  
  repeat {
    
    step <- step + 1
    
    vif_long <- calc_vif_table(current_df)
    
    # 汇总每个变量最严重的VIF
    vif_summary <- vif_long %>%
      group_by(predictor) %>%
      summarise(
        max_VIF = max(VIF, na.rm = TRUE),
        mean_VIF = mean(VIF, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(max_VIF))
    
    max_vif_value <- vif_summary$max_VIF[1]
    max_vif_var <- vif_summary$predictor[1]
    
    if (trace) {
      cat("\n========================\n")
      cat("Step", step, "\n\n")
      print(vif_summary)
      
      cat(
        "\n当前最大VIF：",
        round(max_vif_value, 3),
        "| 删除变量：",
        max_vif_var,
        "\n"
      )
    }
    
    # 所有变量都小于阈值则停止
    if (
      is.na(max_vif_value) ||
      max_vif_value < threshold
    ) {
      break
    }
    
    # 删除最大VIF变量
    current_df <- current_df %>%
      select(-all_of(max_vif_var))
    
    removed <- bind_rows(
      removed,
      data.frame(
        step = step,
        removed_var = max_vif_var,
        max_vif = max_vif_value,
        stringsAsFactors = FALSE
      )
    )
    
    # 防止变量太少
    if (ncol(current_df) <= 2) {
      warning("变量已减少到2个及以下，停止迭代。")
      break
    }
  }
  
  # 最终VIF结果
  final_vif_long <- calc_vif_table(current_df)
  
  final_vif_summary <- final_vif_long %>%
    group_by(predictor) %>%
    summarise(
      max_VIF = max(VIF, na.rm = TRUE),
      mean_VIF = mean(VIF, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(max_VIF))
  
  list(
    kept_data = current_df,
    removed_log = removed,
    final_vif = final_vif_summary
  )
}

# =========================
# 5. 执行VIF筛选
# 医学论文最常用：
# threshold = 5
# 如果你想宽松一些可改成10
# =========================

vif_threshold <- 5

res <- iterative_vif_filter(
  num_vars,
  threshold = vif_threshold,
  trace = TRUE
)

# 输出结果
cat("\n========================\n")
cat("剔除日志：\n")
print(res$removed_log)

cat("\n最终保留变量：\n")
print(names(res$kept_data))

cat("\n最终VIF结果：\n")
print(res$final_vif)

# =========================
# 6. 图1：最终VIF柱状图
# =========================

p_final <- ggplot(
  res$final_vif,
  aes(
    x = reorder(predictor, max_VIF),
    y = max_VIF
  )
) +
  geom_col(
    fill = "#2C7FB8",
    width = 0.7
  ) +
  geom_hline(
    yintercept = vif_threshold,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  coord_flip() +
  labs(
    title = "Final VIF of Retained Variables",
    x = "Variables",
    y = "Max VIF"
  ) +
  theme_bw(base_size = 12)

print(p_final)

# =========================
# 7. 导出结果（可选）
# 取消注释即可保存
# =========================

# write.csv(
#   res$removed_log,
#   "vif_removed_log.csv",
#   row.names = FALSE
# )

# write.csv(
#   res$final_vif,
#   "vif_final_table.csv",
#   row.names = FALSE
# )

# write.csv(
#   res$kept_data,
#   "data_after_vif_filter.csv",
#   row.names = FALSE
# )

# ggsave(
#   "vif_final_plot.png",
#   p_final,
#   width = 8,
#   height = 6,
#   dpi = 300
# )