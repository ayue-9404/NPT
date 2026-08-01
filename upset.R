# =========================================
# 1. 加载包
# =========================================

library(UpSetR)
library(readxl)

# =========================================
# 2. 读取数据
# =========================================

dat <- read_excel("韦恩.xlsx")
dat <- as.data.frame(dat)

# =========================================
# 3. 清洗函数
# =========================================

clean_col <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != ""]
  unique(x)
}

# =========================================
# 4. 提取四种方法（已改RFE）
# =========================================

Univariate <- clean_col(dat$Univariate)
LASSO <- clean_col(dat$LASSO)
Boruta <- clean_col(dat$Boruta)
RFE <- clean_col(dat$RFE)

# 所有变量集合
all_vars <- unique(c(
  Univariate,
  LASSO,
  Boruta,
  RFE
))

# =========================================
# 5. 构建0/1矩阵（UpSet专用）
# =========================================

upset_data <- data.frame(row.names = all_vars)

upset_data$Univariate <- as.numeric(all_vars %in% Univariate)
upset_data$LASSO <- as.numeric(all_vars %in% LASSO)
upset_data$Boruta <- as.numeric(all_vars %in% Boruta)
upset_data$RFE <- as.numeric(all_vars %in% RFE)

# =========================================
# 6. 绘制SCI风格UpSet图
# =========================================

upset(
  upset_data,
  sets = c("Univariate", "LASSO", "Boruta", "RFE"),
  
  order.by = "freq",
  keep.order = TRUE,
  
  main.bar.color = "#2C7FB8",
  matrix.color = "#4C72B0",
  sets.bar.color = "#8DA0CB",
  
  text.scale = c(
    1.4,
    1.2,
    1.2,
    1.2,
    1.3
  )
)