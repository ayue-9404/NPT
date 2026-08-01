# =========================
# 变量相关性热图（适用于“最终.xlsx”）—— 无数字版
# =========================

# 加载包
library(readxl)
library(corrplot)

# 如果没安装 corrplot，先运行：
# install.packages("corrplot")

# =========================
# 1. 读取 Excel 文件
# =========================

data <- read_excel("最终.xlsx")

# 转为 data.frame
data <- as.data.frame(data)

# 删除第一列 ID
data <- data[, -1]

# =========================
# 2. 只保留数值型变量
# （相关性分析只能用于连续变量）
# =========================

data_num <- data[, sapply(data, is.numeric)]

# 查看纳入分析的变量
colnames(data_num)

# =========================
# 3. 计算相关系数矩阵
# Pearson相关（最常用）
# =========================

cor_matrix <- cor(
  data_num,
  use = "complete.obs",
  method = "pearson"
)

# 如果你想用 Spearman（医学论文也常用）
# cor_matrix <- cor(
#   data_num,
#   use = "complete.obs",
#   method = "spearman"
# )

# =========================
# 4. 绘制相关性热图（已去掉方块内文字）
# =========================

corrplot(
  cor_matrix,
  
  method = "color",       # 彩色热图
  type = "upper",         # 只显示上三角
  order = "hclust",       # 聚类排序
  
  tl.col = "black",       # 变量名称颜色
  tl.cex = 0.8,           # 字体大小
  tl.srt = 45,            # 标签旋转角度
  
  col = colorRampPalette(
    c("#77C034", "white", "#C388FE")
  )(200),
  
  # 关键：关闭了数字显示
  # addCoef.col = "black",
  # number.cex = 0.6,
  
  diag = FALSE            # 不显示对角线
)