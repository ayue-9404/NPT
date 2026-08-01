# ================================
# 0️⃣ 加载包
# ================================
library(readxl)
library(car)
library(dplyr)
library(ggplot2)

# ================================
# 1️⃣ 读取数据
# ================================
df <- read_excel("COX.xlsx")

df <- df[, !is.na(names(df))]

names(df) <- c(
  "time","status","CTP","eGFR","GDF15",
  "MELD_Na","RFH_NPT","TyG","Frailty","gender"
)

df <- df %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# ================================
# 2️⃣ 7个变量
# ================================
vars <- c("CTP","eGFR","GDF15","RFH_NPT","TyG","Frailty","gender")

# ================================
# 3️⃣ 计算VIF（逐变量轮换）
# ================================
vif_list <- lapply(vars, function(y){
  
  model <- lm(
    as.formula(
      paste(y, "~", paste(setdiff(vars, y), collapse = "+"))
    ),
    data = df
  )
  
  vif_values <- vif(model)
  
  data.frame(
    Variable = names(vif_values),
    VIF = as.numeric(vif_values)
  )
})

vif_df <- bind_rows(vif_list) %>%
  group_by(Variable) %>%
  summarise(VIF = mean(VIF, na.rm = TRUE))

# ================================
# 4️⃣ SCI画图
# ================================
p_vif <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  
  geom_bar(stat = "identity", fill = "#2C7BB6", width = 0.6) +
  
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  
  geom_text(aes(label = round(VIF,2)), vjust = -0.5, size = 4) +
  
  coord_flip() +
  
  scale_y_continuous(limits = c(0, 5), expand = c(0,0)) +
  
  labs(
    title = "Multicollinearity Diagnosis (VIF)",
    x = "",
    y = "Variance Inflation Factor"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_vif

# ================================
# 5️⃣ 保存
# ================================
ggsave("VIF_7variables_SCI.pdf", p_vif, width = 7, height = 5)
ggsave("VIF_7variables_SCI.tiff", p_vif, width = 7, height = 5, dpi = 600)