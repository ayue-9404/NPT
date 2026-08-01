library(ggplot2)
library(dplyr)

# ================================
# 用最终模型预测
# ================================
dtrain <- xgb.DMatrix(data = X, label = y)

model_final <- xgb.train(
  params = list(objective = "binary:logistic"),
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

pred <- predict(model_final, dtrain)

# ================================
# 分组（10组）
# ================================
df_cal <- data.frame(
  pred = pred,
  y = y
)

df_cal <- df_cal %>%
  mutate(group = ntile(pred, 10)) %>%
  group_by(group) %>%
  summarise(
    mean_pred = mean(pred),
    obs = mean(y)
  )

# ================================
# 画校准曲线（SCI风格）
# ================================
ggplot(df_cal, aes(x = mean_pred, y = obs)) +
  
  geom_point(size = 3, color = "#D7191C") +
  
  geom_line(color = "#D7191C", size = 1) +
  
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  
  labs(
    title = "Calibration Curve",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))