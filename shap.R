############################################################
# XGBoost SHAP combined bar + beeswarm plot
############################################################


library(shapviz)
library(ggplot2)



#=========================
# 1. SHAP计算
#=========================


final_features <- c(
  "GDF15",
  "CA-199",
  "GGT",
  "CTP score",
  "eGFR",
  "TyG index"
)


X_train_shap <- train_x[, final_features]



shap_obj <- shapviz(
  model_xgb$finalModel,
  X_pred = as.matrix(X_train_shap)
)



#=========================
# 2. combined SHAP plot
#=========================


p <- sv_importance(
  shap_obj,
  kind="both"
)+
  
  theme_classic()+
  
  labs(
    title="SHAP summary plot of XGBoost model",
    x="SHAP value",
    y=NULL
  )+
  
  theme(
    axis.text=element_text(size=11),
    axis.title=element_text(size=12)
  )



print(p)



#=========================
# 3. 保存
#=========================


ggsave(
  "XGBoost_SHAP_combined_beeswarm_bar.pdf",
  p,
  width=8,
  height=6
)


############################################################
# END
############################################################