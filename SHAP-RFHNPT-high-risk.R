############################################################
# SHAP explanation for RFH-NPT low-risk patient
# XGBoost model
############################################################


library(shapviz)



#=========================
# 1. 找一个NPT低风险患者
#=========================


high_index <- which(
  train_data$`RFH-NPT criteria` == 1
)[1]


low_index



#=========================
# 2. SHAP waterfall plot
#=========================


low_waterfall <- sv_waterfall(
  shap_obj,
  row_id = high_index
)


print(low_waterfall)



ggsave(
  "SHAP_waterfall_RFHNPT_high_risk.pdf",
  low_waterfall,
  width=7,
  height=5
)



#=========================
# 3. SHAP force plot
#=========================


low_force <- sv_force(
  shap_obj,
  row_id = high_index
)


print(low_force)



ggsave(
  "SHAP_force_RFHNPT_high_risk.pdf",
  low_force,
  width=8,
  height=3
)



############################################################
# END
############################################################