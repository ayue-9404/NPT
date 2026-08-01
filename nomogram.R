############################################################
# Nomogram based on final logistic regression model
############################################################


library(rms)


# 设置数据分布
dd <- datadist(train_data)
options(datadist="dd")



# 最终模型

nomogram_model <- lrm(
  
  `RFH-NPT criteria` ~ 
    GDF15 +
    `CA-199` +
    GGT +
    `CTP score` +
    eGFR +
    `TyG index`,
  
  data=train_data,
  
  x=TRUE,
  y=TRUE
)



nomogram_model