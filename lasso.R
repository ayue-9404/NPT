############################################################
# LASSO feature selection for RFH-NPT
# Input: all candidate variables
# Output:
# 1. LASSO coefficient path plot
# 2. Cross-validation lambda plot
############################################################


#========================
# 1. 加载包
#========================

library(glmnet)
library(dplyr)



#========================
# 2. 设置变量
#========================


outcome <- "RFH-NPT criteria"


predictors <- c(
  "Gender",
  "Age",
  "BMI",
  "HGS",
  "Frailty index",
  "Etiologies",
  "Ascites",
  "Hepatic encephalopathy",
  "Esophagogastric varices",
  "Infection",
  "CTP score",
  "MELD score",
  "MELD-Na score",
  "GDF15",
  "WBC",
  "PLT",
  "NLR",
  "AFP",
  "CEA",
  "CA-199",
  "INR",
  "ALB",
  "AST",
  "GGT",
  "TBIL",
  "eGFR",
  "TyG index"
)



#========================
# 3. 构建LASSO数据
#========================


lasso_data <- data %>%
  select(
    all_of(predictors),
    all_of(outcome)
  )



#========================
# 4. 转换为模型矩阵
#========================


x <- model.matrix(
  as.formula(
    paste(
      "`",
      outcome,
      "` ~ ."
    )
  ),
  data=lasso_data
)[,-1]


y <- lasso_data[[outcome]]



#========================
# 5. LASSO Logistic模型
#========================


set.seed(123)


lasso_model <- glmnet(
  x,
  y,
  family="binomial",
  alpha=1,
  nlambda=100
)



#========================
# 图1
# LASSO coefficient path
#========================


pdf(
  "LASSO_coefficient_path.pdf",
  width=7,
  height=6
)


plot(
  lasso_model,
  xvar="lambda",
  label=TRUE
)


dev.off()



#========================
# 6. 10-fold CV寻找最佳lambda
#========================


set.seed(123)


cv_lasso <- cv.glmnet(
  x,
  y,
  family="binomial",
  alpha=1,
  nfolds=10
)



#========================
# 图2
# Lambda selection curve
#========================


pdf(
  "LASSO_lambda_selection.pdf",
  width=7,
  height=6
)


plot(
  cv_lasso
)


dev.off()



#========================
# 7. 提取lambda.min变量
#========================


coef_min <- coef(
  cv_lasso,
  s="lambda.min"
)


coef_table <- data.frame(
  Variable=rownames(coef_min),
  Coefficient=as.numeric(coef_min)
)



lasso_selected <- coef_table %>%
  filter(
    Variable!="(Intercept)",
    Coefficient!=0
  )



print(lasso_selected)



#========================
# 8. 提取lambda.1se变量
#========================


coef_1se <- coef(
  cv_lasso,
  s="lambda.1se"
)


lasso_1se_selected <- data.frame(
  Variable=rownames(coef_1se),
  Coefficient=as.numeric(coef_1se)
)%>%
  filter(
    Variable!="(Intercept)",
    Coefficient!=0
  )



print(lasso_1se_selected)



#========================
# 9. 保存结果
#========================


write.csv(
  lasso_selected,
  "LASSO_lambda_min_selected.csv",
  row.names=FALSE
)


write.csv(
  lasso_1se_selected,
  "LASSO_lambda_1se_selected.csv",
  row.names=FALSE
)



############################################################
# END
############################################################