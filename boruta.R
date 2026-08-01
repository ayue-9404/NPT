############################################################
# Univariate Logistic Regression + Boruta Feature Selection
# Outcome: RFH-NPT criteria
############################################################


#========================
# 1. 加载R包
#========================

library(dplyr)
library(broom)
library(Boruta)
library(ggplot2)
library(tibble)



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
# 3. 单因素 Logistic 回归
#========================


univ_results <- lapply(
  predictors,
  function(var){
    
    
    formula <- as.formula(
      paste0(
        "`",
        outcome,
        "` ~ `",
        var,
        "`"
      )
    )
    
    
    model <- glm(
      formula,
      data=data,
      family=binomial
    )
    
    
    tidy(
      model,
      exponentiate=TRUE,
      conf.int=TRUE
    ) %>%
      filter(term!="(Intercept)") %>%
      mutate(
        Variable=var
      )
    
  }
) %>%
  bind_rows()



# 查看单因素结果

print(univ_results)



# 保存单因素结果

write.csv(
  univ_results,
  "Univariate_logistic_results.csv",
  row.names=FALSE
)



#========================
# 4. 筛选单因素显著变量
#========================


sig_vars <- univ_results %>%
  filter(p.value < 0.05) %>%
  pull(Variable) %>%
  unique()


print(sig_vars)



#========================
# 5. 构建Boruta数据
#========================


boruta_data <- data %>%
  select(
    all_of(sig_vars),
    all_of(outcome)
  )



#========================
# 6. Boruta分析
#========================


set.seed(123)


boruta_model <- Boruta(
  as.formula(
    paste0(
      "`",
      outcome,
      "` ~ ."
    )
  ),
  data=boruta_data,
  maxRuns=500,
  doTrace=1
)



# 查看结果

print(boruta_model)



#========================
# 7. Boruta结果整理
#========================


boruta_result <- attStats(
  boruta_model
)


boruta_result <- boruta_result %>%
  rownames_to_column(
    "Variable"
  )


print(boruta_result)



# 保存Boruta结果

write.csv(
  boruta_result,
  "Boruta_results.csv",
  row.names=FALSE
)



#========================
# 8. Boruta默认重要性图
#========================


pdf(
  "Boruta_importance_boxplot.pdf",
  width=8,
  height=6
)

plot(
  boruta_model,
  las=2,
  cex.axis=0.8,
  main="Boruta Feature Selection"
)

dev.off()



#========================
# 9. SCI风格Boruta条形图
#========================


boruta_plot_data <- boruta_result %>%
  arrange(meanImp)



p <- ggplot(
  boruta_plot_data,
  aes(
    x=reorder(Variable, meanImp),
    y=meanImp,
    fill=decision
  )
)+
  geom_col(
    width=0.7
  )+
  coord_flip()+
  theme_classic()+
  labs(
    x=NULL,
    y="Mean importance",
    title="Boruta Feature Importance"
  )



print(p)



ggsave(
  "Boruta_feature_importance.pdf",
  p,
  width=7,
  height=6
)



#========================
# 10. 获取最终确认变量
#========================


selected_features <- getSelectedAttributes(
  boruta_model,
  withTentative=FALSE
)


print(selected_features)



write.csv(
  data.frame(
    Selected_features=selected_features
  ),
  "Boruta_selected_features.csv",
  row.names=FALSE
)



############################################################
# END
############################################################