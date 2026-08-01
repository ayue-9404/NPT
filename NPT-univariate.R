############################################################
# Univariate logistic regression forest plot
# Outcome: RFH-NPT criteria
############################################################


# 加载包
library(dplyr)
library(broom)
library(ggplot2)
library(forcats)


#=========================
# 1. 设置变量
#=========================

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



#=========================
# 2. 单因素 Logistic回归
#=========================

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
      exponentiate = TRUE,
      conf.int = TRUE
    ) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        Variable = var
      )
    
  }
) %>%
  bind_rows()



# 查看结果
univ_results



#=========================
# 3. 整理森林图数据
#=========================


forest_data <- univ_results %>%
  mutate(
    label=paste0(
      round(estimate,2),
      " (",
      round(conf.low,2),
      "-",
      round(conf.high,2),
      ")"
    ),
    Variable=fct_reorder(
      Variable,
      estimate
    )
  )



#=========================
# 4. 绘制单因素森林图
#=========================


p <- ggplot(
  forest_data,
  aes(
    x=estimate,
    y=Variable
  )
)+
  
  geom_point(
    size=3
  )+
  
  geom_errorbarh(
    aes(
      xmin=conf.low,
      xmax=conf.high
    ),
    height=0.25
  )+
  
  geom_vline(
    xintercept=1,
    linetype="dashed"
  )+
  
  scale_x_log10()+
  
  theme_classic()+
  
  labs(
    title="Univariate Logistic Regression",
    x="Odds Ratio (95% CI)",
    y=NULL
  )



print(p)



#=========================
# 5. 保存图片
#=========================


ggsave(
  "RFH_NPT_univariate_forest.pdf",
  p,
  width=8,
  height=10
)



#=========================
# 6. 保存结果表
#=========================


write.csv(
  univ_results,
  "RFH_NPT_univariate_logistic_results.csv",
  row.names=FALSE
)