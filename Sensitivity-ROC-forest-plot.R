library(ggplot2)


# =========================
# Forest plot data
# =========================

forest_df <- data.frame(
  
  cutoff=c(
    "≥2 (Primary cutoff)",
    "≥1 (Sensitivity analysis 1)",
    "≥3 (Sensitivity analysis 2)",
    "≥4 (Sensitivity analysis 3)"
  ),
  
  AUC=c(
    0.826,
    0.738,
    0.731,
    0.647
  ),
  
  lower=c(
    0.789,
    0.691,
    0.682,
    0.510
  ),
  
  upper=c(
    0.863,
    0.786,
    0.780,
    0.784
  )
  
)



# 调整顺序

forest_df$cutoff <- factor(
  forest_df$cutoff,
  levels=rev(forest_df$cutoff)
)



# =========================
# Plot
# =========================

p <- ggplot(
  forest_df,
  aes(
    x=AUC,
    y=cutoff
  )
)+
  
  
  geom_errorbar(
    
    aes(
      xmin=lower,
      xmax=upper
    ),
    
    width=0.15,
    
    linewidth=1
    
  )+
  
  
  geom_point(
    
    size=4
    
  )+
  
  
  geom_vline(
    
    xintercept=0.5,
    
    linetype="dashed",
    
    color="grey60"
    
  )+
  
  
  geom_text(
    
    aes(
      
      label=paste0(
        
        round(AUC,3),
        
        " (",
        
        round(lower,3),
        
        "–",
        
        round(upper,3),
        
        ")"
        
      )
      
    ),
    
    hjust=-0.1,
    
    size=4
    
  )+
  
  
  scale_x_continuous(
    
    limits=c(0.45,1),
    
    breaks=seq(
      0.5,
      1,
      0.1
    )
    
  )+
  
  
  labs(
    
    x="AUC (95% CI)",
    
    y=NULL,
    
    title="Sensitivity analysis of model discrimination across RFH-NPT cutoff definitions"
    
  )+
  
  
  theme_classic(
    
    base_size=15
    
  )+
  
  
  theme(
    
    panel.border=element_rect(
      
      color="black",
      
      fill=NA,
      
      linewidth=1
      
    ),
    
    plot.title=element_text(
      
      hjust=0.5,
      
      face="bold"
      
    )
    
  )


print(p)



ggsave(
  
  "RFH_NPT_cutoff_AUC_forest_plot.pdf",
  
  p,
  
  width=7,
  
  height=5,
  
  dpi=600
  
)