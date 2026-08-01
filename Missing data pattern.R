library(VIM)

aggr(dat,
     col = c("navyblue", "red"),
     numbers = TRUE,
     sortVars = TRUE,
     labels = names(dat),
     cex.axis = 0.7,
     gap = 3,
     ylab = c("Missing data pattern", "Pattern"))