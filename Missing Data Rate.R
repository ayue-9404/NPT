library(dplyr)
library(ggplot2)
library(tidyr)

missing_rate <- dat %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "MissingRate"
  )

ggplot(missing_rate, aes(x = reorder(Variable, MissingRate), y = MissingRate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Missing Data Rate by Variable",
    x = "Variables",
    y = "Missing (%)"
  ) +
  theme_minimal()