
library(tidyverse)

mean_white_prison_pop <- mean(data$white_male_prison_pop, na.rm = T) * 10
mean_black_prison_pop <- mean(data$black_male_prison_pop, na.rm = T) * 10
mean_white_jail_pop <- mean(data$white_jail_pop, na.rm = T) * 10
mean_black_jail_pop <- mean(data$black_jail_pop, na.rm = T) * 10

chart1 <- ggplot(data = data) + 
  geom_line(mapping = aes(y = black_prison_pop, x = year, color = "Black prison population trend")) +
  geom_hline(yintercept = mean_white_prison_pop, color = "blue") +
  geom_hline(yintercept = mean_black_prison_pop, color = "black") +
  labs(title = "Trend of black prison population and the contrast between the mean prison population of black and white")
