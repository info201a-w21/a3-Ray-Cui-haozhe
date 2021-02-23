install.packages("maps")

library(tidyverse)
library(maps)

jail_trend <- read.csv("incarceration_trends.csv")


df <- jail_trend %>%
  group_by(state, black_prison_pop, white_prison_pop)

mean_white_prison_pop <- mean(df$white_male_prison_pop, na.rm = T) * 10
mean_black_prison_pop <- mean(df$black_male_prison_pop, na.rm = T) * 10
mean_white_jail_pop <- mean(df$white_jail_pop, na.rm = T) * 10
mean_black_jail_pop <- mean(df$black_jail_pop, na.rm = T) * 10

mean_top10_white_jail_proportion <- mean(top10_white_jail_proportion$white_jail_proportion)

char1 <- ggplot(data = df) + 
  geom_line(mapping = aes(y = black_prison_pop, x = year, color = "black_prison_pop")) +
  geom_hline(yintercept = mean_white_prison_pop, color = "blue") +
  geom_hline(yintercept = mean_black_prison_pop, color = "black") +
   labs(title = "Trend over time of black prison pop and comparsion between the mean prison pop with black")

chart2 <- ggplot(data = jail_trend) +
  geom_point(mapping = aes(x = black_prison_pop, y = white_prison_pop)) +
  facet_wrap(~region) +
  labs(title = "The comparsion between the pop of whtie prison and black prison in regions")


white_black_prison_pop <- jail_trend %>%
  select(2,3,4,5,58,62)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",")

map_data <- white_black_prison_pop %>%
  left_join(county_shape, by = "white_prison_pop")
  



  
  



