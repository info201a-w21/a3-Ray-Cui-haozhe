library(tidyverse)

chart2 <- ggplot(data = data) +
  geom_point(mapping = aes(x = black_prison_pop, y = white_prison_pop)) +
  facet_wrap(~region) +
  labs(title = "The comparsion between the population of whtie prison and black prison in regions")
