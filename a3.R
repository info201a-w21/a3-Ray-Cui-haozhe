

library(tidyverse)


data <- read.csv("incarceration_trends.csv")


df <- jail_trend %>%
  group_by(state, black_prison_pop, white_prison_pop)

mean_white_prison_pop <- mean(df$white_male_prison_pop, na.rm = T) * 10
mean_black_prison_pop <- mean(df$black_male_prison_pop, na.rm = T) * 10
mean_white_jail_pop <- mean(df$white_jail_pop, na.rm = T) * 10
mean_black_jail_pop <- mean(df$black_jail_pop, na.rm = T) * 10

mean_top10_white_jail_proportion <- mean(top10_white_jail_proportion$white_jail_proportion)

chart1 <- ggplot(data = df) + 
  geom_line(mapping = aes(y = black_prison_pop, x = year, color = "black_prison_pop")) +
  geom_hline(yintercept = mean_white_prison_pop, color = "blue") +
  geom_hline(yintercept = mean_black_prison_pop, color = "black") +
   labs(title = "Trend over time of black prison pop and comparsion between the mean prison pop with black")

chart2 <- ggplot(data = jail_trend) +
  geom_point(mapping = aes(x = black_prison_pop, y = white_prison_pop)) +
  facet_wrap(~region) +
  labs(title = "The comparsion between the pop of whtie prison and black prison in regions")


incarceration_mad <- jail_trend  %>%
  select(2,3,4,5,58,62)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- incarceration_mad %>%
  left_join(county_shapes, by = "fips") %>%
  filter(state == "WA" & county_name != "Unknown")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

black_prison_pop_map <- ggplot(map_data) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_prison_pop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme

  



  
  



