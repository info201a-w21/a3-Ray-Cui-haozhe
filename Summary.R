library(tidyverse)




# How many total jail population have there been in the U.S. by the most recent date
# in the dataset? `total_jail_pop_cases After calculated, how many total jail population
# of balck and white have been in the U.S by the most recent data? What is their ratio 
# and  What are the five states with the highest rates?


most_ten_year_pop <- data %>%
  group_by(year, state) %>%
  filter(2008 <= year & year <= 2018) %>%
  summarise(sum_total_pop = sum(total_pop_15to64, na.rm =T))

top_10_pop_state <- most_ten_year_pop %>%
  group_by(state) %>%
  summarise(sum_10_year_pop = sum(sum_total_pop)) %>%
  mutate(mean_ten_year_pop = sum_10_year_pop / 10) %>%
  top_n(5, state)

# The states which have the most population difference between 2008 to 2018 is
# VT, WA, WI, WV and WY. 
# Next, we find the ratio of total population to jail_black and white
# and prison_black and white in these 5 states to discover whether these are some
# difference between prison and jail. 

ratio_jail_black_white <- data %>%
  filter(2008 <= year & year <= 2018) %>%
  filter(state == "WA"| state =="VT"|state =="WI"|state =="WV"|state =="WY") %>%
  select(year, state, black_jail_pop, white_jail_pop, total_pop_15to64) %>%
  mutate(ratio_black_to_white_jail = black_jail_pop / white_jail_pop) %>%
  mutate(ratio_white_to_black_jail = white_jail_pop/ black_jail_pop)

difference_jail_ratio <- ratio_jail_black_white %>%
  summarise(sum_black_to_white_ratio = sum(ratio_black_to_white_jail, na.rm =T),
            sum_white_to_black_ratio = sum(ratio_white_to_black_jail, na.rm =T))

# From the contrast between these two ratio, we can find the ratio_black_to white are
# much larger than ratio_white_to_black (359.2596 and inf)

# Now for prison analysis 

ratio_prison_black_white <- data %>%
  filter(2008 <= year & year <= 2018) %>%
  filter(state == "WA"| state =="VT"|state =="WI"|state =="WV"|state =="WY") %>%
  select(year, state, black_prison_pop, white_prison_pop, total_pop_15to64) %>%
  mutate(ratio_black_to_white_prison = black_prison_pop / white_prison_pop) %>%
  mutate(ratio_white_to_black_prison =  white_prison_pop/ black_prison_pop)
  
  

difference_prison_ratio <- ratio_prison_black_white %>%
  summarise(sum_black_to_white_ratio = sum(ratio_black_to_white_prison, na.rm =T),
            sum_white_to_black_ratio = sum(ratio_white_to_black_prison, na.rm =T))

# we can find the ratio_black_to white are
# much larger than ratio_white_to_black (192.2482 and inf)
  
  


            
            


`
  
  