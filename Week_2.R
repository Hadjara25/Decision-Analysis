library(ggplot2)

library(dplyr)
diamonds


# select carat & price

select(diamonds, carat,price)

#filter

filter(diamonds, carat > 0.5)

# rename
diamonds<- rename(diamonds, cost = price)
names(diamonds)

# mutate

mutate(diamonds, "expensive", mean(diamonds$cost))

# group_by

diamonds %>% group_by("expensive")


#summarise

group_by(diamonds) %>%
  summarise( carat)
