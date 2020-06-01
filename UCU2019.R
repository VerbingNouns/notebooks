data = read.csv("UCU-ballot2019.csv") # ingredients

library(tidyverse) # open the recipe book

View(data) # look at what the ingredients are

data %>%
  filter(Ballot == "USS-strike") %>%
  mutate(Turnout = case_when( pcTurnout >= 50 ~ "over",
                              pcTurnout <  50 ~ "under")) %>%
  ggplot(aes( x = Region %>% reorder(-pcYes),
              y = pcYes,
              fill = Turnout)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Region name")

