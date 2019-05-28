library(tidyverse)

df.1 <- read.csv("data/binomial-data.csv",header=TRUE,as.is=TRUE)
View(data)
head(data, 10)


data %>%
  ######
mutate(condition = case_when(experiment == "first" & condition == "Baseline" ~ "Gap", # exp1 has two conditions: Gap and RP
                             experiment == "first" & condition == "Treatment" ~ "RP",
                             experiment == "second" & condition == "Baseline" ~ "Old form", # exp 2 has two conditions: Old and New
                             experiment == "second" & condition == "Treatment" ~ "New form",
                             TRUE ~ as.character(condition) # exp3 conditions stay the same
)
) -> data2





set.seed(12345)

data %>%
  filter(experiment == "second") %>% # only include the rows in which "experiment" contains the string "second"
  ######
mutate(fakeValue = rnorm(n=selectCode, mean = 10, sd = 3), # create fake continuous data
       fakeLength = round(rnorm(n=selectCode, mean = 6, sd = .5), 0), # create fake discrete data
       fakeResid = fakeValue / fakeLength) %>% # calculate fake value residualised by fake length
  ######
mutate(condition = case_when(experiment == "first" & condition == "Baseline" ~ "Gap", # exp1 has two conditions: Gap and RP
                             experiment == "first" & condition == "Treatment" ~ "Resumptive Pronoun",
                             experiment == "second" & condition == "Baseline" ~ "Old form", # exp 2 has two conditions: Old and New
                             experiment == "second" & condition == "Treatment" ~ "New form",
                             TRUE ~ as.character(condition) # exp3 conditions stay the same
)
) %>%
  head() # show the first 6 lines of the data.frame


iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour=Species)) +
  geom_smooth(aes(colour=Species),method = "lm")



iris %>% head()

iris %>%
  filter(Petal.Length > 1.6) %>%
  ggplot(aes(x = Species, y = Sepal.Length, fill=Species)) +
  geom_boxplot() +
  theme(legend.position = "null")

iris %>%
  filter(Species != "setosa") %>%
  filter(Petal.Length < 4.8) %>%
  ggplot(aes(x = Species, y = Sepal.Length, fill=Species)) +
  geom_boxplot() +
  theme(legend.position = "null")

iris %>%
  ggplot(aes(x = Petal.Length, fill=Species)) +
  geom_histogram()



iris %>%
  filter(Species != "setosa") %>%
  filter(Petal.Length > 4.8) %>%
  ggplot(aes(x = Species, y = Sepal.Length, fill=Species)) +
  geom_boxplot() +
  geom_point(alpha=.5) +
  theme(legend.position = "null")
