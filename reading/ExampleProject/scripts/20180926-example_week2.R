library(tidyverse)

data <- read.csv("data/binomial-data.csv", header=TRUE, as.is=TRUE)
View(data)

head(data)
data

as.tibble(data)



iris[iris$Sepal.Length<5 & iris$Species=="versicolor" & iris$Petal.Width > 0.3,]


iris %>%
  filter(Sepal.Length < 5,
         Species == "versicolor" | Species == "virginica",
         Petal.Width > 0.3)

data %>%
  ######
filter(experiment != "second") %>% head()





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
) -> newData#%>%
  #head()


long <- read.csv("data/long-data.csv")
long %>%
  spread(key=measure,value=value) %>%
  ######
separate(Savings,into=c("amount","currency"),sep=" ") %>%
  mutate(Age = as.integer(Age),
         amount = as.numeric(amount)) %>%
  as.tibble()






iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, colour=Species)) +
  geom_point(aes(shape=Species)) +
  facet_wrap(~Species)
