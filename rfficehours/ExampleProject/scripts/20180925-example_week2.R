data <- read.csv("data/binomial-data.csv",as.is=TRUE)
View(data)

library(tidyverse)
long <- read_csv("data/long-data.csv")

head(data, 10)
as.tibble(data)


View(iris)

iris[iris$Sepal.Length<5 & iris$Species=="versicolor" & iris$Petal.Width > 0.3,]

iris %>%
  filter(Sepal.Length < 5,
         Species == "versicolor",
         Petal.Width > 0.3)




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



set.seed(12345)

data %>%
  filter(experiment == "second") %>% # only include the rows in which "experiment" contains the string "second"
  mutate(fakeValue = rnorm(n=selectCode, mean = 10, sd = 3), # create fake continuous data
         fakeLength = round(rnorm(n=selectCode, mean = 6, sd = .5), 0), # create fake discrete data
         fakeResid = fakeValue / fakeLength) %>% # calculate fake value residualised by fake length
  mutate(condition = case_when(experiment == "first" & condition == "Baseline" ~ "Gap", # exp1 has two conditions: Gap and RP
                               experiment == "first" & condition == "Treatment" ~ "Resumptive Pronoun",
                               experiment == "second" & condition == "Baseline" ~ "Old form", # exp 2 has two conditions: Old and New
                               experiment == "second" & condition == "Treatment" ~ "New form",
                               TRUE ~ as.character(condition) # exp3 conditions stay the same
  )
  ) %>%
  ######
transmute(subject = subject, # subject stays the same
          condition = as.factor(condition), # convert condition to a factor
          residualValue = fakeResid) -> newData



head(newData)

long %>%
  spread(key=measure,value=value) %>%
  ######
separate(Savings,into=c("amount","currency"),sep=" ")



long %>%
  spread(key=measure,value=value) %>%
  separate(Savings,into=c("amount","currency"),sep=" ") %>%
  ######
mutate(Age = as.integer(Age),
       amount = as.numeric(amount))




iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour=Species)) +
  geom_smooth(method = "lm")
