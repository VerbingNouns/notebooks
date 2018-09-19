data <- read.csv("data/binomial-data.csv", as.is = TRUE)
data$condition <- as.factor(data$condition)

head(data)
tail(data)

unique(data$experiment)
unique(data$item)

length(unique(data$item))

class(data$experiment)
class(data$item)

class(data$condition)
unique(data$condition)

str(data)
one <- "one"
str(one)

lotsOfNumbers <- 1:500


head(data[data$experiment=="second",]) # now i'm writing something that will need to wrap around
# to be able to read the whole thing

