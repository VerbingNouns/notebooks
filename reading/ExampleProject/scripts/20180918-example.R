x <- 3
y = 4
x + y -> z
z< -x+y

longText <- c("i don't think we'll be able to fit this text on one line of the output console","so that it might wrap around and display on two lines")
print(longText)

data <- read.csv("data/binomial-data.csv")
head(data)
tail(data)

longText[2]

unique(data$experiment)
length(longText)

length(data$experiment)

data[4,1]
data[4,]
data[,1]


paste("Hello","world!") # the paste() function connects text values with one space.


c("one","two","three")

one <- "one"
two <- 2
three <- 3

allThree <- c(one,two,three)
justNumbers <-c(two,three)
