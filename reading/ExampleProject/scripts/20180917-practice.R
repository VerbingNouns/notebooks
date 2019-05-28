3+2+
  4
longText <- c("i don't think we'll be able to fit this text on one line of the output console","so that it might wrap around and display on two lines")
print(longText)

x <- 3 # or x=3
y <- 4 # or y<-4
x+y
 y - x
y^x
x/4

data <- read.csv("data/binomial-data.csv")

head(data)
tail(data)

unique(data$experiment)

length(unique(data$item))



head(data[data$experiment=="second",1:3])

data2 <- read.delim("filename.txt",sep=";")
