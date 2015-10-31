library(foreign)

data <- read.arff("communities.arff")[, 0:100]
data <- scale(data)
write.table(data, file="communities.csv", sep = ";",
            quote = FALSE, col.names = FALSE)