#fileurl <- 'http://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv'
#download.file(fileurl, destfile = './data/dirty_iris.csv')

dirty_iris<-read.csv("D:/iris_dirty.csv")
View(dirty_iris)
s<-sum(complete.cases(dirty_iris))
cat('Num of observations: ', s)
cat('Percentage: ',
    s / nrow(dirty_iris) * 100)

# Define special values to replace
special_values <- c("?", "-", "N/A", "NA", "null", "NaN")
# Replace all special values with NA
dirty_iris[dirty_iris %in% special_values] <- NA
View(dirty_iris)

library(editrules)
E <-editfile("D:/q2Rules.txt")
E
violationq2 = violatedEdits(E, dirty_iris)
violationq2

summary(violationq2)
plot(violationq2)

boxplot(dirty_iris$Sepal.Length)
boxplot.stats(dirty_iris$Sepal.Length)$out
