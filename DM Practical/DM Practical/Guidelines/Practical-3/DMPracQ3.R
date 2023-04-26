
#Here, 2 specifies that the function should be applied to each column of the data frame.
#Setting MARGIN=2 means that we are operating on columns
#setting MARGIN=1 would mean that we operate on r
wine <- read.csv("C:/Users/HP/Desktop/DMpracs/wine (1).data")
apply(wine, 2, mean)   # should return a vector of zeros
apply(wine, 2, sd)     # should return a vector of ones
# Standardize the wine attributes
wine <- scale(wine)
wine
data(iris)
apply(iris[,-5], 2, mean)   # should return a vector of zeros
apply(iris[,-5], 2, sd)     # should return a vector of ones
# Standardize the wine attributes
Iris2 <- scale(iris[,-5])
View(Iris2)

