library(psych)
Ques1 <- function(){
  cars
  str(cars)
  cat("\nAvg speed : ",mean(cars$speed))
  cat("\nAvg distance : ",mean(cars$dist))
  cat("\nGeometric mean : \n")
  print(geometric.mean(cars))
  cat("\nHarmonic mean : \n")
  print(harmonic.mean(cars))
  cat("Unique values in dist : \n",unique(cars$dist))
  cat("\nVarience in speed : ",var(cars$speed))
  cat("\nVarience in dist : ",var(cars$dist))
  cat("\nIQR in speed : ",IQR(cars$speed))
  cat("\nQuartiles in distance :\n")
  print(quantile(cars$dist))
}

Ques2 <- function(){
  a <- read.csv("/home/deeksha/Documents/6th sem/DM/DM_Practicals/DataPreprocessing/Dirty android1 data.csv")
  cat("\nNA count : ",sum(is.na(a)))
  s <- sum(complete.cases(a))
  p <- s/nrow(a)*100
  cat("\nNon Empty Rows : ",sum(complete.cases(a)),"\npercentage of complete rows : ",p)
  cat("\nRows With Empty Column LCOM3 : ",which(is.na(a$LCOM3)))
  cat("\nColumn Mean : \n")
  print(colMeans(a[,-19],na.rm=TRUE))
  for(i in 1:ncol(a[,-19])){
    a[is.na(a[,i]), i] <- mean(a[,i], na.rm = TRUE)
  }
  cat("\nColumn Mean : \n")
  print(colMeans(a[,-19]))
  data_again<-read.csv("/home/deeksha/Documents/6th sem/DM/DM_Practicals/DataPreprocessing/Dirty android1 data.csv")
  na.omit(data_again)
  data_again
}

repeat {
  cat("\nEnter 1 for Ques1")
  cat("\nEnter 2 for Ques2")
  cat("\nEnter 3 for exit\n")
  c <- readline("Enter your choice : ")
  if(c == '3'){
    break
  }
  if(c == '1')
    Ques1()
  if(c == '2')
    Ques2()
}
