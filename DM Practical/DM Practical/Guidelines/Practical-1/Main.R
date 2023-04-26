people <-read.table("D:/people.txt",header= TRUE)
View(people)

#install package editrules and arules 
library(editrules)
library(arules)

E<-editset(expression(
  Age>=0,
  Age<=150,
  status %in% c('married','single','widowed'),
  if(Age<=18) agegroup %in% c('child'),
  if(Age>18 && Age<65) agegroup %in% c('adult'),
  if(Age>=65) agegroup %in% c('elderly')
))
E
violations <-violatedEdits(E, people)
violations

summary(violations)
plot(violations)

