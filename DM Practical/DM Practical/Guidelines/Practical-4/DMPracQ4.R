library(arules)

data("Mushroom")
ep <-apriori(Mushroom, parameter = list(supp=0.5, conf=0.75))
summary(ep)
inspect(ep)

ep <-apriori(Mushroom, parameter = list(supp=0.6, conf=0.6))
summary(ep)
inspect(ep)

data("Income")
inc <-apriori(Income, parameter = list(supp=0.5, conf=0.75))
summary(inc)
inspect(inc)

inc <-apriori(Income, parameter = list(supp=0.6, conf=0.6))
summary(inc)
inspect(inc)

