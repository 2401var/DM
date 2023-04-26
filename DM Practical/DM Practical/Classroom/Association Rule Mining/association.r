library(arules)
dataset=read.transactions("1000-out1.csv",sep=",",rm.duplicate=TRUE)
dataset
summary(dataset)
itemFrequencyPlot(dataset,topN=10)
rules=apriori(data=dataset,parameter=list(support=0.5,confidence=0.75))
rules=apriori(data=dataset,parameter=list(support=0.6,confidence=0.6))
rules=apriori(data=dataset,parameter=list(support=0.5,confidence=0.6))
