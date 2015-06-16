library(arules)
data("Groceries")

## Mine rules.
rules <- apriori(Groceries, parameter=list(support=.0002))

## Select a subset of rules using partial matching on the items
## in the right-hand-side and a quality measure
rules.sub <- subset(rules, lift >  5 & support < .0008)

## Display rules.
inspect(sort(rules.sub, by = "lift"))
