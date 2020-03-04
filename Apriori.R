library(arules)
library(arulesViz)
library(datasets)

dt <- read.csv(file = "MOCK_DATA.csv")
head(dt)

#converting data into transactions 
train <- sapply(dt,as.factor)
train <- data.frame(train, check.names=FALSE)
txns <- as(train,"transactions")

arules::inspect(head(txns))

#applying apriori with support and confidence
apriorirules <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.6, minlen=2,target = "rules"))
arules::inspect(head(sort(apriorirules, by="lift")))

#rounding up rules with 3 decimal points
quality(apriorirules) <- round(quality(apriorirules), digits = 3)
arules::inspect(head(sort(apriorirules, by="lift")))

####finding interesting rules by giving consequent(RHS) manually with support = 0.09 and conf = 0.57

apriorirules1.milk <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("milk=true"),default="lhs"))
apriorirules1.butter <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("butter=true"),default="lhs"))
apriorirules1.pasta <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("pasta=true"),default="lhs"))
apriorirules1.shrimp <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("shrimp=true"),default="lhs"))
apriorirules1.Chicken <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("Chicken=true"),default="lhs"))
apriorirules1.Yogurt <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("Yogurt=true"),default="lhs"))
apriorirules1.Rice <- arules::apriori(txns, parameter = list(support=0.09,confidence=0.57, minlen=2), appearance = list(rhs=c("Rice=true"),default="lhs"))

##**#inspecting those rules by sorting with R
arules::inspect(head(sort(apriorirules1.milk, by="lift")))
arules::inspect(head(sort(apriorirules1.butter, by="lift")))
arules::inspect(head(sort(apriorirules1.pasta, by="lift")))
arules::inspect(head(sort(apriorirules1.shrimp, by="lift")))
arules::inspect(head(sort(apriorirules1.Chicken, by="lift")))
arules::inspect(head(sort(apriorirules1.Yogurt, by="lift")))
arules::inspect(head(sort(apriorirules1.Rice, by="lift")))



# #code for finding out redundant rules and removing redundant rules(not that usable)
# redundant <- is.redundant(apriorirules, measure="confidence")
# which(redundant)
# apriorirules.pruned <- apriorirules[!redundant]
# apriorirules.pruned <- sort(apriorirules.pruned, by="lift")
# inspect(apriorirules.pruned)


plot(v,type = "o",col = "red", xlab = "rhs", ylab = "lhs", 
     main = "Rain fall chart")

#plotting the rules
plot(apriorirules1.milk, method = "grouped")
plot(apriorirules1.butter, method = "grouped")
plot(apriorirules1.pasta, method = "grouped")
plot(apriorirules1.shrimp, method = "grouped")
plot(apriorirules1.Chicken, method = "grouped")
plot(apriorirules1.Yogurt, method = "grouped")
plot(apriorirules1.Rice, method = "grouped")

