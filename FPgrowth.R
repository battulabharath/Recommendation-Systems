library(arules)
library(arulesViz)
library(datasets)
library(rCBA)

dt <- read.csv(file = "MOCK_DATA.csv")
head(dt)
train <- sapply(dt,as.factor)
train <- data.frame(train, check.names=FALSE)
txns <- as(train,"transactions")

#applying fpgrowth on trnsactional data with different consequent values
fpgrowthrules.milk <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="milk")
fpgrowthrules.butter <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="butter")
fpgrowthrules.pasta <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="pasta")
fpgrowthrules.shrimp <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="shrimp")
fpgrowthrules.Chicken <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="Chicken")
fpgrowthrules.Yogurt <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="Yogurt" )
fpgrowthrules.Rice <- rCBA::fpgrowth(txns, support = 0.09, confidence = 0.5, maxLength=2, consequent="Rice")

#inspecting those rules by sorting with lift
arules::inspect(sort(fpgrowthrules.milk, by="lift"))
arules::inspect(sort(fpgrowthrules.butter, by="lift"))
arules::inspect(head(sort(fpgrowthrules.pasta, by="lift")))
arules::inspect(sort(fpgrowthrules.shrimp, by="lift"))
arules::inspect(head(sort(fpgrowthrules.Chicken, by="lift")))
arules::inspect(head(sort(fpgrowthrules.Yogurt, by="lift")))
arules::inspect(sort(fpgrowthrules.Rice, by="lift"))



rules = rCBA::fpgrowth(txns, support=0.03, confidence=0.03, maxLength=2, consequent="milk",parallel=FALSE)

uu <- rCBA::buildFPGrowth(txns, className = 'milk')

#plotting the rules of fpgrowth.chicken(consequent=chicken)
plot(fpgrowthrules.Chicken, method ="grouped")

#plotting for remaining consequent rules
plot(fpgrowthrules.milk, method ="grouped")
plot(fpgrowthrules.butter, method ="grouped")
plot(fpgrowthrules.pasta, method = "grouped")
plot(fpgrowthrules.shrimp, method = "grouped")
plot(fpgrowthrules.Yogurt, method ="grouped")
plot(fpgrowthrules.Rice, method = "grouped")



