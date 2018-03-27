# Load package arules
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# Load Transaction data
temp<-read.csv(file.choose(),header=F)
temp
fdmart<-split(temp$V2,temp$V1)
head(fdmart2)
# What type of data structure is Transaction data?
class(fdmart2)
Transc<-as(fdmart2,"transactions")
class(Transc)
# Summary of Transaction data. Most frequnt items
summary(Transc)
# plot frequencies of frequent items
itemFrequencyPlot(Transc, support=0.1, cex.names=0.8)
# Run ariori algorithm and generate rules
rules <- apriori(Transc, parameter = list(support=0.005, confidence=0.8))
# summary of rules
summary(rules)
# Inspect rules
inspect(rules)


# Subsets. find subset of rules that has Wine in the Right hand side

WineRulesrhs <- subset(rules, subset = rhs %pin% c("Wine"))
summary(WineRulesrhs)
inspect(WineRulesrhs)
inspect(head(sort(WineRulesrhs, by ="lift"),3))
plot(BeverageRulesWine, main="Scatter Plot for Wine")
plot(head(sort(WineRulesrhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)

# Subsets. find subset of rules that has Wine in the Left hand side

WineRuleslhs <- subset(rules, subset = lhs %pin% c("Wine"))
summary(WineRuleslhs)
inspect(WineRuleslhs)
inspect(head(sort(WineRuleslhs, by ="lift"),3))
plot(WineRuleslhs, main="Scatter Plot for Wine")
plot(head(sort(WineRuleslhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)


# Subsets. find subset of rules that has Beer in the Right hand side

BeerRulesrhs <- subset(rules, subset = rhs %pin% c("Beer"))
summary(BeerRulesrhs)
inspect(BeerRulesrhs)
inspect(head(sort(BeerRulesrhs, by ="lift"),3))
plot(BeerRulesrhs, main="Scatter Plot for Beer")
plot(head(sort(BeerRulesrhs,by="confidence"),5),method="graph",interactive=TRUE,shading="lift")


# Subsets. find subset of rules that has Beer in the Left hand side

BeerRuleslhs <- subset(rules, subset = lhs %pin% c("Beer"))
summary(BeerRuleslhs)
inspect(BeerRuleslhs)
inspect(head(sort(BeerRuleslhs, by ="lift"),3))
plot(BeerRuleslhs, main="Scatter Plot for Beer")
plot(head(sort(BeerRuleslhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)


# Subsets. find subset of rules that has Wine and Beer in the Left hand side

BeverageRuleslhs <- subset(rules, subset = lhs %ain% c("Wine", "Beer"))
summary(BeverageRuleslhs)
inspect(BeverageRuleslhs)


# Subsets. find subset of rules that has Wine and Beer in the Right hand side

BeverageRulesrhs <- subset(rules, subset = rhs %ain% c("Wine", "Beer"))
summary(BeverageRulesrhs)
inspect(BeverageRulesrhs)


# Subsets.find subset of rules that has Canned Vegetables in the Right hand side.

CannedVegRulesrhs <- subset(rules, subset = rhs %pin% c("Canned Vegetables"))
summary(CannedVegRulesrhs)
inspect(CannedVegRulesrhs)
inspect(head(sort(CannedVegRulesrhs, by ="lift"),3))
plot(CannedVegRulesrhs, main="Scatter Plot for Canned Vegetables")
plot(head(sort(CannedVegRulesrhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)


# Subsets.find subset of rules that has Canned Vegetables in the Left hand side.

CannedVegRuleslhs <- subset(rules, subset = lhs %pin% c("Canned Vegetables"))
summary(CannedVegRuleslhs)
inspect(CannedVegRuleslhs)
inspect(head(sort(CannedVegRuleslhs, by ="lift"),3))
plot(CannedVegRuleslhs, main="Scatter Plot for Canned Vegetables")
plot(head(sort(CannedVegRuleslhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)

# Subsets.find subset of rules that has Fresh Vegetables in the Right hand side.

FreshVegRulesrhs <- subset(rules, subset = rhs %pin% c("Fresh Vegetables"))
summary(FreshVegRulesrhs)
inspect(FreshVegRulesrhs)
inspect(head(sort(FreshVegRulesrhs, by ="lift"),3))
plot(FreshVegRulesrhs, main="Scatter Plot for Fresh Vegetables")
plot(head(sort(FreshVegRulesrhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)

# Subsets.find subset of rules that has Fresh Vegetables in the Left hand side.

FreshVegRuleslhs <- subset(rules, subset = lhs %pin% c("Fresh Vegetables"))
summary(FreshVegRuleslhs)
inspect(FreshVegRuleslhs)
inspect(head(sort(FreshVegRuleslhs, by ="lift"),3))
plot(FreshVegRuleslhs, main="Scatter Plot for Fresh Vegetables")
plot(head(sort(FreshVegRuleslhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)


# Subsets.find subset of rules that has Canned Vegetables and Fresh Vegetables in the Right hand side.

CnFVegRulesrhs <- subset(rules, subset = rhs %ain% c("Fresh Vegetables","Canned Vegetables"))
summary(CnFVegRulesrhs)
inspect(CnFVegRulesrhs)

# Subsets.find subset of rules that has Canned Vegetables and Fresh Vegetables in the Left hand side.

CnFVegRuleslhs <- subset(rules, subset = lhs %ain% c("Fresh Vegetables","Canned Vegetables"))
summary(CnFVegRuleslhs)
inspect(CnFVegRuleslhs)
inspect(head(sort(CnFVegRuleslhs, by ="lift"),3))
plot(CnFVegRuleslhs, main="Scatter Plot for Canned and Fresh Vegetables")
plot(head(sort(CnFVegRuleslhs,by="confidence"),5),method="graph",interactive=TRUE,shading=NA)
hist(CnFVegRuleslhs)


# Subsets.find subset of rules that has Small Transaction

SmalltransRules <- rules[size(items(rules))<=5]
summary(SmalltransRules)
inspect(SmalltransRules)
inspect(head(sort(SmalltransRules, by ="lift"),3))
plot(SmalltransRules, shading="order", control=list(main = "Plot for Small Transaction"))


# Subsets.find subset of rules that has Large Transaction

LargetransRules <- rules[size(items(rules))>5]
summary(LargetransRules)
inspect(LargetransRules)
inspect(head(sort(LargetransRules, by ="lift"),3))
plot(LargetransRules, shading="order", control=list(main = "Plot for Large Transaction"))


# Run ariori algorithm and generate rules

newrules <- apriori(Transc, parameter = list(support=0.02, confidence=0.7))
summary(newrules)
inspect(newrules)
inspect(head(sort(newrules, by ="lift"),10))
plot(rules1, method="matrix3D", measure="lift", control=list(reorder=TRUE))
