load("titanic.raw.rdata")

View(titanic.raw)
idx <- sample(1:nrow(titanic.raw),5)
titanic.raw[idx,]

summary(titanic.raw)

install.packages("arules")
library('arules')

rules.all <- apriori(titanic.raw)


inspect(rules.all)

rules <- apriori(titanic.raw,
                 
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))


quality(rules) <- round(quality(rules), digits=3)

rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

inspect(rules.sorted[1:2])

## find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted, , sparse = FALSE)
subset.matrix[ lower.tri(subset.matrix, diag = T) ] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
## which rules aredundant
which(redundant)

## [1] 2 4 7 8
## remove redundant rules
rules.pruned <- rules.sorted[!redundant]
rules.pruned

inspect(rules.pruned)

inspect(rules.pruned[1])

rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

install.packages("arulesViz")

library(arulesViz)
plot(rules.all)


plot(rules.all, method = "grouped")
plot(rules.all, method = "graph")
plot(rules.all, method = "graph", control = list(type = "items"))
plot(rules.all, method = "paracoord", control = list(reorder = TRUE))

install.packages('knitr', dependencies = TRUE)
library('knitr')
install.packages("rmarkdown")
install.packages("latexpdf")
library('latexpdf')

