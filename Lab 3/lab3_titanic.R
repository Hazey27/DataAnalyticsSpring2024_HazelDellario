data(Titanic)

library("rpart")

# rpart, ctree, hclust
# for Survived ~.


str(Titanic)
# all column values are strings when they should be FACTORS

Titanic = data.frame(Titanic)
Titanic$Class = as.factor(Titanic$Class)
Titanic$Sex = as.factor(Titanic$Sex)
Titanic$Age = as.double(Titanic$Age)
Titanic$Survived = as.factor(Titanic$Survived)
Titanic$Freq = as.double(Titanic$Freq)

# rpart ----------------------------------------------------------------------------------
titanic_rpart = rpart(Survived ~ ., data = Titanic)
plot(titanic_rpart)
text(titanic_rpart)

printcp(titanic_rpart) # display the results
plotcp(titanic_rpart)
summary(titanic_rpart)
par(mfrow=c(1,2)) 
rsq.rpart(titanic_rpart) # visualize cross-validation results
# plot tree
plot(titanic_rpart, uniform=TRUE, main="Regression Tree for Survival Rate")
text(titanic_rpart, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
ptitanic_rpart<- prune(titanic_rpart, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(ptitanic_rpart, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(ptitanic_rpart, use.n=TRUE, all=TRUE, cex=.8)
post(ptitanic_rpart, file = "ptree2.ps", title = "Pruned Regression Tree for Mileage")

plot(titanic_rpart,compress=TRUE)
text(titanic_rpart, use.n=TRUE)


# ctree ----------------------------------------------------------------------

# style of data doesn't work with this function
# titanic_tree = ctree(Survived ~ ., data = Titanic) # this doesn't look right
# plot(titanic_tree)
# library("party")
# party.tree <-ctree(Survived ~ ., data = Titanic)
# plot(party.tree)


cforest(Survived ~ ., data = Titanic, controls=cforest_control(mtry=2, mincriterion=0))

library(tree)
tree_pkg <- tree(Survived ~ ., data = Titanic)
tree_pkg
tree_pkg$frame
plot(tree_pkg)
text(tree_pkg)
#find "prettier" ways to plot the tree



# hclust ----------------------------------------------------------------------

titanic.dist = stats::dist(Titanic)
titanic.hclust = hclust(titanic.dist)

dev.off()
plot(titanic.hclust)

summary(titanic.hclust)


titanic.dendrogram = as.dendrogram(titanic.hclust)
plot(titanic.dendrogram)


library("ape")

titanic.phylo = as.phylo(titanic.hclust)


# I don't think any of these graphs are helpful for this data set but they may be helpful for other data types
plot(titanic.phylo)
plot(titanic.phylo, type = "cladogram")
plot(titanic.phylo, type = "unrooted")
plot(titanic.phylo, type = "fan")
plot(titanic.phylo, type = "radial")
