#importing dataset
data <-read.csv(file.choose(),header = TRUE)
View(data)
str(data)
data$NSPF <- factor(data$NSP)
str(data)
set.seed(1234)
pd <- sample(2,nrow(data),replace = TRUE, prob = c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]
#decision tree with party
library(party)
tree <- ctree(NSPF~LB+AC+FM , data = train)
plot(tree)
tree <- ctree(NSPF~LB+AC+FM , data = train, controls = ctree_control(mincriterion = 0.99,minsplit = 500)
plot(tree)
predict(tree,validate, type = "prob")
#predict
predict(tree,validate)
#decision tree with rpart
library(rpart)
tree1 <- rpart(NSPF~LB+AC+FM,train)
plot(tree1)
library(rpart.plot)
rpart.plot(tree1)
rpart.plot(tree1,extra=1)
testpred <- predict(tree, newdata= validate)
tab <- table(testpred,validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)


