######################
## Seminar 7        ##
## Michal Kubista   ##
## 24 February 2021 ##
######################

install_and_load = function(name, char = T) {
  if (!require(name, character.only = char)) {
    install.packages(name)
  }
  require(name, character.only = char)
}

sapply(
    c("data.table","tidyverse","magrittr", "GGally",
      "rpart", "rattle", "randomForest", "forestFloor",
      "caret"),
    install_and_load
)

# devtools::install_version("forestFloor", version = "1.11.1")
# library("forestFloor")

#-- PART 1 - CHURN ############################################################
#--- 1.1 ETL -------------------------------------------------------------------
# downloaded from https://www.kaggle.com/blastchar/telco-customer-churn
telco = fread("w7/data/WA_Fn-UseC_-Telco-Customer-Churn.csv",
              colClasses = c(rep("factor", 5),
                             "numeric",
                             rep("factor", 12),
                             rep("numeric", 2),
                             "factor")
              )

str(telco)
summary(telco)
View(telco)

#--- 1.2 TRAIN / TEST ----------------------------------------------------------
set.seed(12345)

index = sample(nrow(telco), size =  0.8 * nrow(telco))
train = telco[index,] %>% as.data.frame()
test = telco[-index,] %>% as.data.frame()

summary(train[,-1])
summary(test[,-1])

rm(index)

#--- 1.2 TREE ------------------------------------------------------------------
## simple tree
tree = rpart(Churn ~ ., train[,-1])
fancyRpartPlot(tree)

tree$variable.importance
summary(tree)

## tweaking the controls
cont = rpart.control(minsplit = 1, minbucket = 1, cp = 1e-10)
tree = rpart(Churn ~., train[,-1], control = cont)
tree
tree$cptable

## prune tree (1 SE rule)
tree_pruned = prune(tree, 0.00372882)
tree_pruned = prune(tree, 0.00474577)
fancyRpartPlot(tree_pruned)

## trees train accuracy
pred = as.data.frame(predict(tree))
sum(ifelse(pred$No > 0.5, "No", "Yes") == train$Churn) / nrow(train)

pred_prune = as.data.frame(predict(tree_pruned))
sum(ifelse(pred_prune$No > 0.5, "No", "Yes") == train$Churn) / nrow(train)

## pruned tree test accuracy
full = as.data.frame(predict(tree, test))
sum(ifelse(full$No > 0.5, "No", "Yes") == test$Churn) / nrow(test)

pruned = as.data.frame(predict(tree_pruned, test))
test$pred = ifelse(pruned$No > 0.5, "No", "Yes")
sum(test$Churn == test$pred)/nrow(test)

## good result?
sum(test$Churn == "No") / nrow(test)
table(true = test$Churn, pred = test$pred)

rm(tree, tree_pruned, pruned, cont, pred, pred_prune, full)
test$pred = NULL

#--- 1.3 FOREST ----------------------------------------------------------------
rf = randomForest(Churn ~ ., train[,-1])

## HEEEEELP!

#---- 1.3.1 randomFOREST -------------------------------------------------------
rf = randomForest(Churn ~ ., train[,-1], importance = TRUE, keep.inbag = TRUE)
varImpPlot(rf)

## train performance
rf$confusion
sum(rf$predicted == train$Churn)/nrow(train)

## test performance
test$pred = predict(rf, test[,-1])
sum(test$pred == test$Churn)/nrow(test)
table(true = test$Churn, pred = test$pred)

#---- 1.3.2 CARET aka GET A SERVER ---------------------------------------------
metric = "Accuracy"

## OOB vs CV vs repeatedCV
control <- trainControl(method = "oob", number = 5, search = "random")
rf_random <- train(Churn ~ ., data = train[,-1], method = "rf", metric = metric,
                   tuneLength = 10, trControl = control)
print(rf_random)
plot(rf_random)

##---- 1.3.3 OPTIMISE randomFOREST ---------------------------------------------
errors = c()
for (i in 3:10) {
    rf = randomForest(Churn ~ ., train[,-1], mtry = i, importance = TRUE,
                      keep.inbag = TRUE, ntrees = 2000)
    
    errors[length(errors) + 1] = 
        sum(rf$predicted == train$Churn)/nrow(train)
}
errors
which.max(errors)

## "best" forest visualisation
rf = randomForest(Churn ~ ., train[,-1], mtry = 4, importance = TRUE,
                  keep.inbag = T, ntrees = 2000)
rf$importance

ff <- forestFloor(rf, train[,-1], binary_reg = T)
plot(ff, col = fcol(ff, 1), plot_seq = 1:6)