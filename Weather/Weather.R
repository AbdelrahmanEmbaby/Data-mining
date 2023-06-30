###Decision Tree Algorithm###
###packages
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
###import dataset
weather <- read.csv(file.choose(),header = T)
head(weather)
###creating instance of Decision tree classifier
DT_clf <- rpart(play~.,data=weather)
rpart.plot(DT_clf)
###prediction
pred <- predict(DT_clf, newdata=weather, type='class')
pred
###calculations
result <- table(weather$play,pred,dnn=list("Actual","Predicted"))
print("Confusion matrix")
result
tp <- result[2,2]
fp <- result[1,2]
tn <- result[1,1]
fn <- result[2,1]
cat("True positive: ",tp)
cat("False positive: ",fp)
cat("True negative: ",tn)
cat("false negative: ",fn)
p <- tp+fn
cat("Positive: ",p)
n <- tn+fp
cat("Negative: ",n)
acc <- (tp+tn)/(p+n)
cat("Accuracy: ",acc)
err <- 1-acc
cat("Error: ",err)
precision <- tp/(tp+fp)
cat("Precision: ",precision)
recall <- tp/p
cat("Recal or Sensitivity: ",recall)
F1_score <- (2*precision*recall)/(precision+recall)
cat("F1 Score: ",F1_score)
###ROC graph
roc <- roc(weather$play~(as.numeric(pred)), plot=T, print.auc = T)
