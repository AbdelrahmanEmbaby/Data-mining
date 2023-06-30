###Naive Bayes Algorithm###
###packages
library(naivebayes)
library(caTools)
library(ggplot2)
library(pROC)
###import dataset
weather2 <- read.csv(file.choose(),header = T)
head(weather2)
###splitting train and test samples
sample <- sample.split(weather2$outlook, SplitRatio = 0.8)
train  <- subset(weather2, sample == TRUE)
test   <- subset(weather2, sample == FALSE)
train
test
###creating instance of Naive Bayes classifier
NB_clf <- naive_bayes(play ~ ., data = train)
plot(NB_clf)
###prediction
pred <- predict(NB_clf, newdata=test, type='class')
pred
###calculations
result <- table(test$play,pred,dnn=list("Actual","Predicted"))
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
roc <- roc(test$play~(as.numeric(pred)), plot=T, print.auc = T)



