###KNN Algorithm###
###packages
library(caTools)
library(class)
library(ggplot2)
library(pROC)
###import dataset
iris <- read.csv(file.choose(),header = T)
iris <- subset( iris, select = -Id )
iris$Species = as.numeric(as.factor(iris$Species))
head(iris)
###splitting train and test samples
sample <- sample.split(iris$Species, SplitRatio = 0.8)
train  <- subset(iris, sample == TRUE)
test   <- subset(iris, sample == FALSE)
train
test
###creating instance of KNN classifier
KNN_clf <- knn(train = train,
               test = test,
               cl = train$Species,
               k = 1)


###prediction
pred <- KNN_clf
pred
###calculations
result <- table(test$Species,pred,dnn=list("Actual","Predicted"))
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
roc <- roc(test$Species~(as.numeric(pred)), plot=T, print.auc = T)
