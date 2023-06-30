###KMeans clustring + KNN Algorithm###
###packages
library(ClusterR)
library(cluster)
library(caTools)
library(class)
library(ggplot2)
library(pROC)
###import dataset
customers <- read.csv(file.choose(),header = T)
customers <- subset( customers, select = -CustomerID )
customers$Genre = as.numeric(as.factor(customers$Genre))
head(customers)
###Applying KMeans clustering
KMeans <- kmeans(customers, centers = 4, nstart = 20)
customers$cluster = KMeans$cluster
customers
###splitting train and test samples
sample <- sample.split(customers$cluster, SplitRatio = 0.8)
train  <- subset(customers, sample == TRUE)
test   <- subset(customers, sample == FALSE)
train
test
###creating instance of KNN classifier
KNN_clf <- knn(train = train,
               test = test,
               cl = train$cluster,
               k = 1)


###prediction
pred <- KNN_clf
pred
###calculations
result <- table(test$cluster,pred,dnn=list("Actual","Predicted"))
print("Confusion matrix")
result
#tp <- result[2,2]
#fp <- result[1,2]
#tn <- result[1,1]
#fn <- result[2,1]
#cat("True positive: ",tp)
#cat("False positive: ",fp)
#cat("True negative: ",tn)
#cat("false negative: ",fn)
#p <- tp+fn
#cat("Positive: ",p)
#n <- tn+fp
#cat("Negative: ",n)
#acc <- (tp+tn)/(p+n)
#cat("Accuracy: ",acc)
#err <- 1-acc
#cat("Error: ",err)
#precision <- tp/(tp+fp)
#cat("Precision: ",precision)
#recall <- tp/p
#cat("Recal or Sensitivity: ",recall)
#F1_score <- (2*precision*recall)/(precision+recall)
#cat("F1 Score: ",F1_score)
###ROC graph
roc <- roc(test$cluster~(as.numeric(pred)), plot=T, print.auc = T)
