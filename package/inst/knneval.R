library(class)

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
classes <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

knn.result <- knn(train, test, classes, k=3)

result <- data.frame(classes, knn.result)
names(result) <- c("data", "prediction")

cm <- table(result$data, result$prediction) # confusion matrix

n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class 
n_correct <- sum(diag)
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

misclassified <- n - n_correct
accuracy <- n_correct / n 

# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is defined as the harmonic mean (or a weighted average) of precision and recall.

precision <- diag / colsums 
recall <- diag / rowsums 
f1 <- 2 * precision * recall / (precision + recall) 

# Per-class metrics can be averaged over all the classes resulting in 
# macro-averaged precision, recall and F-1.

macro.precision <- mean(precision)
macro.recall <- mean(recall)
macro.f1 <- mean(f1)
