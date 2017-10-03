##### http://vault.data-r.com/img/over_under_fit.png

### Library

library(e1071) # Support Vector Machines
library(class) # kNN

### Data

load(url("http://vault.data-r.com/data/logregknn.rda"))

plot(data[which(data$class == 0), 1:2], col="blue", xlim=c(0,1), ylim=c(0,1))
points(data[which(data$class == 1), 1:2], col="orange")

### Evaluation Data (lambda-Grid)

lambda <- 0.01

test <- data.frame()
for(x in seq(0,1,by=lambda)) {
  for(y in seq(0,1,by=lambda)) {
    test <- rbind(test, c(x,y))
  }
}
names(test) <- c("V1", "V2")

### Logistic Regression

fit <- glm(class~V1+V2, data=data, family=binomial())
response <- predict(fit, test, type='response')
logreg.class <- rep(0, nrow(test))
logreg.class[which(response >= 0.5)] <- 1

plot(test[which(logreg.class == 0),], col="#bbdefb", xlim=c(0,1), ylim=c(0,1), pch=15)
points(test[which(logreg.class == 1),], col="#ffe0b2", pch=15)
points(data[which(data$class == 0), 1:2], col="#0d47a1")
points(data[which(data$class == 1), 1:2], col="#e65100")

### SVM

fit <- svm(class~V1+V2, data=data)
svm.class <- as.numeric(predict(fit, test, type='response')) - 1

plot(test[which(svm.class == 0),], col="#bbdefb", xlim=c(0,1), ylim=c(0,1), pch=15)
points(test[which(svm.class == 1),], col="#ffe0b2", pch=15)
points(data[which(data$class == 0), 1:2], col="#0d47a1")
points(data[which(data$class == 1), 1:2], col="#e65100")

### kNN

k <- 7
knn.class <- class::knn(data[,1:2], test, data[,3], k)

plot(test[which(knn.class == 0),], col="#bbdefb", xlim=c(0,1), ylim=c(0,1), pch=15)
points(test[which(knn.class == 1),], col="#ffe0b2", pch=15)
points(data[which(data$class == 0), 1:2], col="#0d47a1")
points(data[which(data$class == 1), 1:2], col="#e65100")
