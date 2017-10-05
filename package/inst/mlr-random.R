##### This code manually applies a Monte Carlo Cross Validation using a
##### classical holdout structure drawn randomly.

### Libraries

library(mlr)

### Data

load(url("http://vault.data-r.com/data/churn.rda"))
churn <- churn[,-7]
churn$churn <- as.factor(churn$churn)

### Machine Learning Process

# Parameters

n <- nrow(churn)
n_repeat <- 5 # how often to split randomly into training/test
n_training <- 2/3 # size of training set

# Create Task

task <- makeClassifTask(data = churn, target = "churn")

# Compute acc and f1 for a list of classifiers

learners <- list("classif.rpart", "classif.randomForestSRC", "classif.naiveBayes", "classif.gbm")

m.acc <- matrix(ncol=length(learners))
m.f1 <- matrix(ncol=length(learners))

for(iteration in 1:n_repeat) {
  p_acc <- c()
  p_f1 <- c()
  runtime <- c(Sys.time())
  for(learner in learners) {
    train.set <- sample(n, size = n_training * n)
    test.set <- setdiff(1:n, train.set)
    model <- train(learner, task, subset = train.set)
    prediction <- predict(model, task = task, subset = test.set)
    current_performance <- performance(prediction, measures = list(acc, f1))
    p_acc <- c(p_acc, current_performance[1])
    p_f1 <- c(p_f1, current_performance[2])
  }
  
  m.acc <- rbind(m.acc, p_acc)
  m.f1 <- rbind(m.f1, p_f1)
}

result.acc <- apply(m.acc, 2, function(x) { mean(x, na.rm=TRUE) })
names(result.acc) <- learners

result.f1 <- apply(m.f1, 2, function(x) { mean(x, na.rm=TRUE) })
names(result.f1) <- learners
