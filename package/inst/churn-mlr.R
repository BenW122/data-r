### Libraries

library(mlr)

### Data

load(url("http://vault.data-r.com/data/churn.rda"))
churn <- churn[,-7]
churn$churn <- as.factor(churn$churn)

### Machine Learning Process

# Create Task

task <- makeClassifTask(data = churn, target = "churn")

# Define the learner

learner <- makeLearner("classif.rpart")

n <- nrow(churn)
train.set <- sample(n, size = 2/3*n)
test.set <- setdiff(1:n, train.set)

# Fit the model

model <- train(learner, task, subset = train.set)

# Make predictions

prediction <- predict(model, task = task, subset = test.set)

# Evaluate the learner

accuracy <- performance(prediction, measures = list(acc))
print(accuracy)

###

learners <- list("classif.rpart", "classif.randomForestSRC", "classif.naiveBayes", "classif.gbm")

p_acc <- c()
p_f1 <- c()
runtime <- c(Sys.time())
for(learner in learners) {
  model <- train(learner, task, subset = train.set)
  runtime <- c(runtime, Sys.time())
  prediction <- predict(model, task = task, subset = test.set)
  current_performance <- performance(prediction, measures = list(acc, f1))
  p_acc <- c(p_acc, current_performance[1])
  p_f1 <- c(p_f1, current_performance[2])
}

print(p_acc)
print(p_f1)
