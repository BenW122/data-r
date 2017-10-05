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

# Define the resampling (e.g. a 5-fold cross-validation)

resampling <- makeResampleDesc("CV", iters = 5)

# Fit and evaluate the model

result <- resample(learner, task, resampling, measures = list(acc, f1, timetrain))
result$measures.test
