### Libraries

library(stringr)
library(arules)
library(tidyverse)
library(cluster)
library(rpart)
library(rpart.plot)
library(recommenderlab)

### Data

load(url("http://vault.data-r.com/data/onlineretail.rda"))
dataset <- onlineretail

### Split Date and Time

Day <- as.Date(substr(dataset$InvoiceDate,1, 8), "%d/%m/%y")
Time <- substr(as.character(dataset$InvoiceDate), 10, 14)

dataset <- cbind(dataset, Day, Time)

### Keep UK only

pos_uk <- which(dataset$Country == "United Kingdom")
dataset <- dataset[pos_uk,]
dataset <- as_tibble(dataset)
dataset$UnitPrice <- as.numeric(str_replace(dataset$UnitPrice, ",", "."))

# RMF Analysis (Recency-Monetary-Frequency)

RMFAnalysis <- summarise(group_by(dataset, CustomerID), 
                         Frequency = n(), 
                         Recency.date = max(Day),
                         Monetary.mean = mean(Quantity * UnitPrice, na.rm = TRUE), 
                         Monetary.sum = sum(Quantity * UnitPrice, na.rm = TRUE))

RMFAnalysis <- cbind(RMFAnalysis, max(RMFAnalysis$Recency.date) - RMFAnalysis$Recency.date)
names(RMFAnalysis)[ncol(RMFAnalysis)] <- "Recency"

### Remove some problematic Customers 

pos_problem <- c(which(RMFAnalysis$Monetary.mean <= 0), 
                 which(RMFAnalysis$Monetary.sum <= 0),
                 which(RMFAnalysis$Monetary.sum >= quantile(RMFAnalysis$Monetary.sum, 0.99)))

RMFAnalysis <- na.omit(RMFAnalysis[-pos_problem,])

RMFAnalysis.select <- RMFAnalysis[,c(2,5,6)]
RMFAnalysis.scale <- data.frame(as.numeric(scale(RMFAnalysis.select[,1])), 
                                as.numeric(scale(RMFAnalysis.select[,2])), 
                                as.numeric(scale(RMFAnalysis.select[,3])))  

### Descriptive: Correlations

cor(as.numeric(RMFAnalysis$Recency), RMFAnalysis$Frequency)
cor(as.numeric(RMFAnalysis$Frequency), RMFAnalysis$Monetary.mean)

### Cluster Analysis (Customer Segmentation)

# Scree Plot

max_clusters <- 15
wss <- (nrow(RMFAnalysis.scale)-1)*sum(apply(RMFAnalysis.scale,2,var))
for (i in 2:max_clusters) wss[i] <- sum(kmeans(RMFAnalysis.scale, centers=i)$withinss)
plot(1:max_clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# Analysis per Cluster

clusters <- 4

fit <- kmeans(RMFAnalysis.scale, clusters)
print(aggregate(RMFAnalysis.select, by=list(fit$cluster), FUN=mean))

RMFAnalysis <- data.frame(RMFAnalysis, fit$cluster)

for(cluster in 1:clusters) {
  cluster_data <- RMFAnalysis[which(RMFAnalysis$fit.cluster==cluster),]
  
  ### Classification / Decision Trees
  
  fit <- rpart(Monetary.sum ~ Frequency + Recency, data=cluster_data, method="anova")
  rpart.plot(fit)
  
  ### Association Rules
  
  pos_transaction <- which(dataset$CustomerID %in%
                           RMFAnalysis$CustomerID[which(RMFAnalysis$fit.cluster==cluster)])
  write.csv2(dataset[pos_transaction,], file="transactions.txt", row.names = FALSE)
  trans <- read.transactions("transactions.txt", format="single", sep=";", cols=c(1,3), rm.duplicates=TRUE)
  
  support <- 0.03
  confidence <- 0.5
  
  rules <- apriori(trans, parameter = list(supp = support, conf = confidence, target = "rules"))
  if(length(rules) > 0) { inspect(head(rules, n = 30, by = "confidence")) }
}
onlineretail.clean <- na.omit(onlineretail)

products <- as(data.frame(onlineretail.clean[,c(7, 2)]), "binaryRatingMatrix")
image(products[100:200,1:200])

recommender <- Recommender(products, method="POPULAR")

# Create recommendations using user ids with ids 1..10 in the training data
pre <- predict(recommender, 1:10, data = products, n = 10)
as(pre, "list")
