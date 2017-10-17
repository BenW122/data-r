### Libraries

library(stringr)
library(dplyr)

### Data 

load(url("http://vault.data-r.com/data/onlineretail.rda"))

### Data cleansing & selection

# Extract Day from date and remove Time

Day <- as.Date(substr(onlineretail$InvoiceDate,1, 8), "%d/%m/%y")
onlineretail <- cbind(onlineretail, Day)

# String replacements (price) and conversion to numeric

onlineretail$UnitPrice <- as.numeric(str_replace(onlineretail$UnitPrice, ",", "."))

### RMF Analysis (Recency-Monetary-Frequency)

RMFAnalysis <- summarise(group_by(onlineretail, CustomerID), 
                         Frequency = n(), 
                         Recency.date = max(Day),
                         Monetary.mean = mean(Quantity * UnitPrice, na.rm = TRUE), 
                         Monetary.sum = sum(Quantity * UnitPrice, na.rm = TRUE))

RMFAnalysis <- cbind(RMFAnalysis, as.numeric(max(RMFAnalysis$Recency.date) - RMFAnalysis$Recency.date))
names(RMFAnalysis)[ncol(RMFAnalysis)] <- "Recency"

# Data cleansing: remove some problematic customers

RMFAnalysis.clean <- filter(RMFAnalysis, Monetary.mean > 0, Monetary.sum > 0, Monetary.sum < quantile(RMFAnalysis$Monetary.sum, 0.99))

# Select and scale attributes for RMF clustering

RMFAnalysis.select <- select(RMFAnalysis.clean, Frequency, Monetary.sum, Recency)
RMFAnalysis.scale <- scale(RMFAnalysis.select) 

### Cluster Analysis (Customer Segmentation)

# Scree Plot

max_clusters <- 15
wss <- (nrow(RMFAnalysis.scale)-1)*sum(apply(RMFAnalysis.scale,2,var))
for (i in 2:max_clusters) wss[i] <- sum(kmeans(RMFAnalysis.scale, centers=i)$withinss)
plot(1:max_clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# Clustering

clusters <- 4 # manually chosen from scree plot

fit <- kmeans(RMFAnalysis.scale, clusters)
print(aggregate(RMFAnalysis.select, by=list(fit$cluster), FUN=mean))

RMFAnalysis <- data.frame(RMFAnalysis.clean, fit$cluster)
names(RMFAnalysis)[ncol(RMFAnalysis)] <- "Cluster"
