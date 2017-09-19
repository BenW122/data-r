# This code replicates the results of the Principal Component Analysis on handwritten digit data
# illustrated in Elements of Statistical Learning by Hastie, Tibshirani
# 2nd edition, 12th print, pp. 536-539.

# install.packages("ElemStatLearn")
library("ElemStatLearn")
library("ggplot2")

# function to show a single 16x16 handwritten digit
show.digit <- function(digit, col=grey.colors(255)) {
  image(matrix(as.numeric(digit), nrow=16)[, 16:1], col=col)
}

# import handwritten digits data
digits <- as.data.frame(zip.train)
# only select digits displaying the number 3
digits <- subset(digits, V1 == 3)
digits <- as.matrix(digits[, -1])

# show first digit
show.digit(digits[1, ])

# Principal Component Analysis
# Note: setting scale to TRUE is recommended
# scale is set to FALSE to reproduce the results from ElemStatLearn
prin.comp <- prcomp(digits, scale = FALSE)

# summary: 
# first 12 components account for approx. 63% of the variation in the threes
# first 50 components account for approx. 90% of the variation in the threes
# See ElemStatLearn p. 537
summary(prin.comp)

# compute standard deviation of each principal component
standard.deviation <- prin.comp$sdev

# compute variance explained by each principal component
variance <- standard.deviation^2

# proportion of variance explained
prop.variance <- variance / sum(variance)

# Scree Plots
pc.variances <- data.frame(variances=prop.variance*100, pcomp=1:length(standard.deviation)) 
cum.pc.variances <- data.frame(variances=cumsum(prop.variance*100), pcomp=1:length(standard.deviation))

var.plot <- ggplot(pc.variances, aes(x = pcomp, y = variances)) + 
  geom_line() + geom_point() +
  labs(x = "Principal Component", y = "Proportion of Variance Explained %")

cum.var.plot <- ggplot(cum.pc.variances, aes(x = pcomp, y = variances)) + 
  geom_line() + geom_point() +
  labs(x = "Principal Component", y = "Cumulative Proportion of Variance Explained %")

# show plots
var.plot
cum.var.plot

### Additional 2D Plot PC1 and PC2

pc1 <- as.numeric(prin.comp$x[,1])
pc2 <- as.numeric(prin.comp$x[,2])

plot(pc1, pc2, pch=20)
text(pc1, pc2, 1:length(pc1), cex=0.6, pos=4, col="blue") 

# 164, 286
# 128, 652

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
show.digit(digits[164, ])
show.digit(digits[286, ])
show.digit(digits[128, ])
show.digit(digits[652, ])
par(op)
