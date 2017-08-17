setwd("C:/Users/buri/Desktop/R practice")

# Outlier detection
# LOF (Local Outlier Factor) is an algorithm for identifying density-based local outliers 
# With LOF, the local density of a point is compared with that of its neighbors. If the 
# former is signi.cantly lower than the latter (with an LOF value greater than one), the 
# point is in a sparser region than its neighbors, which suggests it be an outlier

library(DMwR)
# Remove categorical column "species"
iris2 <- iris[,1:4]
head(iris2)
outlier.scores <- lofactor(iris2, k=5)
lofscores <- head(lofactor(iris[,-5],10))
plot(density(outlier.scores))

# Top5 outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]

# Package Rlof provides function lof(), a parallel implementation of the LOF algorithm. 
# Its usage is similar to the above lofactor(), but lof() has two additional features of 
# supporting multiple values of k and several choices of distance metrics
library(Rlof)
outlier.scores <- head(lof(iris2, k=c(5:10)))

# Clustering using dendogram on Iris data
datasets::iris
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])