library(readr)

train_set <- read_csv("mnist_train.csv", col_names = FALSE)
test_set <- read_csv("mnist_test.csv", col_names = FALSE)
train_labels <- as.factor(train_set[, 1]$X1)
test_labels <- as.factor(test_set[, 1]$X1)
head(train_labels, 20)

head(train_set[,-1])
digits.pca <- prcomp(train_set[,-1], rank=14)
print(digits.pca)
plot(digits.pca, type = "l")
summary(digits.pca)

require(mclust)
digits.clust <- Mclust(digits.pca$x, G=15:20)
summary(digits.clust)

table(train_labels, digits.clust$classification)
