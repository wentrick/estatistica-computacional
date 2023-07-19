pacman::p_load(tidyverse,glmnet,MASS,GGally)

set.seed(42)
# create the variance covariance matrix
sigma <- matrix(c(1 , -0.8,-0.7,
                 -0.8, 1 ,0.9,
                 -0.7, 0.9 , 1),nrow = 3,byrow=TRUE)
# create the mean vector
mu<-c(10, 5, 2) 
# generate the multivariate normal distribution
df<-as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
ggpairs(df)
























