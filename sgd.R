#####code to implement Stochastic gradient descent in linear regression####
##Fake Data

x1 <- rnorm(100000)
x2 <- rnorm(100000)
e <- rnorm(100000)
y <- 5 + 4*x1 + 3*x2 + e
###do a least square###
fit <- lm(y ~ x1+x2)
summary(fit)

###do a Stochastic gradient descent####

sgd_linear <- function(x, y, n, alpha=0.001, seed_num=166) {
  
  beta <- rep(0, ncol(x)) ##already add bias term
  set.seed(seed_num)
  i=0
  m <- nrow(x)
  while (i < n) {
    sample_num <- sample(m,1)
    xx <- x[sample_num,]
    yy <- y[sample_num]
    beta <- beta - alpha * (xx %*% beta - yy) * xx 
    i = i + 1
  }
  
  return (beta)
}

x <- cbind(1,x1,x2)

beta <- sgd_linear(x, y, n = 10000,alpha=0.005)


##one can compare this with LS fit
beta

pred <- x %*% beta
