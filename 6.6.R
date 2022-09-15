n <- 1000
f <- function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)
}
g <- function(x){
  exp(-x+1)
}

X <- runif(n)
Y <- -log(1-X)+1
theta <- mean(f(Y)/g(Y))
sd <- sd(f(Y)/g(Y))