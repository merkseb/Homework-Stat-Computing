m <- 100
x <- .5
comp <- 1-exp(-1/2)
unifsample <- function(x,m){
  u <- runif(m, min = 0, max = x)
  theta <- x*mean(exp(-(u))) 
  return(theta)
}
cdfsample <- function(x,m){
  e <- rexp(m)
  theta <- mean(e<x)
  return(theta)
}

vu <- numeric(n)
ve <- numeric(n)

for (i in 1:n) {
  vu[i] <- unifsample(x,m)
  ve[i] <- cdfsample(x,m)
}
ratio <- var(vu)/var(ve)

c(abs(unifsample(x,m)-comp),abs(cdfsample(x,m)-comp))