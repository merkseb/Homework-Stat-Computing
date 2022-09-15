m <- 100
x <- .5
n <- 100
comp <- 1-exp(-1/2)
unifsample <- function(x,m){
  v <- runif(m/2, min = 0, max = x)
  u <- c(v,x-v)
  theta <- x*mean(exp(-(u)))
  return(theta)
}
cdfsample <- function(x,m){
  u <- runif(m/2)
  theta <- sum((qexp(u)<x)+(qexp(1-u)<x))/m
  return(theta)
}
vu <- numeric(n)
ve <- numeric(n)

for (i in 1:n) {
  vu[i] <- unifsample(x,m)
  ve[i] <- cdfsample(x,m)
}
ratio <- var(vu)/var(ve)

c(abs(unifsample(x,m)-comp),abs(cdfsample(x,m)-comp))/abs(comp)