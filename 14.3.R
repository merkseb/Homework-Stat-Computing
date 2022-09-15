n <- 100
k <- 10

logl <- function(p){
  -log(p)*k-log(1-p)*(n-k)
}
ps <- seq(0,1,length.out=1000)
plot(ps,logl(ps),type="l")
abline(v=.1)
mle(logl,.2,lower = c(.01),upper = c(.99))