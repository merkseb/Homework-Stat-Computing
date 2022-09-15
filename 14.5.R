n <- 600
p <- .65
l1 <- 2
l2 <- 6

l <- c(l1,l2)

I <- sample(1:2,n,replace = T,prob=c(p,1-p))
r <- rpois(n,l[I])

bip <- function(k){
  p*l1^k*exp(-l1)/factorial(k) +(1-p)*l2^k*exp(-l2)/factorial(k)
}

hist(r,freq=FALSE,breaks=c(seq(0,18,.9)))
curve(bip(x),add=TRUE)