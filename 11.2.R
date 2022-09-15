set.seed(42)
n <- 10000
burn <- n*.2
X <- numeric(n)
X[1] <- rgamma(1,1)
k <- 0
f <- function(x,sig = 4){
  r <- numeric(length(x))
  for (i in 1:length(x)) {
    if(x[i]<0){r[i] <- 0}
  else{r[i] <- x[i]/sig^2*exp(-x[i]^2/(2*sig^2))}
  }
  return(r)
}
for (i in 2:n) {
  u <- runif(1)
  y <- rgamma(1,X[i-1],1)
  if(u<=(f(y)*dgamma(X[i-1],y,1))/(f(X[i-1])*dgamma(y,X[i-1],1))){
  X[i] <-  y
  k <- k + 1
  }
  else{
    X[i] <- X[i-1]
  }
}
seq <- seq(0,15,.1)
#plot(burn+1:n,X[burn+1:n],type = "l",xlim = c(burn+1,n))
plot(density(X[burn:n]))
lines(seq,f(seq),cex=.25)
print(k/n)