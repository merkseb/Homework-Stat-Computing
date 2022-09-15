n <- 1500
burn <- 501
k <- 10
psd <- 3
tmean <- 0
tsd <- 1
X <- matrix(0,n,k)
X[1,] <- seq(-10,10,length.out=10)
#metropolis-hastings
for (i in 1:k) {
  u <- runif(n)
  for (j in 2:n) {
    xt <- X[j-1,i]
    y <- rnorm(1,xt,psd)
    acc <- dnorm(y,tmean,tsd)*dnorm(xt,y,psd)/(dnorm(xt,tmean,tsd)*dnorm(y,xt,psd))
    if(u[j]<=acc){X[j,i]=y}
    else{X[j,i]=xt}
  }
}
x <- X[burn:n,]
N <- n-burn+1
#gelman-rubin stat
gel <- numeric(N)
Vhat <- numeric(N)
for (t in 1:N) {
psi <- matrix(0,k,t)
psibar <- numeric(k)
sdpsibar <- numeric(k)
for (i in 1:k) {
  for (j in 1:t) {
    psi[i,j] <- mean(x[1:j,i])
  }
  psibar[i] <- mean(psi[i,])
}

evpsibar <- mean(psibar)
for (i in 1:k) {sdpsibar[i] <- 1/t*sum((psi[i,]-psibar[i])^2)}
W <- mean(sdpsibar)
B <- t/(k-1)*sum((psibar[i]-evpsibar)^2)
Vhat[t] <- (t-1)/t*W+1/t*B
gel[t] <- Vhat[t]/W
}