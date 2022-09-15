
n <- 1000
k <- 0
c <- dnorm(1)/dt(1,2)
y <- numeric(n)
t <- 0
while(k<n){
  x <- rt(1,2)
  u <- runif(1)
  t <- t+1
  if(u <= dnorm(x)/(dt(x,2)*c)){
    k <- k+1
    y[k] <- x
  }
}
s <- seq(-5,5,.01)
plot(density(y),ylim = c(0,0.5))
lines(s,dnorm(s))
#d 
#e
s2 <- c(.025,.975)
qs <- quantile(y,s2)
qt <- qnorm(s2)
sd <- sqrt(s2 * (1-s2))/(n * dnorm(qt)^2)

round(rbind(qt,qs,sd),3);print(n/t)