#Stat. Computing 

#3.2
#a)
n <- 1000
y <- runif(n)
x <- (y > 1/2) * -log(2*(1-y)) + (y <= 1/2) * log(2*y)
plot(density(x), ylim = c(0,0.5))
s <- seq(-5,5,0.01)
test <- 1/2 * exp(-abs(s))
lines(s,test)
s2 <- seq(0.25,.75,.05)
qs <- quantile(x,s2)
qt <- (s2 > 1/2) * -log(2*abs(1-s2)) + (s2 <= 1/2) * log(2*s2)
sd <- sqrt(s2 * (1-s2))/(n * ((qt>0)*(1-1/2*exp(-qt))+(qt<=1/2)*(1/2*exp(qt)))^2)

round(rbind(qt,qs,sd),3)