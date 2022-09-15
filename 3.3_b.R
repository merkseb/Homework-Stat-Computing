#3.3
#b)

r <- seq(-5,5,.01)
fs <- 1/sqrt(2*pi)*exp(-r^2/2)
ts <- gamma(3/2)/sqrt(2*pi)*(1+r^2/2)^(-3/2)
plot(r,fs/ts,type = "l")
abline(h = dnorm(1)/dt(1,2))
