m <- 100
#pi <- c(.1,.4)
pi <- c(seq(.01,1,0.001))
power <- numeric(2)
for (i in 1:991) {
  y <- numeric(m)
  for (k in 1:m) {
    x <- rbinom(1,15,pi[i])
    y[k] <- x<=3
  }
  power[i] <- mean(y)
}
#se <- sqrt(power*(1-power)/m)
df <- data.frame(x=pi,y=power)
ggplot(df,aes(x,y))+geom_smooth(method = 'gam')