m <- c(100,1000,10000)
alpha <- pbinom(3,15,.5)
alpha.h <- numeric(3)
for (i in 1:3) {
  y <- numeric(m[i])
  for (k in 1:m[i]) {
    x <- rbinom(1,15,.5)
    y[k] <- x<=3
  }
  alpha.h[i] <- mean(y)
}
abs(alpha-alpha.h)/alpha