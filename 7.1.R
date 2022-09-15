m <- 100
n <- 10
alpha <- .05
t1 <- qchisq(1-alpha/2,n-1)
t2 <- qchisq(alpha/2,n-1)
y <- numeric(m)
for (i in 1:m) {
  x <- rnorm(n)
  v <- var(x)
  y[i] <- ((n-1)*v/t1<1)*((n-1)*v/t2>1)
}
alpha.h <- mean(y)
se <- sqrt(alpha.h*(1-alpha.h)/m)
abs(alpha-alpha.h)/se