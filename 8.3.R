h <- c(173, 183, 187, 179, 180, 186, 179, 196, 202, 198, 197, 185, 194, 185, 191, 182, 182, 187, 184, 186)

thetahat <- mean(h)

B <- 200
y <- numeric(B)
x <- numeric(B)
ts <- numeric(B)
for (i in 1:B) {
  r <- sample(h,length(h),replace = TRUE)
  y[i] <- mean(r)
  for (j in 1:B) {
      rs <- sample(r,length(h),replace = TRUE)
      x[j] <- mean(rs)
  }
  ts[i] <- (y[i]-thetahat)/sd(x)
}

ci1 <- c(thetahat-qnorm(0.975)*sd(y),thetahat+qnorm(0.975)*sd(y))
ci2 <- c(2*thetahat-quantile(y,.975),2*thetahat-quantile(y,.025))
ci3 <- c(quantile(y,.025),quantile(y,.975))
ci4 <- c(thetahat-quantile(ts,.975)*sd(y),thetahat-quantile(ts,.025)*sd(y))
print(ci1)
print(ci2)
print(ci3)
print(ci4)