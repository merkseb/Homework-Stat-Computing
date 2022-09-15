library(bootstrap)
alpha <- .05 #wanted maximal error of the first kind
B <- 2000 #wanted bootstrap size
n <- nrow(law)
c <- numeric(B)
thetahat <- cor(law$LSAT,law$GPA)
#bootstrap for correlation
for (i in 1:B) {
  s <- sample(1:n,n,replace = TRUE) #generating samples
  c[i] <- cor(law$LSAT[s],law$GPA[s]) #calculating correlation
}

zhat <- qnorm(mean(c<thetahat)) #median bias correction

#jackknife est. for skewness approximate acceleration adjustment
jack <- numeric(n)
for (i in 1:n) {
 jack[i] <- cor(law$LSAT[-i],law$GPA[-i])
}
mjack <- mean(jack)
#calculation of skewness acceleration adjustment
ahat <- sum((mjack-jack)^3)/(6*(sum((mjack-jack)^2))^(3/2))


#quantile calculation
alpha1 <-pnorm(zhat+(zhat+qnorm(alpha/2))/(1-ahat*(zhat+qnorm(alpha/2))))
alpha2 <-pnorm(zhat+(zhat+qnorm(1-alpha/2))/(1-ahat*(zhat+qnorm(1-alpha/2))))


#confidence interval
ci <- c(quantile(c,alpha1),quantile(c,alpha2))

histboot <- hist(c,breaks = 15)
histjack <- hist(jack,breaks = 15)

print(ci)
print(zhat)
print(ahat)