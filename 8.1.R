times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
B <- 200
l <- numeric(200)
comp <- 1/mean(times)
for (i in 1:200) {
  x <- sample(times,length(times),replace = TRUE)
  l[i] <- 1/mean(x)
}
hist(l)
abline(v=comp,col="red")
print(mean(l-comp))
print(sqrt(1/(B-1)*sum((comp-l)^2)))