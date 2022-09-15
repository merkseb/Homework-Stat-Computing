data <- chickwts
a <- data$weight[data$feed=='horsebean']
b <- data$weight[data$feed=='linseed']
c <- data$weight[data$feed=='soybean']
d <- data$weight[data$feed=='sunflower']
e <- data$weight[data$feed=='meatmeal']
f <- data$weight[data$feed=='casein']

feeds <- data$feed

dat <- c(a,b,c,d,e,f)

la <- length(a)
lb <- length(b)
lc <- length(c)
ld <- length(d)
le <- length(e)
lf <- length(f)

len <- c(la,lb,lc,ld,le,lf)

n <- sum(len)

#reps <- factorial(n)/(prod(factorial(len)))

fit <- aov(weight~feed,data)
obs <- summary.lm(fit)$fstatistic[1]

R <- 3000

test <- numeric(R)

for (i in 1:R) {
  s <- sample(1:n,size=n,replace=FALSE)
  sampledf <- data.frame(weight = dat[s], feed = feeds)
  fitting <- aov(weight~feed,sampledf)
  test[i] <- summary.lm(fitting)$fstatistic[1]
}
hist(test,breaks = 30, xlim=c(0,18))
points(obs,0,pch=16,col=2)

p <- mean(abs(test)>abs(obs))
print("p-value=:");print(p)

print("H_0 rejected?");print(p<.05)