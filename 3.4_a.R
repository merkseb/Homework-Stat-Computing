n <- 1000

v1 <- rchisq(n,1)/rchisq(n,3)
v2 <- rchisq(n,10)/rchisq(n,10)
v3 <- rchisq(n,100)/rchisq(n,100)

#hist(v1,breaks = 100)

qqplot(rf(1000,100,100),v3,cex=.25)
abline(0,1)
c(abs(mean(v1)-3/1),abs(mean(v2)-10/8),abs(mean(v3)-100/98))