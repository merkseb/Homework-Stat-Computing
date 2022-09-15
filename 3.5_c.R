n <- 1000

w <- rinvgamma(n,2,2)
x <- rnorm(n,0,sqrt(w))

df <- data.frame(x = quantile(x,ppoints(500)), 
                 y = qt(ppoints(500),4))

ggplot(df, aes(x,y)) + geom_point() + 
  geom_abline(slope = 1, color = "red")