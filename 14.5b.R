n <- 600
p <- .65
l1 <- 2
l2 <- 6

l <- c(l1,l2)

I <- sample(1:2,n,replace = T,prob=c(p,1-p))
r <- rpois(n,l[I])

it <- 200

pt <- numeric(it)
l1t <- numeric(it)
l2t <- numeric(it)
pt[1] <- .4
l1t[1] <- 1
l2t[1] <- 7

for (i in 2:it) {
  p <- pt[i-1]
  l1 <- l1t[i-1]
  l2 <- l2t[i-1]
  
  E <- 1/(1+(1-p)/p*(l2/l1)^r*exp(-l2+l1))
  
  l1t[i] <- sum(r*E)/sum(E)
  l2t[i] <- (sum(r)-sum(r*E))/(n-sum(E))
  pt[i] <- mean(E)
}
p <- .65
l1 <- 2
l2 <- 6
#df <- data.frame(x=1:it,p=pt,l1=l1t,l2=l2t)
#ggplot(df,aes(x))+geom_line(aes(y=p))+geom_line(aes(y=l1))+geom_line(aes(y=l2))

bip <- function(k){
  p*l1^k*exp(-l1)/factorial(k) +(1-p)*l2^k*exp(-l2)/factorial(k)
}
bipest <- function(k){
  pt[it]*l1t[it]^k*exp(-l1t[it])/factorial(k) +(1-pt[it])*l2t[it]^k*exp(-l2t[it])/factorial(k)
}

hist(r,freq=FALSE,breaks=c(seq(0,18,.9)))
curve(bip(x),col=2,lwd=2,add=TRUE)
curve(bipest(x),lwd=2,add = TRUE)