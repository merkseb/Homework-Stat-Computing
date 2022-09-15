library(tidyverse)
library(reshape2)
library(DAAG); attach(ironslag)
crossval <- function(formula,log){
e <- numeric(26)
for (i in 1:26) {
  if(i <= 25){
  r <- c(2*i,2*i-1)
  df <- data.frame(y = magnetic[-r],x = chemical[-r])
  mod <- lm(formula,data = df)
  newdf <- data.frame(x = chemical[r])
  if(log==TRUE){e[i] <- 1/2*sum((magnetic[r]-exp(predict(mod,newdata = newdf)))^2)}
  else{e[i] <- 1/2*sum((magnetic[r]-predict(mod,newdata = newdf))^2)}
  }
  
  else{
  r <- c(2*i,2*i-1,2*i+1)
  df <- data.frame(y = magnetic[-r],x = chemical[-r])
  mod <- lm(formula,data = df)
  newdf <- data.frame(x = chemical[r])
  if(log==TRUE){e[i] <- 1/3*sum((magnetic[r]-exp(predict(mod,newdata = newdf)))^2)}
  else{e[i] <- 1/3*sum((magnetic[r]-predict(mod,newdata = newdf))^2)}
  }
}
 return(e)
}

df <- data.frame(elin = crossval(y~x,FALSE),
equad = crossval(y~x+I(x^2),FALSE),
elog = crossval(log(y)~x,TRUE),
eloglog = crossval(log(y)~log(x),TRUE))
ggplot(data = melt(df)) + 
  geom_boxplot(aes(x=variable,y=value,middle=sd(value)))
print(rbind(apply(as.matrix(df),2,mean),apply(as.matrix(df),2,sd)))
