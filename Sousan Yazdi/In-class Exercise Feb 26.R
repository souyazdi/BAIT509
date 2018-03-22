
library(tibble)
library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

#1. Generate data - as much as you'd like.
x<-genreg(10)

#2. For now, ignore the Y values. Use the means from the distributions listed above
#to predict Y under four circumstances:
#2.1. Using both the values of X1 and X2.
dat<-mutate(x, yhat=0, yhat1=5-x1, yhat2=5+2*x2, yhat3=5-x1+2*x2)

#2.2. Using only the values of X1.


#2.3. Using only the values of X2.


#2.4. Using neither the values of X1 nor X2. (Your predictions in this case will be the same every time - what is that number?)

#MSE

mse <- mean((dat$yhat-x$y)^2)
mse1 <- mean((dat$yhat1-x$y)^2)
mse2 <- mean((dat$yhat2-x$y)^2)
mse3 <- mean((dat$yhat3-x$y)^2)

#more predictor we get more  - x2 more dependent to y than x1 (x2 smaller error)

############################################################

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}

gencla(10)

p.a.1 <- 0.2
p.b.1 <- 0.8/(1+exp(-(1)))
p.c.1 <- 1-p.a.1-p.b.1

p.a.2 <- 0.2
p.b.2 <- 0.8/(1+exp(-(-2)))
p.c.2 <- 1-p.a.2-p.b.2

#generate data as much as you want
dat2<-gencla(1000)

dat2<-mutate(dat2,yhat=sapply(x,function(x_) if(x_<0) "C" else "B")

1- mean(dat2$yhat ==)
