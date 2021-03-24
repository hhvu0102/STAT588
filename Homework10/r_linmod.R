library(mlbench)

data <- read.table("D:/Coursework/Stat 588 Fall 2020/Homework10/midterms.csv", sep=",", header=T)

# Fitting the model

fit<-lm(house.change~unemployment, data = data)
fit

y <- data$house.change
x <- data$unemployment
n <- length(y)

beta.hat <- (n*sum(y*x)-sum(x)*sum(y))/(n*sum(x*x)-sum(x)**2)
beta.hat
alpha.hat <- mean(y)-mean(x)*beta.hat
alpha.hat

# Testing the presence of relationship, H0: beta = 0

sigma.hat.squared <- sum((y - (alpha.hat + x*beta.hat))^2)/n
sigma.hat.squared
Sxx <- sum((x - mean(x))^2)
t <- (beta.hat/sqrt(sigma.hat.squared)) * sqrt(Sxx * (n-2)/n)
t
 
pvalue <- 2*(pt(t,n-2))
pvalue

summary(fit)

# plots

fitted.y <- alpha.hat + beta.hat*x

xy<-cbind(x, fitted.y)
xy<-xy[order(xy[,1]),]

plot(x, y, xlab = "unemployment", ylab = "house.change")
lines(xy[,1], xy[,2], col = 'red', lwd = 2)