
#ANOVA
A <- as.factor(rep(c(-1,1,-1,1),6))
B <- as.factor(rep(c(-1,-1,1,1),6))
C <- as.factor(rep(c(-1,-1,-1,-1,1,1,1,1),3))
times <- c(360,288,336,264,376,280,376,272,
           360,280,344,280,384,272,384,280,
           368,288,352,272,384,280,392,288)

model <- lm(times~A+B+C+A*B+A*C+B*C+A*B*C)
anova(model)

model <- lm(times~A+B+C+A*C+B*C)
anova(model)

#main effects box plots
par(mfrow=c(1,1))
boxplot(times~A+B+C+A*C+B*C)

#interaction plots
par(mfrow=c(1,2))
interaction.plot(A,C, times)
interaction.plot(C,A, times)
interaction.plot(C,B, times)
interaction.plot(B,C, times)



#linear regression
A<-rep(c(-1,1,-1,1),6)
B<-rep(c(-1,-1,1,1),6)
C<-rep(c(-1,-1,-1,-1,1,1,1,1),3)

model.reg <- lm(times~A+B+C+A*C+B*C)
summary(model.reg)

newdata <- data.frame(A=1,B=1, C=-1)
predict(model.reg, newdata, interval="confidence")

#95% CI for mean response evaluated at A+,B+,C-
UB <- 270.66+qt(.975,16)*sqrt(var(c(264,280,272))/3)
LB <- 270.66-qt(.975,16)*sqrt(var(c(264,280,272))/3)
CI <- c(LB, UB)
CI

#residual analysis
par(mfrow=c(1,2))
library(MASS)
e.star = studres(model)
y.hat=predict(model)
plot(e.star~y.hat, ylim=c(-3,3), ylab="Studentized Residuals", 
     xlab="Treatment Mean", main="Plot of Studentized Residuals vs. Treatment Means")
abline(h=2, col="blue", lty=2)
abline(h=-2, col="blue", lty=2)
abline(h=0)

qqnorm(model$residuals, main = "Normal Probability Plot of Residuals"); qqline(model$residuals);

library(nortest)
ad.test(model$residuals)
