set.seed(1000)
x <- rnorm(50, 10, 12)
y <- (x*.5)+rnorm(50, 5, 2)
df1 <- data.frame(x, y)
fit <- lm(y ~ x, data=df1)
df1$predicted <- predict(fit)   # Save the predicted values
df1$residuals <- residuals(fit)
plot(x,y, pch=16, xlab="", ylab="", main="Plotting Residuals")
abline(lm(y~x, data=df1), lty=3, col="peru")
segments(df1$x,df1$predicted,  df1$x,df1$y, col="steelblue")


par(mfrow=c(1,2))
plot(x,y, pch=16, xlab="", ylab="", main="", col="peru")
abline(lm(y~x, data=df1), lty=3, col="steelblue")
set.seed(100)
x2 <- rnorm(60, 100, 40)
y2 <- (x2^2)+rnorm(60, 100, 500)
plot(x2,y2, pch=16, xlab="", ylab="", col="peru")
abline(lm(y2~x2), col="steelblue", lty=3)
summary(lm(y2~x2))

par(mfrow=c(1,2))
fit1 <- lm(y~x)
fit2 <- lm(y2~x2)
res1 <- residuals(fit1)
res2 <- residuals(fit2)
plot(predict(fit1), res1, xlab="Fitted Values", ylab="Residuals", main="Residuals vs. Fitted")
abline(0,0, col="red")
plot(fitted(fit2), res2, xlab="Fitted Values", ylab="Residuals", main="Residuals vs. Fitted")
abline(0,0, col="red")

library(car)
ncvTest(fit1)
plot(fit1,3)

hist(res1, breaks=6, col="peru", main="Histogram of Residuals", xlab="Residuals")

plot(fit1, 2, col="peru", pch=16)

read.csv("https://raw.githubusercontent.com/ejvanholm/DataProjects/master/PSID1982.csv")

summary(lm(wage~ethnicity+occupation+experience+married, data=PSID1982))

summary( fit <- lm(wage~experience+weeks+occupation+industry+south+smsa+married+gender+union+education+ethnicity , data=PSID1982))
vif(fit)

library(AER)
data(CASchools)
summary(fit2 <- lm(math~students+teachers+calworks+lunch+computer+expenditure+income+english, data=CASchools))
vif(fit2)

par(mfrow=c(1,2))
set.seed(1000)
x <- rnorm(50, 10, 12)
y <- (x*.5)+rnorm(50, 5, 2)
df1 <- data.frame(x, y)
plot(x,y, pch=16, xlab="", ylab="", main="No Outlier", ylim=c(0,200))
abline(lm(y~x, data=df1), lty=3, col="peru")
df3 <- df1
df3[51,] <- c(40,171)
plot(df3$x,df3$y, pch=16, xlab="", ylab="", main="With Outlier", ylim=c(0,200))
abline(lm(y~x, data=df3), lty=3, col="peru")

summary(lm(y~x, data=df1) )
summary(lm(y~x, data=df3) )

fit3 <- lm(y~x, data=df3) 

plot(fit3, 5)