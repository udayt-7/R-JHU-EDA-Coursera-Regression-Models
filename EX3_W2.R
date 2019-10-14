
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(w * x) / sum(w)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x -1)

data("mtcars")

lm(mtcars$mpg~mtcars$wt)

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x))/sd(x)
9.31-8.58


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

x = c(530, 940)
y = c(93748300, 181165900)

model <- lm(x~y)
summary(model)


t30 <- 90.31 + (120000000*0.00000469)
t30


library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x =carat, y =price) )
g = g + xlab("Mass")
g = g + ylab('Price')
g = g + geom_point(size = 6, colour = "black", alpha = 0.3)
g = g + geom_point(size = 5,  colour = "blue", alpha = 0.3)
g = g + geom_smooth(method = 'lm', colour = 'black')

g

model <- lm(price ~ carat, data = diamond)
coef(model)
summary(model)












x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

model <- lm(y~x)
summary(model)




?data('mtcars')

?mtcars


fit <- lm(mtcars$mpg~mtcars$wt)
coef <- summary(fit)$coefficients
coef

predict(fit, newdata = data.frame(x = 3), interval = ("prediction"))


fit_car2 <- lm(mtcars$mpg~mtcars$wt)
sumCoef2 <- coef(summary(fit_car2))
(sumCoef2[2,1] + c(-1, 1) * qt(.975, df = fit_car2$df) * sumCoef2[2, 2])


fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared
