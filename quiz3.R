suppressMessages(library(UsingR))
suppressMessages(library(ggplot2))
suppressMessages(library(stats))
suppressMessages(library(lmtest))
suppressMessages(library(dplyr))
data("mtcars")

mcyl<-relevel(factor(mtcars$cyl),"4")
fit<-lm(mpg~mcyl+wt, data = mtcars)
summary(fit)$coef[3]

mcyl<-relevel(factor(mtcars$cyl),"4")
fit_adjusted<-lm(mpg~mcyl+wt, data = mtcars)
summary(fit_adjusted)$coefficients

fit_unadjusted<-lm(mpg~mcyl, data = mtcars)
summary(fit_unadjusted)$coefficients

lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

max(influence(lm(y~x))$hat)

influence.measures(lm(y~x))
