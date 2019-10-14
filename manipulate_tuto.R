library(manipulate)

?manipulate

manipulate(plot(1:x), x = slider(5, 10))

manipulate(plot(cars, xlim=c(x.min,x.max)), x.min=slider(0,15),  x.max=slider(15,30))

manipulate(barplot(as.matrix(longley[,factor]), beside = TRUE, main = factor), factor = picker("GNP", "Unemployed", "Employed"))
