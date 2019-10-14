library(manipulate)
library(UsingR)
data(galton)
library(reshape)
galton
long <- melt(galton)
long


g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1) 
g <- g + facet_grid(. ~ variable)
g

myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "blue", colour = "white", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))




# load necessary packages/install if needed
library(ggplot2); library(UsingR); data(galton)
# function to plot the histograms
myHist <- function(mu){
  # calculate the mean squares
  mse <- mean((galton$child - mu)^2)
  # plot histogram
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon",
                                                       colour = "black", binwidth=1)
  # add vertical line marking the center value mu
  g <- g + geom_vline(xintercept = mu, size = 2)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
# manipulate allows the user to change the variable mu to see how the mean squares changes
#   library(manipulate); manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))]
# plot the correct graph
myHist(mean(galton$child))




library(dplyr)
# constructs table for different combination of parent-child height
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child (in)", "parent (in)", "freq")
# convert to numeric values
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
# filter to only meaningful combinations
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g + scale_size(range = c(2, 20), guide = "none" )
# plot grey circles slightly larger than data as base (achieve an outline effect)
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size = freq))
# change the color gradient from default to lightblue -> $white
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g




