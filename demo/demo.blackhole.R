library(deconstructR)

set.seed(234)
N <- 20
x <- rnorm(N)
y <- rnorm(N)+0.3*x

xlab <- "Perceptual Speed"
ylab <- "Alpha Peak Frequency"


blackhole(x,y, xlab=xlab, ylab=ylab)


