library(deconstructR)

set.seed(234)
N <- 20
x <- rnorm(N)
y <- rnorm(N)+0.3*x

xlab <- "Chronic Functional Limitations"
ylab <- "Depressive Affect"


gravity(x,y, xlab=xlab, ylab=ylab)


