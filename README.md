# deconstructR

An R package for deconstructing scatter plots. Currently, two animated graphs are provided: `blackhole` and `gravity`. Here is a simple example: 

```{r, eval=FALSE}
set.seed(234)
N <- 20
x <- rnorm(N)
y <- rnorm(N)+0.3*x

xlab <- "Chronic Functional Limitations"
ylab <- "Depressive Affect"


gravity(x,y, xlab=xlab, ylab=ylab)
```

![gravity](https://github.com/brandmaier/deconstructR/blob/master/inst/gravity.gif?raw=true]
