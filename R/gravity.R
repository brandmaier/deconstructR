# deconstruct

library(animation)
#ani.options(interval=.05)



gravity <- function(x,y, xlim=NULL, ylim=NULL,xlab=NULL, ylab=NULL) {
  


speedy <- rep(0, N)

if (is.null(xlim))
xlim <- c(min(x),max(x))

if (is.null(ylim))
ylim <- c(min(y),max(y))

height <- ylim[2]-ylim[1]
floory <- ylim[1]
freeze <- rep(FALSE, N)



saveGIF( aniloop(x,y,xlim,ylim,xlab,ylab), 
         interval=0.05, movie.name="scatterdeconstruct.gif",
         ani.width=600)

}