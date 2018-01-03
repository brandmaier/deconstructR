

deconstructr.plot <- function(x,y, xlim=NULL, ylim=NULL,xlab=NULL, 
                              ylab=NULL, movie.name=NULL, type="gravity") {
  

  if (is.null(movie.name)) {
   movie.name <- "scatterdeconstruct.gif"
  }


if (is.null(xlim))
xlim <- c(min(x),max(x))

if (is.null(ylim))
ylim <- c(min(y),max(y))

height <- ylim[2]-ylim[1]
floory <- ylim[1]
freeze <- rep(FALSE, N)


if (type == "blackhole") {
  fun <- aniloop.blackhole
} else if (type =="gravity") {
  fun <- aniloop.gravity
} else if (type == "flock") {
  fun <- aniloop.flock
} else {
  stop("Unknown type!")
#  return();
}


saveGIF( fun(x,y,xlim,ylim,xlab,ylab), 
         interval=0.05, movie.name=movie.name,
         ani.width=600)

}