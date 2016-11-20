aniloop.blackhole <- function(x,y,xlim,ylim,xlab,ylab) {
  
  angle <- rep(NA, length(x))
  
  center.x <- (max(x)+min(x))/2
  center.y <- (max(y)+min(y))/2
  
  
  
  for (i in 1:totalframes) {
    
    
    plot(x,y,xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab)
   
     
    x <- x + sin(angle)
    y <- y + cos(angle)
    
  }
  
}