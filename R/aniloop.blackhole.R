blackhole <- function(x,y, xlim=NULL, ylim=NULL,xlab=NULL, ylab=NULL, movie.name=NULL) 
{
  deconstructr.plot(x,y,xlim, ylim, xlab, ylab, movie.name=movie.name, type="blackhole")
}

getAngle <- function(x1,y1,x2,y2) {
  #return( atan2(y1,x1)-atan2(y2,x2))
  return( atan2(y2-y1,x2-x1))
}

aniloop.blackhole <- function(x,y,xlim,ylim,xlab,ylab, frames=60, stillframe=20) {
  
  totalframes <- frames+2*stillframe
  
  
  angle <- rep(NA, length(x))
  
  center.x <- (max(x)+min(x))/2
  center.y <- (max(y)+min(y))/2
  
  oangle <- getAngle(center.x, center.y, x, y)

  speed <- rep(.01 * (max(x)-min(x)) ,length(x))
  
  for (i in 1:totalframes) {
    
    
    
    plot(x,y,xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab)
  
    
    if (i <= stillframe) {
      next
    }
    
    if (i > stillframe+frames) {
      next
    }
    angle <- getAngle(center.x, center.y, x, y)
    
    x <- x - cos(angle)*speed
    y <- y - sin(angle)*speed
    
    stopped <- (sign(angle)!=sign(oangle))
    
    if (any(stopped)) {
      speed[stopped] <- 0
      x[stopped] <- center.x
      y[stopped] <- center.y
    }
    
    speed <- speed * 1.1
  
  }
  
}