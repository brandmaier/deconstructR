
#' Gravity plot function
#'
#' @param x
#' @param y
#' @param xlim
#'
gravity <- function(x,y, xlim=NULL, ylim=NULL,xlab=NULL, ylab=NULL, movie.name=NULL) 
{
  deconstructr.plot(x,y,xlim, ylim, xlab, ylab,movie.name=movie.name, type="gravity")
}


aniloop.gravity <- function(x,y,xlim,ylim, xlab,ylab, frames=60, stillframe=20) {
  
  totalframes <- frames+2*stillframe
  
  speedy <- rep(0, N)
  
  bouncyness <- 0.5 #0.3
  
  for (i in 1:totalframes) {
    
    
    plot(x,y,xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab)
    
    model <- lm(y~x)
    abline(model, lty=1)
    p <- round(cor.test(x,y)$p.value,2)
    #   legend("topright", legend = paste("p=",p,sep=""),border = FALSE,bty = "n")
    
    floory <- ylim[1]
    height <- ylim[2]-ylim[1]
    
    if (i <= stillframe) {
      next
    }
    
    if (i > stillframe+frames) {
      next
    }
    
    y <- y + speedy
    
    onfloor <- (y <= (floory+0.01*height))
    
    # increase speed (accelerate)
    speedy[!onfloor] <- speedy[!onfloor] - runif(N,0,height*0.02)
    
    # bounce
    speedy[onfloor] <- -speedy[onfloor]*bouncyness
    
    #speedy[onfloor] <- ifelse(abs(speedy[onfloor]<0.2,0,speedy[onfloor]))
    y[onfloor] <- floory
    
    #if (i > stillframe+10) {
    #  freeze <- freeze | (abs(speedy) < 0.5)
    #}
    
    
    
  }
  
  
}

