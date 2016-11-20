
aniloop.gravity <- function(x,y,xlim,ylim, xlab,ylab, frames=60, stillframe=20) {
  
  totalframes <- frames+2*stillframe
  
  for (i in 1:totalframes) {
    
    
    plot(x,y,xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab)
    
    model <- lm(y~x,lty=1)
    abline(model)
    p <- round(cor.test(x,y)$p.value,2)
    #   legend("topright", legend = paste("p=",p,sep=""),border = FALSE,bty = "n")
    
    
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
    speedy[onfloor] <- -speedy[onfloor]*0.3
    
    #speedy[onfloor] <- ifelse(abs(speedy[onfloor]<0.2,0,speedy[onfloor]))
    y[onfloor] <- floory
    
    #if (i > stillframe+10) {
    #  freeze <- freeze | (abs(speedy) < 0.5)
    #}
    
    
    
  }
  
  
}
}
