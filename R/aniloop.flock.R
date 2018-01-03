
#' Flock plot function based on Craig Reynolds (1986) flocking algorithm
#'
#' @param x
#' @param y
#' @param xlim
#'
flock <- function(x,y, xlim=NULL, ylim=NULL,xlab=NULL, ylab=NULL, movie.name=NULL) 
{
  deconstructr.plot(x,y,xlim, ylim, xlab, ylab,movie.name=movie.name, type="flock")
}


aniloop.flock <- function(x,y,xlim,ylim, xlab,ylab, frames=80, stillframe=20) {
  
  num.elems <- length(x)
  
  totalframes <- frames+2*stillframe
  
  vel.x <- rnorm(n = num.elems)
  vel.y <- rnorm(n = num.elems)
  
  for (i in 1:totalframes) {
    
    plot(x,y,xlim=xlim,ylim=ylim, xlab=xlab, ylab=ylab)
    
    # distance matrix
    D <-as.matrix( dist(cbind(x,y)))
    
    # init
    rule1.vel.x <- rep(0, num.elems)
    rule1.vel.y <- rep(0, num.elems)
    rule2.vel.x <- rep(0, num.elems)
    rule2.vel.y <- rep(0, num.elems)    
    
    # (1) align
  
    for (j in 1:num.elems) {
      neighbors <- which(D[, j]<.2) # neighbors incl. self
      rule1.vel.x[j] <- sum(vel.x[neighbors])
      rule1.vel.y[j] <- sum(vel.y[neighbors])
      # normalize
      vnorm <- sqrt(rule1.vel.x[j]*rule1.vel.x[j]+rule1.vel.y[j]*rule1.vel.y[j])
      rule1.vel.x[j] <- rule1.vel.x[j] / vnorm
      rule1.vel.y[j] <- rule1.vel.y[j] / vnorm
    }
    
    # (2) separation
    for (j in 1:num.elems) {
      neighbors <- which(D[, j]<.1) # neighbors incl. self
      for (k in neighbors) {
       # if (k==j) next;
        rule2.vel.x[j] <- rule2.vel.x[j]+ x[k]-x[j]
        rule2.vel.y[j] <- rule2.vel.y[j]+ y[k]-y[j]
      } 
    }
    
    # invert
    rule2.vel.x <- -rule2.vel.x
    rule2.vel.y <- -rule2.vel.y
    
    #print(rule2.vel.x)
    #print(rule2.vel.y)

    
    # normalize
    vnorm <- sqrt(rule2.vel.x*rule2.vel.x+rule2.vel.y*rule2.vel.y)
    rule2.vel.x <- rule2.vel.x / vnorm
    rule2.vel.y <- rule2.vel.y / vnorm        
    
    nas <- is.na(rule2.vel.y) | is.na(rule2.vel.x)
    if (sum(nas) >0) {
      rule2.vel.x[nas] <- 0
      rule2.vel.y[nas] <- 0
    }
    
    # (3) cohesion
    center.x <- mean(x)
    center.y <- mean(y)
    rule3.vel.x <- -x+center.x
    rule3.vel.y <- -y+center.y
    # normalize
    vnorm <- sqrt(rule3.vel.x*rule3.vel.x+rule3.vel.y*rule3.vel.y)
    rule3.vel.x <- rule3.vel.x / vnorm
    rule3.vel.y <- rule3.vel.y / vnorm    
    
    w1 <- 1 # alignment  (2)
    w2 <- 1.5 # separation (.5)
    w3 <- 1 # cohesion (1)
    
    # add together velocities
    vel.x <- vel.x + w1*rule1.vel.x + w2*rule2.vel.x + w3*rule3.vel.x
    vel.y <- vel.y + w1*rule1.vel.y + w2*rule2.vel.y + w3*rule3.vel.y

    # normalize
    vnorm <- sqrt(vel.x*vel.x+vel.y*vel.y)    
    vel.x <- vel.x / vnorm
    vel.y <- vel.y / vnorm
    
    # move 'em
    scale <- 0.05
    x <- x + vel.x*scale
    y <- y + vel.y*scale
    
  }

}