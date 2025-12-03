#' Overbooking problem
#'
#' @param N number of seats on the plane
#' @param gamma probability of having more shows than seats (pain)
#' @param p probability of a person showing up
#'
#' @returns a plot for a discrete function for the problem, a plot for a continuous function, and a list of the number of tickets to sell based on each
#' @export
#'
#' @examples
#' ntickets(N=200,gamma=0.8,p=0.95)
ntickets = function(N,gamma,p){

#pbinom(N+0.5,x,p) - 1 + g =0
  xx <- seq(N-1,N+25,by = 1)
  objd <- pbinom(N, size = xx, p) -1 + gamma #discrete function
  objd1 <- abs(objd) #make values positive to minimize
  plot(x=xx, y=objd1,
       pch =21,
       bg = "blue",
       xlab = "Number of Tickets Sold",
       ylab = "Objective Function",
       main = "Objective Vs n for Discrete Function",
       type = "b")
  index1 <- which.min(objd1) #what is minimum?
  nd <- xx[index1] #number of tickets to sell for discrete
  points(index1,xx[index1], pch = 21, bg = "red", cex = 2)
  text(x=nd,
       y=.4,
       labels= paste("n=", nd))

  sd2= sqrt(N*p*(1-p))
  objc <- 1-gamma-pnorm((N+0.5 - xx*p)/sd2) #continuous function
  objc1 <- abs(objc) #make values positive to minimize
  plot(x=xx, y=objc1,
       pch =21,
       bg = "blue",
       xlab = "Number of Tickets Sold",
       ylab = "Objective Function",
       main = "Objective Vs n for Continuous Function",
       type = "b")
  index2 <- which.min(objc1) #what is minimum?
  nc <- xx[index2] #number of tickets to sell for discrete
  points(index2,xx[index2], pch = 21, bg = "red", cex = 2)
  text(x=nc,
       y=.4,
       labels= paste("n =", nc))

  return(list(nd=nd, nc=nc, N=N, p=p, gamma=gamma))
}
