#' Probability under a normal curve
#'
#' @param mu mean of distribution
#' @param sigma standard deviation of distribution
#' @param a target X value for probability
#'
#' @returns a plot of a normal curve with the probability of X less than or equal to a shaded
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma, a)
  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma,a,length=1000)
  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  # Fill in the polygon with the given vertices
  polygon(c(mu-4*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  # Area
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(mu-4*sigma,mean=mu,sd=sigma)
  prob=round(prob,4)
  # Put in the text with the appropriate area
  text(x=3, y=0.01, paste("Area = ", prob, sep=""))
}
