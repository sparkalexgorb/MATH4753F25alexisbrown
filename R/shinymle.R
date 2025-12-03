#' Maximum Likelihood Estimation for Different Distributions
#'
#' @returns an app that creates MLE graphs for the selected distribution and sample size n
#' @export
#'
#' @examples
#' shinymle
shinymle <- function(){
  shiny::runApp(system.file("SHINY", package = "MATH4753F25alexisbrown"),
                launch.browser = TRUE)
}
