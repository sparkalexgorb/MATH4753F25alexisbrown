#' Birthday Problem
#'
#' @param n the number of people in the group
#'
#' @returns a probability scalar that 2 or more people sharing a birthday in a group
#' @export
#'
#' @examples
#' birthday(25)
birthday <- function(n){
  1 - exp(lchoose(365,n)+ lfactorial(n) - n*log(365))
}
