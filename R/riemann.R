#' Implement Riemann Integration for a Generic Function
#'
#' @param f A function with a single argument x
#' @param range A vector containing the lower and upper endpoints of the range of integration
#' @param n The number of intervals used for Riemann approximation
#'
#' @return Returns a single value which is the Riemann approximation of the function over (a,b)
#'
#' @examples
#' riemann(5,sqrt, c(1,3))

#' @export
riemann <- function(n,f,range){
  # f = function with a single argument x
  # range = a vector specifying the lower and upper end points (a,b)
  # n = number of intervals for riemann approximation

  # determine x_i (use right end-point)
  i = 1:n
  a = range[1]
  b = range[2]

  x_i = seq(a,b,length = n+1)[-(n+1)]

  # final answer
  rSum = sum(f(x_i)*((b-a)/n))
  return(rSum)

}
