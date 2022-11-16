#' Implement Trapezoid Rule for Generic Function
#'
#' @param n The number of intervals used for approximation
#' @param f A function with a single argument x
#' @param range A vector containing the lower and upper endpoints of the range of integration
#'
#' @return Returns a single value which is the trapezoidal approximation of the function over (a,b)
#'
#' @examples
#' trapezoid(5,sqrt, c(1,3))
#'
#' @export
trapezoid <- function(n,f,range){
  # input
  a = range[1]
  b = range[2]

  x1 = seq(a,b, length = n+1)[-(n+1)]
  x2 = seq(a,b,length = n+1)[-1]
  f1 = f(x1)
  f2 = f(x2)
  tSum = sum(((f1+f2)/2)*((b-a)/n))
  return(tSum)
}
