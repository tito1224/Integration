#' Implement Simpson's Rule for Generic Function
#'
#' @param n The number of intervals used for approximation
#' @param f A function with a single argument x
#' @param range A vector containing the lower and upper endpoints of the range of integration
#'
#' @return Returns a single value which is the simpson approximation of the function over (a,b)
#'
#' @examples
#' simpsons(5,sqrt, c(1,3))
#'
#' @export
simpsons <- function(n,f,range){
  # input
  a = range[1]
  b = range[2]
  i = 1:n

  x1 = seq(a,b,length =n+1)[-(n+1)]
  x2 = x1+(b-a)/(2*n)
  x3 = x2 + (b-a)/(2*n)

  f1 = f(x1)
  f2 = f(x2)
  f3 = f(x3)

  sSum = sum(((b-a)/(6*n)) * (f1 + 4*f2 + f3))
  return(sSum)
}
