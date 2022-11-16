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
