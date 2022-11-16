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
