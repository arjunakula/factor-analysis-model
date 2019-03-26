fun <- function(x, A, b)
{
  x = matrix(x)
  result = 0.5 * t(x) %*% A %*% x + t(x) %*% b
  return(as.numeric(result))
}

A = matrix(c(2,1,1,1),nrow=2)
b = matrix(c(1,1))
x=matrix(c(0,0))
H = diag(2)
v = matrix(c(-1, -1))
optim(par=x, fn = fun, A = A, b = b, method = "BFGS")