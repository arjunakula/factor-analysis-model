tls<-function(x,y,kappa=1,lambda=1) {
# X is an n x m matrix with m <= n
# We wish to find the argmon over z and b of
# kappa*|x-z|^2 + lambda*|y-z %*% b|^2
# This can be restated as on page 20 of 202Blecture3.pdf
#	
  n<-nrow(x)
  m<-ncol(x)
  u<-cbind(kappa*x,lambda*y)
# Compute the SVD of X (or at least the first m vectors and singular values)
  s<-svd(u,nu=m,nv=m)
# Form z recognizing the row and column restrictions.
  z<-(s$u)%*%diag(s$d[1:m]) #; c<-s$v
  c1<-t(s$v)[,1:m]; c2<-t(s$v)[,-(1:m)]
  m<-kappa*solve(c1)
  b<-m%*%c2/lambda
  z<-z%*%c1/kappa
# Compute the sums-of-squares
  ssq1<-sum((x-z)^2); ssq2<-sum((y-z%*%b)^2)
  ssqt<-kappa*ssq1+lambda*ssq2
  ssq <- c(ssq1,ssq2,ssqt)
  names(ssq) <- c("X","Y","Total")
  return(list(b=b,z=z,ssq=ssq))
}

load("V:/arjun_google_drive_docs/CourseWork/UCLA_Winter_2017/202b/HW3/tobacco.rda")
data = as.matrix(tobacco[,4:9])
outputs = as.matrix(tobacco[,1:1])

lsq = lsfit(data,outputs)
tot_least_sq =  tls(data, outputs)

tls_ycap = tot_least_sq$z
ols_ycap = X.toba %*% lsq$b
plot(Y.toba[,1], fitted_Y_tls[,1], pch=0, ylim = c(1, 3), main = "burn rate")
points(Y.toba[,1], fitted_Y_ols[,1], pch=1)
legend(x = 2.0, y = 3.0, legend = c("tls", "ols"), pch= c(0, 1))



= $$\left[\begin{array}
           {r}
           X^t \:\:\: \sqrt{k}I_{r}\\
           \end{array}\right]
\left[\begin{array}
       {r}
       X \\
       \sqrt{k}I_{r}\\
       \end{array}\right]
)^{-1} \left[\begin{array}
              {r}
              X^t \:\:\: \sqrt{k}I_{r}\\
              \end{array}\right]\left[\begin{array}
                                       {r}
                                       Y \\
                                       0\\
                                       \end{array}\right]

$$