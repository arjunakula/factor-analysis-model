#ridge regression function from lecture  3 slides
ridge<-function(x,y,kappa=0) {
  b<-solve(crossprod(x) +kappa*diag(ncol(x)),crossprod(x,y))
  ssq<-sum((y-x%*%b)^2)
  return(list(b=b,ssq=ssq))
}
#slightly modified ridge trace plotting function
#from lecture 3 slides
ridgeTrace<-function(x,y,kmax,nmax) {
  kk <- seq(0,kmax,length=(nmax+1))[-1] #discard 0 because crossproduct won't be invertible
  bb<-array(0,c(ncol(x),nmax)) #number variables by number kappas
  for (i in 1:nmax){
    bb[,i]<-ridge(x,y,kappa=kk[i])$b #set ith column to ridge trace for kappa i
  }
  plot(kk,seq(min(bb),max(bb),length=nmax),
    type="n",xlab="kappa",ylab="B")
  for (i in 1:ncol(x)){
    lines(kk,bb[i,])
  }
}



load('pet.rda')
data = as.matrix(pet[,1:268])
outputs = as.matrix(pet[,269, drop = FALSE])
mean_X = scale(data, center = T, scale = F)
mean_Y = scale(outputs, center = T, scale = F)
#ridgeTrace(mean_X, mean_Y, 1, 250)
mean_X = scale(mean_X);
mean_Y = scale(mean_Y)
split_val = seq(0,0.1,length=101)[-1]
cr = rep(NaN, 100);
OSD = rep(NaN, 100)
for(i1 in 1:100)
{
  tempo = matrix(NaN, nrow = nrow(mean_X), ncol = ncol(mean_Y));
  
  for(i2 in 1:nrow(mean_X))
  {
    newX = mean_X[-i2, ,drop = FALSE];
    newY = mean_Y[-i2, ,drop = FALSE];
    tempo[i2,] = mean_Y[i2,,drop = FALSE] - cbind(1,mean_X[i2,,drop = FALSE]) %*% ridge(newX, newY, split_val[i1])$b
  }
  cr[i1] = mean(tempo^2)
  OSD[i1] = sd(tempo^2)/nrow(mean_X)
}
