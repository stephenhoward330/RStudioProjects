
# 2a 
# additive linear model example E(Y|X1,X2) = 2 + 3 X1 - 4 X2

x1 <- seq(0, 3, length = 50)
x2 <- seq(-1, 1, length = 50)

lmfig1fun<-function(x1,x2){ 
  2+3*x1-4*x2 
}

par(mfrow=c(2,2))

plot(x1,lmfig1fun(x1,-1),xlim=c(0,3), 
     xlab='X1',ylim=c(-2,15),ylab='E(Y|X1,X2)',type='l')
text(1.5,12,'X2=-1')
lines(x1,lmfig1fun(x1,0),lty=1)
text(1.5,8,'X2=0')
lines(x1,lmfig1fun(x1,1),lty=1)
text(1.5,4,'X2=1')

persp(x1,x2, outer(x1,x2, FUN = lmfig1fun),
      theta=330,phi=45,r=20,
      xlab='X1',ylab='X2',zlab='E(Y|X1,X2)',
      xlim=c(0,3),ylim=c(-1,1),zlim=c(-2,15),ticktype='detailed')

contour(x1,x2, outer(x1,x2, FUN = lmfig1fun),xlab='X1',ylab='X2')

image(x1,x2,outer(x1,x2, FUN = lmfig1fun),
      xlab='X1',ylab='X2',col=heat.colors(50))

par(mfrow=c(1,1))



# 2b
# additive linear model example
# E(Y|X,Z) = 14 - 8 X + 2 Z
#
#           = 14 - 8 X if Z=0
#           = 16 - 8 X if Z=1

x <- seq(0, 1, length = 50)

lmfig2fun <- function(x, z){
  14-8*x+2*z
}

plot(x,lmfig2fun(x,0),
     xlim=c(0,1),xlab='X',ylim=c(5,17),ylab='E(Y|X,Z)',type='l')
text(.5,8.5,'Z=0')
lines(x,lmfig2fun(x,1),lty=1)
text(.5,14,'Z=1')



# All-star game data
# 4a
Y<-matrix( c( 303.3, 467.1, 422.8, 391.6, 403.8, 373.8, 263.5, 226.8, 183.9,
              208.4, 208.2, 168.8, 245.4, 160.0, 173.2),
           nrow=15, ncol=1, byrow=TRUE)

# 4b
X<-matrix( c( 1, 3, 1, 8, 1, 1,
              1, 6, 3, 3, 1, 2,
              1, 4, 1, 4, 1, 5,
              1, 5, 1, 3, 1, 1,
              1, 7, 1, 3, 3, 1,
              1, 6, 1, 3, 5, 2,
              1, 6, 1, 8, 5, 3,
              1, 5, 1, 8, 3, 2,
              1, 1, 3, 4, 2, 1,
              1, 3, 5, 2, 2, 4,
              1, 2, 3, 3, 1, 3,
              1, 1, 4, 3, 2, 3,
              1, 6, 5, 3, 1, 5,
              1, 2, 1, 5, 2, 2,
              1, 5, 1, 4, 2, 1),
           nrow=15,ncol=6,byrow=TRUE)

# 4c

beta.hat<-solve(t(X)%*%X)%*%t(X)%*%Y
beta.hat

# 1d
sigma2<-(t(Y-X%*%beta.hat)%*%(Y-X%*%beta.hat))/(nrow(X)-ncol(X))
sigma2

# 1e
C<-matrix(c(0,0,1,0,0,0),nrow=1,ncol=6,byrow=TRUE)

sqrt(sigma2*(C%*%(solve(t(X)%*%X)%*%t(C))))

# 1f
C<-matrix(c(0,0,0,1,0,0),nrow=1,ncol=6,byrow=TRUE)

C%*%beta.hat/sqrt(sigma2*(C%*%(solve(t(X)%*%X)%*%t(C))))

2*(1-pt(abs(C%*%beta.hat/sqrt(sigma2*(C%*%(solve(t(X)%*%X)%*%t(C))))),nrow(X)-ncol(X)))

# 1g
C<-matrix(c(0,0,1,1,1,1),nrow=1,ncol=6,byrow=TRUE)

C%*%beta.hat/sqrt(sigma2*(C%*%(solve(t(X)%*%X)%*%t(C))))

2*(1-pt(abs(C%*%beta.hat/sqrt(sigma2*(C%*%(solve(t(X)%*%X)%*%t(C))))),nrow(X)-ncol(X)))

# 1a
Xr<-matrix( c( 1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1,
              1),
           nrow=15,ncol=1,byrow=TRUE)

beta.hatr<-solve(t(Xr)%*%Xr)%*%t(Xr)%*%Y

SSE<-t(Y-X%*%beta.hat)%*%(Y-X%*%beta.hat)
SSEr<-t(Y-Xr%*%beta.hatr)%*%(Y-Xr%*%beta.hatr)

df<-nrow(X)-ncol(X)
dfr<-nrow(X)-ncol(Xr)

Fstat<-((SSEr-SSE)/(dfr-df))/(SSE/df)
p.value<-(1-pf(Fstat,dfr-df,df))

# 1b
Xr<-matrix( c( 1, 1, 8, 1, 1,
               1, 3, 3, 1, 2,
               1, 1, 4, 1, 5,
               1, 1, 3, 1, 1,
               1, 1, 3, 3, 1,
               1, 1, 3, 5, 2,
               1, 1, 8, 5, 3,
               1, 1, 8, 3, 2,
               1, 3, 4, 2, 1,
               1, 5, 2, 2, 4,
               1, 3, 3, 1, 3,
               1, 4, 3, 2, 3,
               1, 5, 3, 1, 5,
               1, 1, 5, 2, 2,
               1, 1, 4, 2, 1),
            nrow=15,ncol=5,byrow=TRUE)

beta.hatr<-solve(t(Xr)%*%Xr)%*%t(Xr)%*%Y

SSE<-t(Y-X%*%beta.hat)%*%(Y-X%*%beta.hat)
SSEr<-t(Y-Xr%*%beta.hatr)%*%(Y-Xr%*%beta.hatr)

df<-nrow(X)-ncol(X)
dfr<-nrow(X)-ncol(Xr)

Fstat<-((SSEr-SSE)/(dfr-df))/(SSE/df)
p.value<-(1-pf(Fstat,dfr-df,df))

# 1c
Xr<-matrix( c( 1, 3,
               1, 6,
               1, 4,
               1, 5,
               1, 7,
               1, 6,
               1, 6,
               1, 5,
               1, 1,
               1, 3,
               1, 2,
               1, 1,
               1, 6,
               1, 2,
               1, 5),
            nrow=15,ncol=2,byrow=TRUE)

beta.hatr<-solve(t(Xr)%*%Xr)%*%t(Xr)%*%Y

SSE<-t(Y-X%*%beta.hat)%*%(Y-X%*%beta.hat)
SSEr<-t(Y-Xr%*%beta.hatr)%*%(Y-Xr%*%beta.hatr)

df<-nrow(X)-ncol(X)
dfr<-nrow(X)-ncol(Xr)

Fstat<-((SSEr-SSE)/(dfr-df))/(SSE/df)
p.value<-(1-pf(Fstat,dfr-df,df))





# Video game data
# 6
Y<-matrix( c( 10, 8, 9, 10, 6, 5, 7, 7),
           nrow=8, ncol=1, byrow=TRUE)
X<-matrix( c( 0, 0, 0, 1,
              0, 1, 0, 0,
              0, 0, 1, 0,
              0, 0, 0, 1,
              0, 1, 0, 0,
              1, 0, 0, 0,
              0, 0, 1, 0,
              1, 0, 0, 0),
           nrow=8, ncol=4, byrow=TRUE)

beta.hat2<-solve(t(X)%*%X)%*%t(X)%*%Y
beta.hat2

Xr<-matrix( c( 1,
               1,
               1,
               1,
               1,
               1,
               1,
               1),
            nrow=8,ncol=1,byrow=TRUE)

beta.hat2r<-solve(t(Xr)%*%Xr)%*%t(Xr)%*%Y

SSE2<-t(Y-X%*%beta.hat2)%*%(Y-X%*%beta.hat2)
SSE2r<-t(Y-Xr%*%beta.hat2r)%*%(Y-Xr%*%beta.hat2r)

df<-nrow(X)-ncol(X)
dfr<-nrow(X)-ncol(Xr)

Fstat<-((SSE2r-SSE2)/(dfr-df))/(SSE2/df)
p.value<-(1-pf(Fstat,dfr-df,df))



