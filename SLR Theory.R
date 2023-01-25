
# function for join density of Bivariate Normal
biv.norm<-function(x,y,mu.x,mu.y,sigma2.x,sigma2.y,rho){
  (
    (1/(2*pi*sqrt((1-rho^2)*sigma2.x*sigma2.y)))
    *exp( -(1/(2*(1-rho^2)))*( (x-mu.x)^2/sigma2.x
                               - (2*rho*(x-mu.x)*(y-mu.y)/sqrt(sigma2.x*sigma2.y))
                               + (y-mu.y)^2/sigma2.y))
  )
}

x<-seq(-2.5,12.5,length=50)
y<-seq(-1,7,length=50)

par(mfrow=c(1,2))
# a 3-D plot
persp(x,y,
      outer(x,y,biv.norm,mu.x=5,mu.y=3,sigma2.x=6,sigma2.y=2,rho=-0.9),
      theta=-40,phi=30,
      xlab="x",ylab="y",zlab="f(x,y)",main="Bivariate Normal")

# a contour plot
contour(x,y,
        outer(x,y,biv.norm,mu.x=5,mu.y=3,sigma2.x=6,sigma2.y=2,rho=-0.9),
        xlab="x",ylab="y",main="Bivariate Normal",drawlabels=FALSE)

# overlay line for E(Y | X=x) = mu.y + rho sigma.y/sigma.x (x-mu.x)
lines(x,3+(-0.9)*sqrt(2)/sqrt(6)*(x-5),col="royalblue")
par(mfrow=c(1,1))





# Simulation study to investigate discrepancy
#   between estimated and true line

# True Model is
#
#
# choose beta0 = 0, beta1 = 1, sigma2 = .09
# choose n = 10

set.seed(2319)

# choose X's that are equally spaced between 0 and 1
# n = 10
x<-seq(0,1,length=10)
y<- 0 + 1*x + rnorm(10,0,sqrt(0.09))
# estimated line
sim1<-lm(y~x)
# difference between estimated and true
coef(sim1) - c(0,1)
# plot demonstrating discrepancy
plot(x,y)
abline(0,1,col="black")
abline(coef(sim1),col="red")




  