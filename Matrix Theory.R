
# Matrix Theory


# 1D

E <- matrix(c( 1, 3, -4, 
               5, -7, 2), nrow=2, ncol=3, byrow=TRUE)

F <- matrix(c( 3, -2, 5, 
               6, 9, 7), nrow=2, ncol=3, byrow=TRUE)

E+F
E-F
t(E+F)
t(E)+t(F)


# 2E

B<-matrix(c( 8, 3, 7, 
             -2, 5,-3),nrow=2,ncol=3,byrow=TRUE)
C<-matrix(c(-2, 5, 
            3, 7,
            6,-4),nrow=3,ncol=2,byrow=TRUE)
D<-matrix(c( 1, 2, 
             -3, 1, 
             2, 4),nrow=3,ncol=2,byrow=TRUE)

B%*%C
C%*%B

t(B%*%C)
t(C)%*%t(B)

B%*%(C+D) 
B%*%C + B%*%D 


# 8

A<-matrix(c( 7, -3, 2, 
             4, 9, 5),nrow=2,ncol=3,byrow=TRUE)

t(A)%*%A
A%*%t(A)

