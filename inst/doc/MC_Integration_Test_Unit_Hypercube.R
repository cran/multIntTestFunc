## ----integrate_Rn-------------------------------------------------------------
require(multIntTestFunc)
set.seed(42)

n <- as.integer(4) #the dimension needs to be of type "integer"
coeffs <- 1:n #get example coefficients
f <- new("unitCube_cos2",coeffs=coeffs,dim=n) #create instance

#print some information on the function
print(getIntegrationDomain(f))
print(getTags(f))
print(getReferences(f))

#evaluate integral by simple Monte Carlo integration
N <- 10000 #choose number of node points
U <- matrix(runif(N*n),N,n) #arrange points in matrix

D <- domainCheck(f,U) #check if the points are in the integration domain
U <- U[D,] #select the points in the integration domain (here f is supported over C_n so we take all points)
U <- as.matrix(U,ncol=n) #arrange points in matrix - only necessary because for n=1 we might get a vector, but we need a matrix input for our evaluate function

eval <- evaluate(f,U) #evaluate the function f on the points U
approx <- mean(eval) #approximate the expectation by average
print(approx)

#print exact integral
print(exactIntegral(f))

