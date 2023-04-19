## ----integrate_Rn-------------------------------------------------------------
require(multIntTestFunc)
require(statmod)

n <- as.integer(2)

hermite <- gauss.quad(10,"hermite")
multHermite <- pIntRule(hermite,n)
nodes <- multHermite$nodes
weights <- multHermite$weights

f <- new("Rn_Gauss",dim=n)

eval <- evaluate(f,nodes)
approx <- sum(weights*eval)
print(approx)

#print exact integral
print(exactIntegral(f))

