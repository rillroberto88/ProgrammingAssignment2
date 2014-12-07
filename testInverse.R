test <- function(n = 5, minNr = 1, maxNr = 10) {
# n     -  size of matrix
# minNr -  minimum element in matrix
# maxNr -  element in matrix

# generate matrix of size n x n with
#   uniform random numbers between minNr and maxNr
M <- replicate(n,runif(n,minNr,maxNr))   

X <- makeCacheMatrix(M)   # create cache object for matrix
Xinv <- cacheSolve(X)     # calculate inverse of matrix
print("Note: no message about caching!")
Xinv <- cacheSolve(X)     # again to see if cache works (message has to print)
print("Note message about caching above!")

print("Original matrix:") # print original matrix
print(X$get(), digits = 3)
print("Inverse matrix:")  # print original matrix
print(Xinv, digits = 3)

id <- X$get() %*% Xinv    # identity matrix, hopefully

print("Numerical approximation of matrix * inverse:")
print(id, digits = 3)
print("Product rounded:")
print(round(id))


if (sum(id - diag(n) < 10e-14) == n*n) {
	print("Inverse calculated is CORRECT!")
} else {
	print("Inverse calculated is INCORRECT!")
}
}