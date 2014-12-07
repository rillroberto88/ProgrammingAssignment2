# Function to test matrix inverse caching.
# Author: Rill Robert (December, 2014)
test <- function(n = 5, minNr = 1, maxNr = 10, eps = 10e-14) {
#Input parameters with default values:
# n     -  size of matrix
# minNr -  minimum element in matrix
# maxNr -  element in matrix
# eps   -  treshold for checking integer equality

# generate matrix of size n x n with
#   sampling with replacement between minNr and maxNr
M <- replicate(n, sample(minNr:maxNr, n, replace = TRUE))   

X <- makeCacheMatrix(M)   # create cache object for matrix
t1 <- Sys.time()
Xinv <- cacheSolve(X)     # calculate inverse of matrix
t2 <- Sys.time()
time1 <- t2 - t1
print("Note: no message about caching above!")
t1 <- Sys.time()
Xinv <- cacheSolve(X)     # again to see if cache works (message has to print)
t2 <- Sys.time()
time2 <- t2 - t1
print("Note message about caching above!")

print("Original matrix:") # print original matrix
print(X$get())
print("Inverse matrix:")  # print original matrix
print(Xinv, digits = 3)

id <- X$get() %*% Xinv    # identity matrix, hopefully

print("Numerical approximation of matrix * inverse:")
print(id, digits = 3)
print("Product rounded:")
print(round(id))


if (sum(abs(id - diag(n)) < eps) == n*n) {
	print("Inverse calculated is CORRECT!")
} else {
	print("Inverse calculated is INCORRECT!")
}
print("Comparison of times elapsed with/without caching:")
print(c("  Without caching: ", time1))
print(c("  With    caching: ", time2))
}