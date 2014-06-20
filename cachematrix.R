####        cachematrix.R       Dick Lane, 2014-June-11
####    create an object containing a matrix and its inverse

####    makeCacheMatrix( x = matrix() )
##  input:  non-singular matrix [invertibility is not checked]
##  output: list of two functions
#       getMat: returns value of original matrix
#       getInv: computes & caches inverse of matrix on first call,
#               returns inverse of matrix
##  usage:
#       a <- makeCacheMatrix( matrix( c(3,2,7,5), 2,2) )
#       a$getMat()      ##  returns the original 2 by 2 matrix
#       a$getInv()      ##  returns inverse = matrix(c(5,-2,-7,3),2,2)
makeCacheMatrix <- function( x = diag(c(4,-2,1/8)) ) {
    J <- NULL  ##  cached inverse of x is NULL until GetInv changes its state

    GetMat <- function() x

    GetInv <- function() {
        if (is.null(J)) {
            message("calculate and cache inverse, then return the inverse")
            J <<- solve(x)    ##  store inverse in cache
## NOTE: this assignment is done in environment of makeCacheMatrix
##  and cached within the object produced by makeCacheMatrix
        } else {
            message("returning cached inverse")
        }
        J
    }

    ##  return list of two functions
    list( getMat = GetMat , getInv = GetInv )
####    NOTE: difference in names is intended to
####        distinguish between internal and external aspects
}


####        cacheSolve
##  input:  object produced by makeCacheMatrix
##  output: inverse of the original matrix
####    NOTE: assignment mandates this name and its return value,
####    BUT assignment only requires the inverse be computed on first call
####    AND cached value of inverse be returned on subsequent calls
##  usage:
#       a <- makeCacheMatrix( matrix( c(3,2,7,5), 2,2) )
#       cacheSolve( a )     ##  GetInv displays a message, returns inverse
cacheSolve <- function(x) {
    ## Return the inverse of the matrix component of x
    x$getInv()
}


###########################################################

####    example of use
cat("Output of 'x <- matrix( c(3,-2,-7,5) , nrow=2 , ncol=2 )' is\n")
x <- matrix( c(3,-2,-7,5) , nrow=2 , ncol=2 )
print(x)

cat("\n Output of 'y <- makeCacheMatrix( x )' is\n")
y <- makeCacheMatrix( x )
print(y)

cat("First use of 'z <- cacheSolve( y )' displays a message")
z <- cacheSolve( y )
cat("and its output is\n")
print(z)

cat("Confirm inverse was obtained by computing matrix product  'x %*% z'\n")
print(round( x %*% z ) )    ##  identity matrix of rank 2

cat("\n Subsequent use of 'cacheSolve(y)' reports")
print( cacheSolve( y ) )
