
## The following is based on the example code by Peng et al for Assignment 2 
## of the Coursera course "Understanding R"
##
## These functions return the inverse of an inputted square matrix by
## either   (1) checking for the existance of and returning a previously
##              computed inverse matrix
##          (2) computing the matrix de novo using the "solve()" function
##              if a completed inverted matrix is not found.
## A third function, "makeSquareMatrix()" which accepts the variable "nm"
## makes a normally distributed test matrix of dimensions nm*nm is included
## to make a test matrix


### makeCacheMatrix() constructs a list of functions which
###         (1) Set the inputted matrix (setMatrix)
###         (2) Retrive the inputted matrix (getMatrix)
###         (3) Set the inverse matrix (setInverse)
###         (4) Retrieve the inverse matrix (getMatrix)
###
### This function list does not perform calculations but 
### provides a means of passing and accepting values to the global 
### environment.


makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverse <- function(solve) {
        m <<- solve
    }
    getInverse <- function() {
        m
    }
    list(
        setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

x$getInverse()

###
### If available, cacheSolve retrieves the pre-calculated inverted matirix 
### with the function getInverse().This is accomplished by calling the 
### funtion getInverse() and checking that the value returned is 
### not NULL.If not NULL, it returns the value, otherwise, it calculates the 
### inverse of the matrix and saves the value to memory with the function 
### setInverse().
### This is accomplished by first calling the function getMatrix() to retrive
### the inputted matrix, calling the base-package function solve() to 
### calculate the inversion, then setInverse() to store the inversion.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
        message("Getting cached matrix")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

makeSquareMatrix <- function(nm = 10,meanm = 1,sdm = .1) {
    nm2 <- nm * nm
    matrix(rnorm(nm2,meanm,sdm),nm,nm)
}

testMatrix <- makeSquareMatrix(100)

testMatrix
a <- makeCacheMatrix(testMatrix)


cacheSolve(a)

##########################



## Write a short comment describing this function
