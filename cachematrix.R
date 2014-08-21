## create  list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
##
## using (for example):
## source("cachematrix.R")
## a <- makeCacheMatrix(matrix(1:4, nrow <-2, ncol <-2))
## > a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
makeCacheMatrix <- function(x = matrix()) {
  invmatr <- NULL
  set <- function(y) {
    x <<- y
    invmatr <<- NULL
  }
  get <- function() x
  setinvmatr <- function(solve) invmatr <<- solve
  getinvmatr <- function() invmatr
  list(set = set, get = get,
    setinvmatr = setinvmatr,
    getinvmatr = getinvmatr)  
}

## Calculate the inverse matrix of list from makeCacheMatrix function
## Before calculating it analyze cache for previous calculations 
## (for except the same calculation)
## 
## using (for example):
## source("cachematrix.R")
## a <- makeCacheMatrix(matrix(1:4, nrow <-2, ncol <-2))
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
> 
  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatr <- x$getinvmatr()
  if(!is.null(invmatr)) {
    message("getting cached data")
    return(invmatr)
  }
  data <- x$get()
  invmatr <- solve(data, ...)
  x$setinvmatr(invmatr)
  invmatr
}
