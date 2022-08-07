## Put comments here that give an overall description of what your
## functions do
## The below functions are written in partial fulfillment of Peer graded assignment of R Programming 
## August 8, 2022; GitHub user: Achyut22
## Write a short comment describing this function
## This function is used to create a special "matrix" object that can cache its inverse
##library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
  makeCacheMatrix <- function(x = matrix())     ## defining the argument with the default mode of "matrix"
   { 
    inv <- NULL                                 ## initialize inv as NULL; as it will hold the inverse of the matrix
    set <- function(y)                          ## define the set function to assign new
     {                              
      x <<- y                                  
      inv <<- NULL                              ## if there is a new matrix, reset inv to NULL
     }
    get <- function()x                                ## defining the get fucntion - It returns value of the matrix argument
    setinv <- function(inverse) inv <<- inverse    ## Assigns the value of inv in parent environment
    getinv <- function(){
        inver<-ginv(x)
        inver%*%x            ##function to obtain inverse of the matrix
    }                      
    list(set = set, get = get,
         setinv = setinv, 
         getinv = getinv)  
  }


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" which is returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {             ##checking whether inverse is null
    message("getting cached data")
    return(inv)                   ##return inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
