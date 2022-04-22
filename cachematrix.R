## Basically, this works like the provided 
## makeCacheMean()/cacheMean() example, only with a different
## function, namely solve() instead of mean()
## The following resource helped a lot in understanding this:
## https://github.com/lgreski/datasciencectacontent/
## blob/master/markdown/rprog-breakingDownMakeVector.md

## makeCacheMatrix() provides functions for storing
## the original matrix and the inverse. It is initialized with
## a matrix object. The functions and the data, i.e. the matrix, 
## exist in the environment of makeCacheMatrix.

## By default, initialize the function with an empty matrix.
## This prevents errors when the get() function is called

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## Initialize the inverse
  
  ## Provide getters and setters
  set <- function(y) {
    x <<- y ## Assign the argument to the parent environment
    i <<- NULL  ## Initialize the inverse in the parent environment.
                ## Clears any cached value.
  }
  
  ## Retrieve x from the parent environment
  get <- function() x 
  
  ## Assign argument to the inverse (i) in the parent environment
  setinverse <- function(inverse) i <<- inverse
  
  ## Retrieve i from the parent environment
  getinverse <- function() i
  
  ## Create a named list and return it to the parent environment
  list(set = set, ## Give the name "set" to the set() function
       get = get, ## Give the name "get" to the get() function
       setinverse = setinverse, ## Give the name "setinverse" to the 
                                ## setinverse() function
       getinverse = getinverse ## Give the name "getinverse" to the 
                                ## getinverse() function
       )
  ## That way we can use the $ operator and 
  ## don't have to access the functions like so: myMatrix[[2]]()
}

## cacheSolve() operates on the object that contains the 
## makeCacheMatrix() functions and data. 
## A regular matrix will not work!
## cacheSolve() checks whether makeCacheMatrix$getinverse() 
## returns a matrix object; then it either 
## - returns the cached result or
## - calculates it and stores it with makeCacheMatrix$setinverse()

cacheSolve <- function(x, ...) {
  ## Request the matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## If there is an actual inverse matrix there, 
  ## return that.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Otherwise, get the original matrix and solve it
  data <- x$get()
  i <- solve(data, ...)
  ## Then set the inverse...
  x$setinverse(i)
  ## and return the inverse
  i  
}