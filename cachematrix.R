## The following two functions help in caching the potentially time-consuming computation of calculating the inverse of a matrix


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL #inverse
    #set the matrix
    set <- function(y) {
      m <<- y
      i <<- NULL
    }
    #get the matrix
    get <- function() m
    #set the inverse
    setInverse <- function(inverse) i <<- inverse
    #get the inverse
    getInverse <- function() i
    #return the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    #check if the value already exists?
    if(!is.null(i)) {
      #if yes, then return the cached value
      message("getting cached data")
      return(i)
    }
    #if not, then create it
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
