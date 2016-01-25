## In this assigment I wrote two functions that are used to create a
## special object that stores a numeric vector and caches its mean.
## This is important because the new functions allow me to cache computations 
## (i.e. store a list of functions) that 
## may be too time consuming to constantly rewrite. In other words, by simply
## recalling the contents of a vector (that are not changing) in the cache I 
## can save valuable computational time. 

## Created function `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }

## Created function `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. It first checks to see if the
## mean has already been calculated. If the mean has been calculated, 
## it `get`s the mean from the cache and skips the computation. 
## If the mean has not been calculated, it calculates the mean of the data and sets the 
## value of the mean in the cache via the `setmean`function.

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
