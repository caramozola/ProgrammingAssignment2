#cacheMatrix evaluates the matrix argument "x", caches and returns its inverse. The cached
#value is stored as "m". If cacheSolve is called again using the the same argument it will return
#"getting cached data" along with the saved value. If "x" is overwritten using the set function 
#it will recalculate the inverse, cache it, and return the new value.

#makeCacheMatrix takes a vector "x" as an argument, initiaizes "m" and sets it to "NULL",
#and defines 4 functions internally: set, get, set_inverse, and get_inverse. These functions
#set and return the values of x and m within makeCachematrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

#cacheSolve takes the output of makeCacheMatrix as an argument and checks the value of m to
#see if a value is stored already. If so, it returns the cached value. If m is "NULL"
#it will use solve() to determine the inverse, cache the value, and return this new value.

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

