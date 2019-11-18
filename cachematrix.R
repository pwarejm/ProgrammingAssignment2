## makeCacheMatrix and cacheSolve work together to calculate and cache the inverse of a matrix
## making use of lexical scoping

## makeCacheMatrix accepts a matrix as input and returns a list of four functions
## two getters and two setters

makeCacheMatrix <- function(x = matrix()) {
  in_verse <- NULL
  set <- function(y) {
    x <<- y
    in_verse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) in_verse <<- solve
  getinverse <- function() in_verse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve acceps a list of functions, checks whether the inverse has already been cached in the parent environment,
## returns it from cache or returns a newly calculated inverse if it is not already cached

cacheSolve <- function(x, ...) {
  in_verse <- x$getinverse()
  if(!is.null(in_verse)) {
    message("getting cached data")
    return(in_verse)
  }
  data <- x$get()
  in_verse <- solve(data, ...)
  x$setinverse(in_verse)
  in_verse
        ## Return a matrix that is the inverse of 'x'
}
