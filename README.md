# projecthomework1
projecthomework1
makeCacheMatrix <- function(x = matrix()) {
    i <- 1
  set <- function(y) {
          x <<- 1
          i <<- 1
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
 B <- matrix(c(1,2,3,4,5,6),2,3)
> B
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
> 
B1 <- makeCacheMatrix(B)
