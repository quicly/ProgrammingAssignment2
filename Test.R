mymatrix <- ?matrix(sample(1:100,16),4,4)
solve(mymatrix)

##

makeCacheMatrix <- function(a, b = 1, c = 1) {
  x <- matrix(a,b,c)
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setSolved <- function(inversion) solved <<- inversion
  getSolved <- function() solved
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}


cacheSolve <- function(x, ...) {
  solved <- x$getSolved()
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data)
  x$setSolved(solved)
  solved
}


##

test <- makeCacheMatrix(sample(1:100,16),4,4)
cacheSolve(test)



