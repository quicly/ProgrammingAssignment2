## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. This function takes a number of arguments what will be applied
## in conctructing matrix inside function. Get sub-function is used to see the content of matrix (x$get). 
## setSolved sub-function is used for creating the inverse matrix the content of which will be put 
## in the main function variable "solved". getSolved is used to show the content of inverse matrix. List was created to
## give a name for sunfunctions so that i can use them with $-operator


makeCacheMatrix <- function(a, ...) {
  x <- matrix(a, ...)
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



## Write a short comment describing this function


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


#Testing the function
testMatrix <- makeCacheMatrix(sample(1:100,16),4,4) #putting an 4x4 matrix with 16 random numbers in function
cacheSolve(testMatrix) # testing whether function works
cacheSolve(testMatrix) #checking whether fucntion returns cached value
