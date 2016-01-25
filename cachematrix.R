
## Write a short comment describing this function

## makeCacheMatrix firstly clears the previous matrix in the cache. It has four 
## inner functions in total. This function can be used to get the inverse matrix
## if there is no stored one in the cache. makeCacheMatrix returns a 
## a list of defined functions that are further used by cacheSolve
## - 1) set - setting data for the matrix with clearing previous one 
## 2) get - getting matrix data, 3) setInverse - setting the inverse of given
## matrix, 4) getInverse - getting the inverse of given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get, seti = seti, geti = geti)
}



## cacheSolve functions uses if statement to determine
## if there is a matrix stored in the cache - if the
## matrix is there (the value of inversed matrix isn't null)
## it returns the stored matrix. Otherwise it is starting
## to calculate the inverse of the matrix. 


cacheSolve <- function(x, ...) {
  i <- x$geti()
  if (!is.null(i)) {
    print("data already in the cache -> accessing data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$seti(i)
  i
}