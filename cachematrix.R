## function makeCacheMatrix caches the value of a matrix and its inverse.
## function cacheSolve returns the cached inverse matrix if it exists otherwise it calculates
## the inverse and has it cached by makeCacheMatrix.

## makeCacheMatrix caches both the value of a matrix and its inverse.
## It returns a list with each element being 1 of the 4 sub-functions
## defined within this function

## To run with any square matrix M, the list vec must first be initialized with
## the call vec <- makeCacheMatrix(M).  There after cacheSolve can be called as
## many times as required.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y){
           x <<- y
           m <<- NULL
         }
       get <- function() x
       setinv <- function(inv) m <<- inv
       getinv <- function() m
       list(set = set, get = get, setinv = setinv, getinv = getinv)
       }
    
## cacheSolve first tests to see if the matrix has changed from the previously cached matrix. 
## If it has changed then it calculates the new inverse matrix and has both the matrix and its inverse
## cached by makeCacheMatrix.
## If it has not changed then it tests to see if the inverse is already cached.  If so, the cached inverse is returned.
## If the inverse is not cached, then it calculates the inverse and has both the matrix and its inverse
## cached by makeCacheMatrix.
    
cacheSolve <- function(x) {
## get cached value of matrix and test to see if matrix has changed
          y <- vec$get()
          if (all(x == y))
               {
## matrix has not changed so test to see if inverse is cached
                  m <- vec$getinv()
                  if(!is.null(m)){
## inverse is cached so return cached value
                        message("getting cached inverse")
                        return(m)
                      }
## inverse is not cached so calculate inverse and have it cached
                  message("calculating matrix inverse")
                  data <- x
                  vec$set(x)
                  m <- solve(data)
                  vec$setinv(m)
                  m}
          else {message("matrix changed, calculating new inverse")
## matrix has changed so calculate new inverse and have it cached
                        data <- x
                        vec$set(x)
                        m <- solve(data)
                        vec$setinv(m)
                        m}
          }