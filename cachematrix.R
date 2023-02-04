## Put comments here that give an overall description of what your
## functions do


## Map the variables holding matrix and inverse to a variable outside ths scope

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Return a matrix that is the inverse of 'x'.
## If cached value is present, use that value

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Testing
## Create Matrix
# > x <- matrix(c(1,2,2,3,1,2,3,3,1),3,3)

## Create Cache
# > X <- makeCacheMatrix(x)

## Solve the matrix - first run - no cache yet
# > cacheSolve(X)
#            [,1]       [,2]       [,3]
# [1,] -0.3846154  0.2307692  0.4615385
# [2,]  0.3076923 -0.3846154  0.2307692
# [3,]  0.1538462  0.3076923 -0.3846154

## Cache has been created
## Solve the matrix - second run
# > cacheSolve(X)
# getting cached data
#            [,1]       [,2]       [,3]
# [1,] -0.3846154  0.2307692  0.4615385
# [2,]  0.3076923 -0.3846154  0.2307692
# [3,]  0.1538462  0.3076923 -0.3846154

## "getting cached data" - it works!!!
