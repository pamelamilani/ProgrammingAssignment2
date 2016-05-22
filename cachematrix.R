makeCacheMatrix <- function(M = matrix(1)) {
        # Initialize the inverse to null
        inverse <- NULL
        set <- function(M1) {
                M <<- M1
                # The matrix is changed so any previously computed
                # inverse is invalid, reset to null
                inverse <<- NULL
        }
        get <- function() M
        setInverse <- function(inverse_1) inverse <<- inverse_1
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(M) {
        inverse <- M$getInverse()
        if(!is.null(inverse)) {
                # We already computed this invers, return the cached
                # value
                message("getting cached data")
                return(inverse)
        }
        data <- M$get()
        inverse <- solve(data)
        M$setInverse(inverse)
        inverse
}

# The same cache logic can also be implemented as a single object:
matrixWithInverse <- function(M = matrix()) 
{
        inverse <- NULL
        set <- function(M1) {
                M <<- M1
                inverse <<- NULL
        }
        get <- function() M
        getInverse <- function(){
            if(is.null(inverse)){
                inverse <<- solve(M)
            }
            else{
                message("using cached data")
            }
            inverse
        }
        list(set = set, get = get, getInverse = getInverse)
}