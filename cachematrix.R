## Maked by : Juan Carlos Guanoluisa
## Quito Ecuador

## makeCacheMatrix creates in the environment and returns 
## a list of functions used by cacheSolve 
## to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        # stores the cacheMatrix value
        # initialize to NULL
        # this variable is created into makeCacheMatrix 
        
	  cacheMatrix <- NULL

        # Now we procced to create the matrix
        # in the working environment
        
	  set <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }

        # get the value of the matrix
        
	  get <- function() x
        
	  # invert the matrix and store in cacheMatrix
        
	  setMatrix <- function(inverse) cacheMatrix <<- inverse
        
	  # get the inverted matrix from cacheMatrix
        
	  getInverse <- function() cacheMatrix

        # return the created functions to the working environment
        
	  list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cacheMatrix 
cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cacheMatrix 
        cacheMatrix <- x$getInverse()

        # return inverted matrix from cacheMatrix if it exists
        # else create the matrix in working environment

        if (!is.null(cacheMatrix)) {
                message("getting cached data")

                # display matrix in console
                return(cacheMatrix)
        }

        # create matrix since it does not exist

        matrix <- x$get()

        # make sure matrix is square and invertible
        # if not, handle exception cleanly

        tryCatch( {
                # set and return inverse of matrix
                cacheMatrix <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cacheMatrix)
        } )

        # display matrix in console

        return (cacheMatrix)
}

