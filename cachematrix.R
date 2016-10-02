
# After review run the below Example:)

# First, define the input. Let's say matrix 2 by 2 with rnorm

#initial = matrix (rnorm(4), 2, 2)


# Call makeCacheMatrix as input in the cacheSolve:
#inverse <- cacheSolve(makeCacheMatrix(initial))
#inverse

# Proof initial * inverse = I_2  (where I_2 = Identity matrix of dimension 2)
# proof <- initial %*% inverse
# proof


makeCacheMatrix <- function(x = matrix()) {
        # define an empty matrix. If the user gives as input the same matrix, then the cashed data will be returned!
        inverseMatrix <- NULL
        # Set the matrix
        set <- function(y = matrix() ) {
                x <<- y
                inverseMatrix <<- NULL
        }
        # return matrix
        get <- function() x
        # inverse the matrix
        setInverse <- function(solve) inverseMatrix <<- solve
        # return the inverse
        getInverse <- function() inverseMatrix 
        list(set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        # Tests whether the pre-defined matrix is not NULL so as return the cashed data 
        if(!is.null(inverseMatrix)) {
                message("getting cashed data")
                return(inverseMatrix)
        }
        # otherwise the copmputation is performed
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
