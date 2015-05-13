##===========================================================================================
## This file provides a mechanism for caching the calculation of the inverse of a matrix:
## when getting repeatedly the inverse of a matrix, the inverse is calculated only once,
## and later calls use a saved value of this inverse without actually doing the calculation again.
##
## m <- makeCacheMatrix(myMatrix) creates a new "cached matrix" object m whose content is myMatrix.
##
## m <- makeCacheMatrix() creates an object whose content is an empty matrix.
##
## m$get() returns the content matrix of m.
##
## m$set(myNewMatrix) changes the content matrix of m.
##
## cacheSolve(m) gets the inverse of the matrix stored in m. This can be called repeatedly: the
## inverse of the matrix will be calculated only on the first call, and all the consecutive calls
## will use this pre-calculated value.
##


##-------------------------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse.
## In other terms, the created object holds not only the content of the matrix but also,
## possibly, the inverse of this matrix.
## 
## The function arguments are:
##  - matrixData: a matrix for which we want to cache its inverse. We assume that this matrix
##      is always invertible, and thus we won't take any special precaution when calculating
##      its inverse. If this argument is not provided, the default content is an empty matrix,
##      which is not invertible: the 'set' method must be used later on this object to give it
##      a proper matrix content.
## The return value is a list of functions. Those are used to get and set the matrix data
## and its inverse for this specific instance of special "matrix" object.

makeCacheMatrix <- function(matrixData = matrix()) {
    ## cachedInverse is the inverse of the current matrix.
    ## Its value is not yet known when creating the current cached matrix object:
    ## it will be provided later, the first time the associated function cacheSolve() is called.
    cachedInverse <- NULL
    
    ## R scoping rules are such that matrixData and cachedInverse can be 'seen' by the
    ## functions setMatrix(), getMatrix(), setInverse(), getInverse() that are created
    ## just below.
    ## Note that these functions use the '<<-' operator so that they can modify
    ## these variables.
    
    ## this function is used to change the value of an existing cached matrix.
    ## Argument newMatrixData is a matrix whose content replaces the current matrix data.
    setMatrix <- function(newMatrixData) {
        ## replace the cached matrix data with this new content
        matrixData <<- newMatrixData
        ## the matrix has changed: we don't know (yet) about the inverse of this new matrix
        cachedInverse <<- NULL
    }
    
    ## this function is used to get the value (i.e. the matrix data) of a cached matrix.
    getMatrix <- function() {
        ## returns the matrix itself, i.e. the one that was provided when calling
        ## makeCacheMatrix() or the setMatrix() function.
        matrixData
    }
    
    ## this function is used to set the inverse (calculated separately) of the cached matrix.
    ## This value can be obtained later through the getInverse() function
    ## Argument inverseMatrix is the inverse of the matrix for the current cached matrix object.
    setInverse <- function(inverseMatrix) {
        ## keep the inverse matrix into cachedInverse
        cachedInverse <<- inverseMatrix
    }
    
    ## this function is used to get the inverse matrix, as cached into the current object.
    ## It returns NULL if this inverse matrix is not yet known, i.e. if it has not
    ## yet been calculated and cached through a previous call to setInverse().
    getInverse <- function() {
        ## returns the cached inverse of the matrix, or NULL
        cachedInverse
    }

    ## returns a list of 4 functions, used to manipulated the content of the newly created object.
    ## For example, if we execute
    ##      x <- makeCacheMatrix(myMatrix)
    ## x$get is the getMatrix() function that was created during the execution
    ## of makeCacheMatrix().
    ## This specific getMatrix() function 'sees' the matrixData and cachedInverse fields
    ## of the new 'x' special matrix object. Thus, when calling the x$get() function, we will
    ## obtain the 'myMatrix' value.
    list(   set = setMatrix,
            get = getMatrix,
            set_inverse = setInverse,
            get_inverse = getInverse)
}


##-------------------------------------------------------------------------------------------
## This function returns the inverse matrix of a special "matrix" object, that was previously
## created by makeCacheMatrix().
## This inverse is cached within the special matrix object, so that the inverse matrix
## is calculated only once, and later calls to this function will return the pre-calculated
## cached value instead of re-calculating the inverse of the matrix.
##
## The function arguments are:
##  - cachedMatrix: a cached matrix object previously created by makeCacheMatrix().
##  - all the other parameters are provided as-is to solve() for calculating
##      the inverse of the matrix
## The return value is the inverse of the matrix.

cacheSolve <- function(cachedMatrix, ...) {
    ## get the inverse matrix value that is cached into 'cachedMatrix'
    inverseMatrix <- cachedMatrix$get_inverse()

    if (is.null(inverseMatrix)) {
        ## the cached value returned is NULL.
        ## This means that the inverse matrix has never been calculated yet: do it now
        
        ## get the matrix data from the cached matrix object
        matrixData <- cachedMatrix$get()
        ## calculate its inverse, using any additional parameter provided to cacheSolve()
        inverseMatrix <- solve(matrixData, ...)
        ## store this inverse matrix into the cached matrix object
        cachedMatrix$set_inverse(inverseMatrix)
        ## at this point, inverseMatrix contains the proper value for the inverse of the matrix:
        ## we may use it as return value for this function.
    }
    else {
        ## just for debugging: tell that we could get cached data instead of calculating
        ## the inverse of the matrix
        message("getting cached data")
    }

    ## the return value is the inverse of the matrix, that was either found in the cache,
    ## or calculated if not found in the cache
    inverseMatrix
}

##-------------------------------------------------------------------------------------------
## This function tests makeCacheMatrix() and cacheSolve()
##
cacheTest <- function() {
	## 2x2 test matrix
	testMatrix <- matrix(c(1, 3, 8, -2), 2, 2)
	## directly calculate its inverse
	testInverse <- solve(testMatrix)
	## now, put the content of testMatrix into a cached matrix object
	cachedTestMatrix <- makeCacheMatrix(x)
	## get its inverse: this should calculate the inverse matrix
	inverse_1 <- cacheSolve(cachedTestMatrix)
	## get its inverse again: this should now use the cached value
	inverse_2 <- cacheSolve(cachedTestMatrix)
	## return TRUE if the values returned by the cached matrix object
	## have the same content as the calculated inverse
	identical(testInverse, inverse_1) && identical(testInverse, inverse_2)
}
