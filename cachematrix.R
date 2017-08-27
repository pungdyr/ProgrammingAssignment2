### Put comments here that give an overall description of what your functions do
# The makeCacheMatrix() function and the cacheSolve() function, are capable of returning the inverse of a matrix. This is a memory-costly computation. If this exact matrix inversion has been calculated previously, the function pair will return the cached inverse (stored in memory). Otherwise, the calculation will be performed.

### Write a short comment describing this function
# The makeCacheMatrix() function first creates a matrix object that can cache the inverse.
# Objects x and m are initialized to the makeCacheMatrix()-environment.
# The input argument is then assigned ('set') to the x object in the parent environment, and 'NULL' is assigned to the m object - also in the parent environment. The latter command clears any value of m that has previously been cached.
# In the final part, so called 'getters' and 'setters' are defined for both data objects. These are placed as named elements within a list - named, so that we (in makeCacheMatrix()) can call the $-form extract operator to access the functions.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
	}
	get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

### Write a short comment describing this function
# The cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}