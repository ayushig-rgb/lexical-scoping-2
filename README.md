# lexical-scoping-2
#function 1
#The first function, makeVector creates a special “vector”, which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
#The following function calculates the mean of the special “vector” created with the above function. However, it first checks to see if the mean has already been calculated. If #so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean #function.

makeCacheMstrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

#cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not #changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
        m <- mean(data, ...)
        x$setmean(m)
        m
}
