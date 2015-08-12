# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function () {
		x
	}
	setmean <- function (mean) {
		m <<- mean
	}
	getmean <- function () {
		m
	}
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}


# Computes the inverse of the special matrix returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix hasn't changed
# then the cachSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	m <- x$getmean()
	if (!is.null(m)) {
		message("GETTING CACHED VALUE")
		return(m)
	}
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}