#!/bin/R
##
## Coursera "R Programming" course, Assignment 2.
##
## Author: Edward Garson
## Date: December 2014
##
## Further info:
##   https://class.coursera.org/rprog-016/human_grading/view/courses/972581/assessments/3/submissions

## Return a list object that encapsulates a given matrix and its calculated inverse.
## `x_mat' keeps a reference to the given matrix (for comparison purposes)
## `x_inv' is the memoized variable that caches the inverse of the matrix
## `inverse' is an accessor function for the inverse of the given matrix (`x_inv')
## `cache' caches the given matrix along with its inverse, and conveniently returns the inverse
makeCacheMatrix <- function(x = matrix()) {
    x_mat <- x
    x_inv <- NULL
    inverse <- function() x_inv
    original <- function() x_mat
    cache <- function(mat, inv) {
        x_mat <<- mat
        x_inv <<- inv
        x_inv
    }
    list(matrix = x, inverse = inverse, cache = cache, original = original)
}

## Returns the inverse of `x$matrix', potentially retrieving it from cache.
## If the matrix has changed, recompute the inverse and update the cache.
cacheSolve <- function(x, ...) {
    if (cacheIsValid(x)) {
        ## message("cache hit")
        return(x$inverse())
    }
    x$cache(x$matrix, solve(x$matrix, ...)) # n.b. this returns the inverse (in addition to caching it)
}

## Return true if the cached matrix inverse is valid, requirements being:
## 1. the inverse has previously been computed
## 2. the underlying matrix for which the inverse was computed has not changed
cacheIsValid <- function(m) {
    !is.null(m$inverse()) && m$matrix == m$original()
}
