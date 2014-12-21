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
## `x_inv' is the memoized variable that caches the inverse
## `inverse' is an accessor function for the inverse of the given matrix (`x_inv')
## `cache' is a function to set the value of `x_inv', and conveniently returns it
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    inverse <- function() x_inv
    cache <- function(inv) { x_inv <<- inv; x_inv }
    list(matrix = x, inverse = inverse, cache = cache)
}

## Returns the inverse of `x$matrix', potentially retrieving it from cache
cacheSolve <- function(x, ...) {
    if (!is.null(x$inverse())) {
        # message("cache hit")
        return(x$inverse())
    }
    x$cache(solve(x$matrix, ...)) # n.b. this *returns* the inverse in addition to caching it
}
