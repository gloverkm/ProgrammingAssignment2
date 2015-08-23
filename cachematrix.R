## The objective of these two functions are to calculate the inverse of matrices

## I think this solution is very similar to the example in our assignment, except 
## in this case we are first creating a vector as a special "matrix" object where 
## the inverse of the matrix can be calculated in the next function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list (set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Again I think the solution is similar to the example, except solve is used. 
## I am hoping this function can retrieve the inverse from the cache. But when
## I test it I get numbers I don't understand? Hopefully I can find a way to 
## figure out what these numbers are before the deadline. Reviewers: perhaps
## I do not understand what an inverse matrix is? Why isn't it the inverse of 
## numbers in the matrix? My example is below. Okay, checked the discussion 
## forum. Inverse matrix best explained on youtube link posted by Courtney:
## https://www.youtube.com/watch?v=y4B_EC5MNS8. I believe I get what
## the math is doing, but I wish I could see context, so I could learn from it.
## ie, when would I ever need to do this, or how could I use this to solve
## something in the future.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##> data <- makeCacheMatrix(matrix(c(4, 1, 1, 4), c(2, 2)))
##> cacheSolve(data)
##      [,1]        [,2]
##[1,]  0.26666667 -0.06666667
##[2,] -0.06666667  0.26666667
## How is this the inverse? What did I really do? So confused, but I 
## will check discussion boards, again. :)

