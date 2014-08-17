## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly

## Creates a special "matrix" object that can cache its inverse
## A list is returned with a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) get the Inverse of the matrix
## 4) get the Inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #the inverse of the matrix will be stored in m
    set <- function(y) {
        x <<- y
        m <<- NULL #reset the cached value
    }
    get <- function(){
        x  
    } 
    setinverse <- function(inverse) {
        m <<- inverse   
    }
    getinverse <- function(){
        m  
    } 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


#Sample usage:
matrix <- rbind(c(1,5), c(2,7)) 
specialmatrix <- makeCacheMatrix(matrix)
cacheSolve(specialmatrix)
#[,1]       [,2]
#[1,] -2.3333333  1.6666667
#[2,]  0.6666667 -0.3333333
cacheSolve(specialmatrix)
#getting cached data
#[,1]       [,2]
#[1,] -2.3333333  1.6666667
#[2,]  0.6666667 -0.3333333
modifiedmatrix <- rbind(c(1,4), c(5,5)) 
specialmatrix$set(modifiedmatrix)
cacheSolve(specialmatrix)
#[,1]        [,2]
#[1,] -0.3333333  0.26666667
#[2,]  0.3333333 -0.06666667
cacheSolve(specialmatrix)
#getting cached data
#[,1]        [,2]
#[1,] -0.3333333  0.26666667
#[2,]  0.3333333 -0.06666667
