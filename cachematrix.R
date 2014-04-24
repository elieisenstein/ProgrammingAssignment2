## Matix inversion
## Calculates inversion of a matrix (assuming the matrix is invertible).
## If matrix is cached - output is the cached inverted matrix
## else - calculates the inverted matrix.
##Remark: whole concept, resembles singleton design pattern.


##Generates the chached matrix object
makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse<- function(inversed) inv <<-inversed
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Solve for inversion, use cached inverted matrix if available,
##calculates - otherwise
cacheSolve <- function(x, ...) 
{
        inv <- x$getinverse()
        if (!is.null(inv)) 
        {
                message("getting cached inverse matrix")
                return(inv)
        }
         
        data <- x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}
        
        
        
