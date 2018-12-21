## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix gets a matrix as a parameter and creates a variable z to store an inverse of that matrix. 
#A set of functions will also be created that return or set the inverse matrix. These will be stored in a list from where these sub-functions can be called.
# the set sub-function takes y as a parameter and replaces the matrix x in the outer environment with y and sets z in the outer environment as NULL
makeCacheMatrix <- function(x = matrix()) {
    z = NULL
    set <- function(y){
        x<<-y
        z<<-NULL
    }
    get<- function()x
    setinverse <- function(inverse) z <<- inverse
    getinverse <- function() z
    list(setinverse = setinverse, getinverse = getinverse)
}



# Cache solve gets an object x that is created by giving makeCacheMatrix a matrix. 
# The inverse of this matrix is assigned to variable Z. This is done by calling the getInverse fuction on the parameter x.
# If there already exists an inverse matrix z will be returned. If not, the matrix will be assigned from x to variable called "matrix" and the inverse will be solved. After this the inverse matrix will be stored in the x object.
cacheSolve <- function(x, ...) {
        z <- x$getInverse()
        if(!is.null(z)){
            print("was not null")
            return(z)
        }
        matrix <- x$get()
        z <- solve(matrix)
        x$setInverse(z)
        z
}
