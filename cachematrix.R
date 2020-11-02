#  Lexical scoping
## Scoping is the mechanism within R that determines how R finds symbols (i.e. programming language elements)
##    to retrieve their values during the execution of an R script.
## Lexical scoping is used to retrieve values from objects based on the way functions are nested when they were written. 

# Cache Function
## A cache is a way to store objects in memory to accelerate subsequent access to the same object. 
## x and m are the key objects

makeCacheMatrix <- function(x = matrix()) { # x is initialized as a function argument and its default value is an empty matrix
   # m creates an empty data object to be used by later code within the function
   m <- NULL
   
   # `set` is the function y; `set` takes an argument named y and assumes it is a matrix
   set <- function(y) {
      
      # <<- form of the assignment operator, which assigns the value on the right side of the operator to an object in the parent 
         # environment named by the object on the left side of the operator
      x <<- y
      
      # Assign the value of NULL to the m object in the parent environment. 
      # This line of code clears any value of m that had been cached by a prior execution of cacheSolve().
      m <<- NULL      
   }
   
   ## since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
   get <- function() x
      
   # Since m is defined in the parent environment and we need to access it after setinverse() completes, 
   # the code uses the <<- form of the assignment operator to assign the input argument to the value of m in the parent environment.
   setinverse <- function(solve) m <<- solve
   
   # R takes advantage of lexical scoping to find the correct symbol m to retrieve for its value.
   getinverse <- function() m
   # last section of code assigns each of these functions as an element within a list(), and returns it to the parent environment.
   list(set = set, 
        get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}
#------------------------------
# Function cacheSolve is required to populate and/or retrieve the inverse from an object of type makeCacheMatrix()
## REQUIRES an input argument of type makeCacheMatrix() 
## if one passes a regular vector to the function, the function call will fail with an error 

cacheSolve <- function(x, ...) {
   # Attempts to retrieve the inverse matrix from the objects passing as the argument
   m <- x$getinverse()
   # it first checks to see if the inverse has already been calculated. 
   #If so, it gets the inverse from the cache and skips the computation. 
   if(!is.null(m)) {
      message("getting cached data")
      # `return` returns it to the parent environment
      return(m)
   }
   # if '!is.null(m)' is false, gets the input object and calculates the inverse
   data <- x$get()
   m <- solve(data, ...)
   # returns the inverse to the parent environment by printing the inverse object
   x$setinverse(m)
   m
}






