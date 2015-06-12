## Put comments here that give an overall description of what your
## functions do

# SR Strand, 6/11/2015
# The two functions in this file enable the calculation, storage and retrieval of a matrix inverse using 
# lexical scoping features of R to allow functions to have their own dedicated data storage.

# The two functions work together. 
# The first function handles storing and retrieving the original matrix and its inverse.
# The second function calculates the matrix inverse if it has not yet been computed, otherwise
# it will return the cached (stored) value.

# This approach to caching and storing an intermediate computation could simplify 
# program logic and speed up computations for numerically intensive tasks.


## Write a short comment describing this function
# The first function, `makecachematrix` returns a list object to provide access to four 
# functions: 
#  1) set the value of the matrix, 
#  2) get the value of the matrix, 
#  3) set the value of the inverse,
#  4) and get the value of the stored inverse.

makeCacheMatrix <- function(x = matrix()) {

    #Define storage for matrix inverse
    matrix.inv <- NULL

    # Define function to storage a new matrix in the storage allocated originally
    set <- function(y) {
      x <<- y

      # Reset the matrix inverse since we have a new matrix
      matrix.inv <<- NULL
    }

    # Define a function to retrieve the storage matrix
    get <- function() x
  
    # Define a function to store the value of the matrix inverse
    set.inverse <- function(new.matrix.inv) matrix.inv <<- new.matrix.inv
    
    # Define a function to retrieve the stored value of the matrix inverse (or NULL if not computed)
    get.inverse <- function() matrix.inv

    # Return a list with the four functions defined for this data
    list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}


## Write a short comment describing this function
# The second function, 'cacheSolve' calculates the matrix inverse if it has not been computed yet
# If the stored inverse is not NULL, it is assumed to be the current matrix inverse and it is returned
# The function uses the R 'solve' function to calculate the inverse
# It is assumed that the data provided is suitable for inversion.
# The 'x' value passed to this routine must be the list defined in the makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Retrieve the storage matrix inverse or NULL
    m <- x$get.inverse()

    if(!is.null(m)) {
      # A solution is available, return it.
      message("getting cached matrix inverse data")
      return(m)
    }

    # No inverse is available, so calculate it now
    
    # Obtain the stored matrix from the function memory
    data <- x$get()

    #Calculate the matrix inverse here
    m <- solve(data, ...)
      
    #Store the matrix inverse in the function cache memory
    x$set.inverse(m)

    # Return the matrix inverse
    m
}
