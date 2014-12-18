## THis script has two functions. 
## makeCacheMatrix: This function creates a special "matrix" object 
##                  that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has 
##             already been calculated (and the matrix has not changed),
##             then cacheSolve should retrieve the inverse from the cache.


#makeCacheMatrix function has getters and setters functions.
#makeCacheMatrix creates a special matrix and caches its inverse
#The get function gets the value of original matrix
#the getInverse function gets the value of cached matrix
#the set function sets the value of matrix and intializes
#                 the inverse matrix to null
#the setInverse function sets the value of inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  
  #initialize inverse matrix to null
  im <- NULL      
  
  #the set function sets the value of new matrix 
  set <- function(y) {
    x <<- y       # sets the value of the matrix
    im <<- NULL   #initialize inverse matrix to null
  }
  
  #the get function gets the value of original vector 
  get <- function()     x
  
  
  #sets the inverse matrix 
  setinverse <- function(solve) im <<- solve
  
  #gets the inverse matrix
  getinverse <- function() im
  
  
  #this gets executed no matter
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve calculates inverse of the matrix only if it is not 
## calculated in the past.
## the function first checks the cache for computed inverse matrix
## using getinverse. If it does not exists in cache, it calculates
## the inverse matrix and then saves it in cache using setinverse
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Get the cached inverse matrix if it exists
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  
  #if the inverse matrix is not cache (first time execution)
  data <- x$get()
  im <- solve(data, ...) #calculate the inverse of matrix
  x$setinverse(im) # cache the computed value using set function
  im   #return inverse matrix
}