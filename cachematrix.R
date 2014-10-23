##  My functions save you time and money by caching a matrix inverse,
##  yes you read correctly folks, a matrix inverse. Preventing
##  you, our dear and valued customer, from the costly and time
##  consuming hassle of calculating the matrix inverse each and every
##  time you might want it. This pre-cached option for your matrix
##  inverse can be used for pushing, pulling, printing, and returing.
##  You can be saving your precious time and hard earned money any
##  time you need that matrix inverse for spur-of-the-moment 
##  presentations requireing computations and calculations, or 
##  even last minute up to date population nullifications.
##
##  Just wait to see what we've got in store for you:
##  The timeless and venerable get rich quick penny pincher's delight
##  'makeCacheMatrix' and 'dietMakeCacheMatrix' functions have
##  been redesigned from the ground up, bringing you the latest and
##  greatest in matrix caching technology. But wait, that's not all!!
##  We have also added to the quality and value you recieve each time
##  you engage one of our classic functions, by bringing you their 
##  long sought after companion function, the nigh prefect like
##  'cacheSolve'. That's right folks, we are here today, offering a
##  once in a lifetime opportunity to get in on the ground floor of
##  this breath taking function that was dreamed up, and then turned
##  into a reality with the sole purpose of helping you solve all your
##  cache problems. No cache? We've not only got your solution, we are
##  your solution.
##  'makeCacheMatrix' and 'cacheSolve', your watch and your wallet
##  will thank you!




##  The 'makeCacheMatrix' function creates an all new top-secret 
##  super-special for your eyes only one-in-a-million 'x' marks the
##  spot lightening struck here more than once panaramic utopian
##  chivalrous yet at the same time feminist gum disease gingivitis
##  fighting cats and dogs living together ambrosiac chocolate covered
##  objet d'matrix' (or more formally objet de 'matrix') that can
##  cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {      ##  defines a function called 'makeCacheMatrix' that creates a 
                                                 ##  list of functions to get and set the value of the
                                                 ##  matrix, and get and set the inverse.
                                                 ##
                                                 ##  1 argument, 'x' where the default is an empty numeric
                                                    
    matrixinverse <- NULL       ##  sets matrixinverse to NULL value, which is not logical, and mostly returns NULL
    set <- function(y) {                ##  defines a new function inside of 'makeCacheMatrix' called 'set'
                                        ##  with 1 argument 'y'.      that
        x <<- y                         ##  sets x = y, but in a different enviornment than 'set'
        matrixinverse <<- NULL          ##  sets m = NULL, in a different enviornment than 'set'
    }
    get <- function() x         ##  defines a new function inside of 'makeCacheMatrix' called 'get'
                                ##  with 0 arguments that returns 'x'
    setinverse <- function(solve) matrixinverse <<- solve ##  defines a new function inside of 'makeCacheMatrix' called
                                                ##  'setinverse' that takes 1 argument, the function mean, 
                                                ##  that sets matrixinverse = mean, in a different env than 'setmean'
    getinverse <- function() matrixinverse      ##  defines a new function inside of 'makeVector' called 'getmean'
                                                ##  with 0 arguments that returns m
    list(set = set, get = get,           ##  returns a list of functions that do what they are called
         setinverse = setinverse,        ##  but specific to ______________________________
         getinverse = getinverse)        ##  'set', 'get', 'setinverse', 'getinverse'
}



##  The mysterious and alluring 'cacheSolve' function will dazzle
##  and delight from ages 1 to 92 by computing the inverse of the
##  special 'matrix' returned by 'makeCacheMatrix', unless said
##  matrix inverse is already in the cache, in which case it just
##  retrieves it from there. With fairytale awe and splendour. 

cacheSolve <- function(x, ...) {          ##  defines a function called 'cacheSolve' that checks to see if
                                          ##  the inverse from 'makeCacheMatrix' is cached, if yes: returns that 
                                          ##  matrix, if no: calculates the inverse and sets it to the cache
                                          ##
                                          ##  1+ arguments, 'x' and '...' from previous functions
  
    matrixinverse <- x$getinverse()           ##  gets the cached inverse of 'x' (may be NULL), sets = to 'matrixinverse' 
    if(!is.null(matrixinverse)) {             ##  if 'matrixinverse' (cached mean) has a value, it says so   
      message("getting cached data")          ##  and returns the value
      return(matrixinverse)
    }
    data <- x$get()                           ##  defines the variable 'data' and sets it equal to 'x' matrix
    matrixinverse <- solve(data, ...)         ##  sets 'matrixinverse' equal to the inverse of 'data', ie 'x' matrix
    x$setinverse(matrixinverse)               ##  sets 'matrixinverse' as the cached inverese of 'x' matrix
    matrixinverse                             ##  Return a matrix that is the inverse of 'x'
}
