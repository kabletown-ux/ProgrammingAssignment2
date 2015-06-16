## My attempts to build cache for a calculated matrix inverse.
## makeCacheFoo is first attempt at not having to pass around a list of functions.

debug <- TRUE
fooMatrix <- NULL

makeCacheFoo <- function() {
    
    #if ( debug ) cat( "fooMatrix [", fooMatrix, "]\n" )
    if ( is.null( fooMatrix ) ) {
        if ( debug ) cat( "Instantiating new fooMatrix... " )
        fooMatrix <<- matrix( 1:9, 3, 3 )
        if ( debug ) cat( "Done! dim [", dim( fooMatrix ), "]\n" )
    } else {
        if ( debug ) print( "fooMatrix already exists, returning from cache..." )
    }
}
#cacheFooSolve <- function( x = matrix() ) {

    ## Return a matrix that is the inverse of 'x'   
#}


## creates a list of 
makeCacheMatrix <- function( x = matrix() ) {
    
    ## param x: a square invertible matrix
    ## return: a list containing functions that
    ## 1) set the matrix
    ## 2) get the matrix
    ## 3) set the inverse
    ## 4) get the inverse
    
    inv = NULL
    set = function( y ) {
        
        # used `<<-` to assign a value to an object in an environment different from the current environment. 
        # TODO: different how?  What's that 'different' env called? Where's it located?
        x <<- y
        inv <<- NULL
    }
    get = function() {
        x
    }
    setinv = function( inverse ) {
        inv <<- inverse 
    }
    getinv = function() {
        inv
    }
    # what this function returns.
    # TODO why is this approach preferable to OOP use of global value for inv, as attempted in makeCacheFoo above?
    list( set=set, get=get, setinv=setinv, getinv=getinv )
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function( x, ... ) {
    
    ## param x: the list of functions created by makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # check cache for inv
    if ( !is.null( inv ) ) {
        
        # get it from the cache and skips the computation. 
        if ( debug ) cat( "Using cached matrix...\n" )
        return( inv )
        
    } else {
        
        # calculate & set in cache
        if ( debug ) cat( "Calculating and setting matrix in cache... " )
        
        mat.data = x$get()
        inv = solve( mat.data, ... )
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv( inv )
        
        if ( debug ) cat( "Done!\n" )
        
        return( inv )
    }
}