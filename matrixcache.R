##this function caches the inverse of the matrix
makeCacheMatrix <- function( m =matrix() ) {
  i <- NULL ##initialization
  set <- function( matrix ) { ##setting the matrix
    m <<- matrix
    i <<- NULL
  }
  get <- function() { ##getting the matrix
    m
  }
  setInverse <- function(inverse) { ##setting the inverse of matrix
    i <<- inverse
  }
  getInverse <- function() { ##getting the inverse of matrix
    i
  }
  list(set = set, get = get, ##method listed
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special matrix returned by 
##makeCacheMatrix above. If the inverse has already been calculated and the
##matrix has not changed, it retrieves the inverse from the cache.
cacheSolve <- function(x,...) {
  m <- x$getInverse() ##getting the cached inverse
  if( !is.null(m) ) { ##checking if the inverse is calculated
       message("getting cached data")
       return(m) ##returning the inverse
  }
  data <- x$get()
  m <- solve(data) %*% data ##calculating the inverse
  x$setInverse(m)
  m ##returning the inverse of the matrix
}
