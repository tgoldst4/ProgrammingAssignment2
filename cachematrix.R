#utilized examples from previous students to solve this one.
#it would be nice if what we went over in lecture had any correlation to homework ;)
#sources:
#http://stackoverflow.com/questions/23796316/returning-the-inverse-matrix-from-a-cached-object-in-r
#https://github.com/sefakilic/coursera-rprog-assignment2/blob/master/cachematrix.R

makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL
  set <- function(y) 
    {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


cacheSolve <- function(x, ...) 
  {
  inv <- x$getinverse()
  if(!is.null(inv)) 
    {
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  }

#test
#m = m = makeCacheMatrix(rbind(c(0, -5), c(-5, 0)))
#m$get()
#cacheSolve(m)