> makeCacheMatrix <- function(x = matrix()) {
+   a <- NULL
+   set <- function(y) {
+     x <<- y
+     a <<- NULL
+   }
+   get <- function() x
+   setinverse <- function(inverse) a <<- inverse
+   getinverse <- function() a
+   list(set = set,
+        get = get,
+        setinverse = setinverse,
+        getinverse = getinverse)
+ }
> cacheSolve <- function(x, ...) {
+   a <- x$getinverse()
+   if (!is.null(a)) {
+     message("getting cached data")
+     return(a)
+   }
+   data <- x$get()
+   a <- solve(data, ...)
+   x$setinverse(a)
+   a
+ }
> P <- matrix(c(1,2,-1,-2,0,1,1,-1,0),3,3)
> P1 <- makeCacheMatrix(B)
> cacheSolve(P1) 
     [,1] [,2] [,3]
[1,]    1    1    2
[2,]    1    1    3
[3,]    2    1    4
> 
