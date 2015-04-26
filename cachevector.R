
# the makeVector function takes vector as a input and returns a list
# that contains 4 function
# set - to set the value of the vector
# get - to get the value of the vector
# setmean - to set the mean of the vector
# getmean - to get the mean of the vector
#

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# This function takes a list ( as created by the cacheVector function )
# and computes it mean. If the mean has been computed previously,it 
# returns the previously computed value
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}