makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, counter = 0, ...) {
  m <- x$getmean()
  print(counter)
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  print(m)
}

test <- function(x) {
  cachemean(x, counter=1)
  cachemean(x, counter=2)
}