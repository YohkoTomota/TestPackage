
factorial_loop <- function(n) {
  if (!is.numeric(n)) {
    stop("n must be a number!")
  } 
  if (n%%1 != 0 | n<0) {
    stop("n must be a positive integer!")
  }
  
  result<-1
  if (n>0) {
    for (i in n:1) {
      result<-result*i
    }
  }
  return(result)
}

factorial_reduce <- function(n) {
  if (!is.numeric(n)) {
    stop("n must be a number!")
  } 
  if (n%%1 != 0 | n<0) {
    stop("n must be a positive integer!")
  }
  
  result<-1
  if (n==0) {
    return(result)
  } else {
    return(reduce(1:n, function(x,y) {x*y}))
  }
  
}

factorial_func <- function(n) {
  if (!is.numeric(n)) {
    stop("n must be a number!")
  } 
  if (n%%1 != 0 | n<0) {
    stop("n must be a positive integer!")
  }
  
  result<-1
  if (n==0) {
    return(result)
  } else {
    return(n*factorial_func(n-1))
  }
  
}

memo<-1
factorial_mem <- function(n) {
  if (!is.numeric(n)) {
    stop("n must be a number!")
  } 
  if (n%%1 != 0 | n<0) {
    stop("n must be a positive integer!")
  }
  
  if (!is.na(memo[n])) {
    return(memo[n]) 
  } else {
    result<-1
    if (n==0) {
      return(result)
    } else {
      result<-n*factorial_func(n-1)
      if (length(memo)<n) {
        length(memo)<-n
      }
      memo[n]<<-result
      return(result)
    }
  }
  
}