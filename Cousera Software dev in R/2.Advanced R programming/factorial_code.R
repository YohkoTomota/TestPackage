#Factorial_loop:
#a version that computes the factorial of an integer using looping (such as a for loop)

Factorial_loop <- function(x){
  if (x < 0){
    stop("x should be an integer greater than or equal to 0")
  }
  if (x==0){
    return(1)  
  }else{
    y <- 0
    for (i in 1:x){
      y <- y + i
    }
    return(y)
  }
}

#Factorial_reduce:
#a version that computes the factorial using the reduce() function in the purrr package. 
#Alternatively, you can use the Reduce() function in the base package.

library(purrr)
Factorial_reduce <- function(x){
  if (x < 0){
    stop("x should be an integer greater than or equal to 0")
  }
  if (x==0){
    return(1)  
  }else{
    x %>% reduce(`+`) %>% return()
  }
}

#Factorial_func: 
#a version that uses recursion to compute the factorial.

Factorial_func <- function(x){
  if (x < 0){
    stop("x should be an integer greater than or equal to 0")
  }
  if (x==0){
    return(1)  
  }else{
    return(x*Factorial_func(x-1))
  }
}

#Factorial_mem:
#a version that uses memoization to compute the factorial.
mem_tbl <- c(1,2,rep(NA,5))

Factorial_mem <- function(x){
  if (x < 0){
    stop("x should be an integer greater than or equal to 0")
  }
  if (x==0){
    return(1)  
  }else{
    if (!is.na(mem_tbl[x])){
      mem_tbl[x]
    } else{
    mem_tbl[x - 1] <<- Factorial_mem(x - 1)
    mem_tbl[x - 2] <<- Factorial_mem(x - 2)
    mem_tbl[x - 1] *mem_tbl[x - 2] 
    }
  }
}

#time the operation of these functions 


library(microbenchmark)
microbenchmark(
  Factorial_func(10),
  Factorial_reduce(10),
  Factorial_loop(10),
  Factorial_mem(10)
)
microbenchmark(
  Factorial_func(100),
  Factorial_reduce(100),
  Factorial_loop(100),
  Factorial_mem(100)
)
