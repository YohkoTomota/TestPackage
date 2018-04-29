library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

# four versions of factorial function

factorial_loop <- function(x){
  if(x == 0) {
    1
  }else {
    fact = 1
    for(i in 1:x) {
      fact = fact * i
    }
    fact
  }
}


factorial_reduce <-  function(x){
  if(x == 0){
    return(1)
  }else{
  reduce(1:x, function(a,b) a*b)
  }
}


factorial_recursive <- function(x){
  if(x == 0)
    return(1)
  x * factorial_recursive(x-1)
}


factorial_tb1 <- c( rep(NA, 100))
factorial_memoization <- function(x){
  if(x==0){1}else
    if(!is.na(factorial_tb1[x])){
      factorial_tb1[x]
    } else {
      factorial_tb1[x] <<- x * factorial_memoization(x-1)
      factorial_tb1[x]
    }
  
}

# checking all versions produce same results

  input<- c(5, 7, 13, 19, 33)
  
  lapply(input, factorial)
  lapply(input, factorial_loop)
  lapply(input, factorial_memoization)
  lapply(input, factorial_recursive)
  lapply(input, factorial_reduce)
  
  sink("factorial_output.txt")

#timing function for specific inputs
cat("======================================================================\n")
cat("comparison of the four different implementations for individual inputs\n")
cat("======================================================================\n")

fact_tbl <- c(rep(NA, 100))

individual_results <- map(input, ~ microbenchmark(
  
  factorial_loop(.),
  
  factorial_reduce(.),
  
  factorial_recursive(.),
  
  factorial_memoization(.)
  
))


names(individual_results) <- as.character(input)

individual_results

#timing of each function for larger inputs
cat("======================================================================\n")
cat("comparison of the four different implementations for range of inputs\n")
cat("======================================================================\n")

get_benchmark <- function(x) {
  
  fact_tbl <<- c(rep(NA, 100))
  
  microbenchmark(lapply(x, factorial_loop),
                 
                 lapply(x, factorial_reduce),
                 
                 lapply(x, factorial_recursive),
                 
                 lapply(x, factorial_memoization))
  
}



ranges <- list(`range 1:10` = 1:10,
               
               `range 1:50` = 1:50,
               
               `range 1:100` = 1:100)



range_results <- lapply(ranges, get_benchmark)

range_results

sink()