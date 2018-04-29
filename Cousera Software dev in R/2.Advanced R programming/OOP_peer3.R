#Library calls
library(purrr)
library(microbenchmark)

### Create Factorial_loop function
Factorial_loop <- function(x){
  ans <- x #Assign variable for eventual answer, initially equal to x
  while(x>1){ #while loop. Limiting condition is x>1. At each iteration:
    x <- (x-1) #subtract 1 from x
    ans <- ans*x #multiply answer by new x value.
  }
  return(ans)
}

### Create Factorial_reduce function
Factorial_reduce <- function(x){
  funlist <- as.list(1:x) #Create a list object with elements 1:x
  reduce_right(funlist,function(x,y){x*y}) #Reduce_right on funlist, multiply each 
}

### Create Factorial_func function
Factorial_func <- function(x){
  stopifnot(x>0) #Condition function on "x is positive"
  if(x==1){ #Base case is identity function when x is 1.
    x
  } else {
    x * Factorial_func(x-1) #Recursive call to Factorial_func when x is above 1.
  }      
}

### Create Factorial_mem function
fac_tbl <- c(1,rep(NA,99)) #create table to store factorials of numbers up to 100

Factorial_mem <- function(x){
  stopifnot(x>0) #Condition function on "x is positive"
  if(!is.na(fac_tbl[x])){ #Check if fac_tbl already contains factorial value for x
    fac_tbl[x]
  } else { #If no value in fac_tbl, write it to fac_tbl and compute function
    fac_tbl[x] <<- x * Factorial_mem(x-1)
    x * Factorial_mem(x-1)
  }
}

### Use microbenchmark to profile the performance of the four functions
#Create a dataframe to store results of speed tests when function is applied to
#different input values, integers 1 through 100.
speed_tests <- data.frame(Trial=c(1:100),
                          Loop=rep(NA,100),
                          Reduce=rep(NA,100),
                          Recurse=rep(NA,100),
                          Memo=rep(NA,100))

#Populate column in speed_tests dataframe for Loop function
for(i in speed_tests$Trial){
  x <- mean(microbenchmark(Factorial_loop(i))[,2])
  speed_tests$Loop[i] <- x
  remove(x)
}

#Populate column in speed_tests dataframe for Reduce function
for(i in speed_tests$Trial){
  x <- mean(microbenchmark(Factorial_reduce(i))[,2])
  speed_tests$Reduce[i] <- x
  remove(x)
}

#Populate column in speed_tests dataframe for Recurse function
for(i in speed_tests$Trial){
  x <- mean(microbenchmark(Factorial_func(i))[,2])
  speed_tests$Recurse[i] <- x
  remove(x)
}

#Populate column in speed_tests dataframe for Memo function
for(i in speed_tests$Trial){
  x <- mean(microbenchmark(Factorial_mem(i))[,2])
  speed_tests$Memo[i] <- x
  remove(x)
}