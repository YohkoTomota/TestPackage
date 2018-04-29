#load packages
library(dplyr)
library(tidyr)
#Functions

#---Difine function that converts a data frame into a “LongitudinalData” object
make_LD <- function(df){
  structure(df, class = "LongitudinalData")
}

#---Difine generic functions-------------
subject <- function(x,i) UseMethod("subject")
visit <- function(x,i) UseMethod("visit")
room <- function(x,i) UseMethod("room")


#---methods for LongitudinaData class----------
#subject and print

print.LongitudinalData <- function(x,...){
  if(length(unique(x$id)) == 1){
    cat("Subject ID: ", unique(x$id), "\n") 
  } else if(length(unique(x$id)) == 0) {
    cat("NULL")
  } else {
    cat("Longitudinal dataset with", length(unique(x$id)), "subjects")
  }
  
  if(length(unique(x$visit)) == 1){
    cat("Visit: ", unique(x$visit), "\n")
  }
  
  if(length(unique(x$room)) == 1){
    cat("Room: ", unique(x$room))
  }
  
  invisible(x)
  
}

subject.LongitudinalData <- function(x,i){
  index <- which(x$id %in% i)
  x <- lapply(x, function(x) x[index])
  structure(x, class = "LongitudinalData")
}

room.LongitudinalData <- function(x,j){
  index <- which(x$room == j)
  x <- lapply(x, function(x) x[index])
  structure(x, class = "LongitudinalData")
}

visit.LongitudinalData <- function(x,k){
  index <- which(x$visit %in% k)
  x <- lapply(x,function(x) x[index])
  structure(x, class = "LongitudinalData")
}


summary.LongitudinalData <- function(object, ...) {
  output <- data.frame(
    visit = object$visit,
    room = object$room,
    value = object$value
  )
  
  if(length(unique(output$visit)) == 1 &
     length(unique(output$room)) == 1){
    
    output <- summary(output$value)
    
  } else {
    
    output <- output %>% 
      aggregate(value ~ visit + room, FUN = mean, data = .) %>%
      spread(room, value)
    
  }
  
  structure(list(id = unique(object$id),
                 output = output),
            class = "Summary")
  
}

#---Method for Summary class------------
print.Summary <- function(x) {
  cat("ID:", x[[1]], "\n")
  print(x[[2]])
  invisible(x)
}