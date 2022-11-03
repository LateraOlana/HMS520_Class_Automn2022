#Q1: Creating function, which returns True if the variable is a string
# False other wise.
is_string <- function(x){
  if (length(x) == 1){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}
# Call the function
x <- is_string(c("Star Mort Rickturn of the Jerri"))
x

#Q2: Function replacing cumsum built-in function
my_cumsum <- function(x) {
  if (length(x)==1) return(x)
  for (i in 2:length(x)) {
    if(!is.numeric(x[i])){
      stop("x must be a numeric vector")
    }
    else{
      x[i] <- sum(x[(i - 1):i])
    }
    
  }
  return (x)
}

#Call my_cumsum function
x <- my_cumsum(c(3,'d',2))
x

#Q3: Creating RMSE function
rmse <- function(x,na_rm){
  if ((is.numeric(x) != TRUE)){
    return("x must be a numeric vector")
  }
  else{
    if (na_rm == TRUE){
      x <- c(na.omit(x))
    }
    else{
      #I will replace it by zero as as no alternative is given in the assignment
      x[is.na(x)] <- 0
    }
    return(sqrt((sum(x^2))/length(x)))
  }
}
x<-rmse(c(1,2,3,'NA'),FALSE)
x


    