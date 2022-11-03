#Creating function, which returns True if the variable is a string
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

#Function replacing cumsum built-in function
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

#Call my_cumsum function()
x <- my_cumsum(c(3,'d',2))
x
