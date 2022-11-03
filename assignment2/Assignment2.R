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
is_string(c("Star Mort Rickturn of the Jerri"))

