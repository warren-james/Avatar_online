# functions for the online expeiment
# check distance from target/
ellipse_dist <- function(x, y, a, b, w, h){
  # x and y refer to the point to check
  # a and b refer to the origin of the shape
  # h and w refer to the reach (i.e., how close you have to get)
  # output <= 1 means inside the reach
  # maybe we should scale the output?
  output <- (x-a)^2/w^2 + (y-b)^2/h^2
  
  return(output)
}

# convert string to numbers 
str_to_num <- function(string) {
  out <- as.numeric(unlist(str_split(string, "[\\[,\\]]")))
  out <- out[complete.cases(out)]
  return(out)
}

str_to_list <- function(string) {
  out <- unlist(str_split(string, "[\\[' ,\\]]"))
  out <- stringi::stri_remove_empty(out, T)
  # out <- as.list(out)
  return(out)
}

# get ellipse coords
# find x and y coords
ellipse_getx <- function(x_reach, y_reach, y_pos){
  x <- x_reach*sqrt(1-(y_pos/y_reach)^2)
  return(x)
}

ellipse_gety <- function(x_reach, y_reach, x_pos){
  y <- y_reach*sqrt(1-(x_pos/x_reach)^2)
  return(y)
}
