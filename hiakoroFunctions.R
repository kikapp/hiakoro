library(maptools)
library(mapproj)
library(ggplot2)



centerState <- function(.df) {
  #.df is a data frame with variables x and y
  #returns a data frame that has been centered where
  #center is the midpoint of the min and max x and y values
  .df$x <- .df$x - (diff(range(.df$x, na.rm = T))/2 + min(.df$x, na.rm = T))
  .df$y <- .df$y - (diff(range(.df$y, na.rm = T))/2 + min(.df$y, na.rm = T))
  return(.df)
}

scaleState <- function(.df, scale_matrix, scale_factor, x_shift, y_shift) {
  #.df is a data frame with numeric variables x and y
  #scale_matrix determines how states are scaled, it should be 2x2
  #scale_factor allows for tweaking of the extent of scaling, it should be a number
  #x_shift and y_shift determin where final shape ends up, units are consistend with 
  #.df units
  #returns a data frame with coordinates that have been centered, scaled, and shifted from
  #the origin
  
  .df <- centerState(.df)
  coords <- t(cbind(.df$x, .df$y))
  scaled_coord <- t(scale_factor*scale_matrix %*% coords)
  
  .df$x <- scaled_coord[,1] + x_shift
  .df$y <- scaled_coord[,2] + y_shift
  return(.df)
}
