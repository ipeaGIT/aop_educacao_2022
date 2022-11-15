
# returns the max extent (horizontal or vertical) of SF layer
get_layer_buffer_size <- function(layer_sf) {
  
  # get bounding box
  bbox <- st_bbox(layer_sf)
  
  # calculate sizes
  len_x <- bbox['xmax'] - bbox['xmin']
  len_y <- bbox['ymax'] - bbox['ymin']
  
  len_max <- max(c(len_x, len_y))
  
  len_km_approx <- len_max * 111
  
  # get buffer radius as a multiple of 8, for compatibility with individual city maps
  buff <- ceiling(len_km_approx / 8) * 4
  
  return(max(c(buff, 16)))
}