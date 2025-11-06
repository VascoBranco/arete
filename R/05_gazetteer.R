#' Get geographic coordinates from localities
#' @description Extract geographic coordinates from strings containing location names, using an online index.
#' through usage of a gazetteer for Python.
#' @param locality vector of characters. A vector of location names to process with a gazetteer.
#' @details Gazetteers are geographical indexes, with the one used for \code{arete} being \href{https://nominatim.org/release-docs/develop/api/Overview/}{Nominatim} which is accessible through the \code{GeoPy} client.
#' @return matrix.
#' @seealso \code{\link{arete_setup}}
#' @examples
#' \dontrun{
#' example = c("Lisbon", "London")
#' 
#' gazzetteer(example)}
#' @export
gazetteer = function(locality){
  
  if (!all(reticulate::py_module_available("mechanize"),
           reticulate::py_module_available("bs4"),
           reticulate::py_module_available("lxml"))  ){
    reticulate::py_install(c("geopy", "folium", "spacy"))
  }
  loc_to_coords = NULL
  reticulate::source_python(system.file("python", "1_gazetteer.py", package = "arete"))
  
  out = matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("Latitude", "Longitude")))
  for (i in 1:length(locality)){
    data = loc_to_coords(locality[i])
    
    if (length(data$latitude) == 0){
      out = rbind(out, c(NA, NA))
    } else {
      out = rbind(out, c(data$latitude, data$longitude))
    }
    
    
  }
  return(out)
}
