#' Convert strings to numerical coordinates
#' @description Convert character strings containing geographic coordinates to sets of numeric values through a rule-based method. Useful as most LLM experience better and more consistent results when asked to return a single string instead of two separate values.
#' @param coord_vector character. A string containing potential geographic coordinates.
#' @param verbose logical. Whether or not to print the overall success of the function upon end.   
#' @details Will convert all strings to N, W for consistency's sake.
#' In a future update the user will be able to specify the outcome format, e.g: "39°S 42°E".
#' @return list. Contain latitude and longitude as the first and second elements,
#' respectively. 
#' @examples
#'  example = c('N 39degrees05’53” E 26degrees21’57”', "39 ° 80 ' N , 32 ° 78 ' E",
#'  "30 ° 20'54.32 \" N , 78 ° 0'53.37 \" E", "19 ° 34 ’ S 29 ° 10 ° E",
#'  "- 25.05012 S ; - 65.49622 W", "S 12 ° 06 ’ , W 76 ° 57 ’", 
#'  "19 ° 34 ’ S 29 ° 10 ° E", "19 ° 32 ’ S 29 ° 08 ° E")
#'  string_to_coords(example)
#' @export
string_to_coords = function(coord_vector, verbose = TRUE){
  out_frame = data.frame(Lat = c(), Long = c())
  success_coords = 0
  for (k in 1:length(coord_vector)){
    # cat(k, "\n")
    try_coords = tryCatch(
      {
        try_coords = suppressWarnings(aux_string_to_coords(coord_vector[k]))
      }
      ,
      error = function(e){
        return("error")
      }
    )
    
    if (!is.null(try_coords)){
      if (length(try_coords) != 2){
        out_frame = rbind(out_frame, data.frame(Lat = NA, Long = NA))
      } else {
        # cat("Succeeded!\n")
        success_coords = success_coords + 1 
        out_frame = rbind(out_frame, data.frame(Lat = try_coords[[1]], Long = try_coords[[2]]))
      }
    } else {
      out_frame = rbind(out_frame, data.frame(Lat = NA, Long = NA))
    }
    
  }
  
  if (verbose){
    cat(paste0(success_coords, " out of ", sum(!is.na(coord_vector)), " (", round(success_coords/sum(!is.na(coord_vector))*100), "%) succeded.\n", sep = "")) 
  }
  return(out_frame)
}
