#' Check EOO differences between two sets of coordinates
#' @description Calculate EOO for two sets of coordinates for a practical assessment of data proximity.
#' @param x data.frame. With two columns containing latitude and longitude. Considered during reporting as data from a LLM.
#' @param y data.frame. With the same formatting as \code{x}. Considered during reporting as the ground truth.
#' @param plots logical. Determines if plots should be printed.
#' @param verbose logical. Determines if output should be printed.
#' @details
#' Extent of occurrence (EOO) is defined as "the area contained within the shortest continuous imaginary boundary which can be drawn to encompass all the known, inferred or projected sites of present occurrence of a taxon, excluding cases of vagrancy"
#' @return list with two values, the percentage of \code{y} that is part of the intersection with \code{x} and \code{y} and the percentage of \code{x} that is part of the intersection with \code{x} and \code{y}
#' @examples
#' set_a = matrix(
#'   c(54.30379, -25.48098, 54.21251,	-25.47146, 59.53277, -20.37448, 55.59712,
#'  -22.39599, 55.47244, -26.30330, 61.39205,	-21.12364, 56.24010, -24.40347),
#'  ncol = 2, byrow = TRUE 
#' )
#' 
#' set_b = matrix(
#' c(54.30379,	-25.48098, 111.42100,	-19.00400, 54.21251, -25.47146, 59.53277,
#'   -20.37448, 55.59125, -22.39599, 55.47244,	-26.30330, 61.39205, -21.12364,
#'    56.24010,	-24.40347),
#'    ncol = 2, byrow = TRUE
#'  )
#' 
#' compare_IUCN(set_a, set_b)
#' @export
compare_IUCN = function(x, y, plots = TRUE, verbose = TRUE){
  # Verify effect of order of points in the calculations
  
  if (typeof(x) == "list"){
    x = matrix(unlist(x), ncol = 2)
  }
  
  if (typeof(y) == "list"){
    y = matrix(unlist(y), ncol = 2)
  }
  
  # EOO and polygon drawing ----------------------------------------------------
  if (plots){
    terra::plot(draw_geom(x, "EOO"))
    terra::plot(draw_geom(y, "EOO"))
  }

  inter = tryCatch(
    {
      inter = terra::intersect(draw_geom(x, "EOO"), draw_geom(y, "EOO"))
      if (plots){
        terra::plot(inter)
      }
    }
    ,
    error = function(e){
      return("Error")
    }
  )
  
  if (length(inter) == 1){
    if (inter == "Error"){
      return(NULL)
    }
  } else {
    inter = terra::intersect(draw_geom(x, "EOO"), draw_geom(y, "EOO"))
  }

  # Get ratio
  suppressWarnings({
    ratio = terra::expanse(inter) / terra::expanse(draw_geom(y, "EOO")) * 100
    ratio_outside =  terra::expanse(inter) / terra::expanse(draw_geom(x, "EOO")) * 100
  })
  
  out_l = list("EOO_sdata_to_GPT_ratio" = round(ratio, 3), 
               "EOO_GPT_to_intersection_ratio" = round(ratio_outside, 3))
  
  if (verbose){
    cat(paste0(out_l[[1]], "% of human data is represented in model data.\n"))
    cat(paste0("The intersection represents ", out_l[[2]], "% of the model data.\n"))
    cat("\n")
  }
  
  return(out_l)
}
