############################################################################
################################## PACKAGES - fn_spatial ###################
############################################################################
#' @importFrom terra plot expanse intersect
#' @importFrom cld2 detect_language
#' @importFrom stringr str_detect str_replace_all str_trim str_sub str_detect
#' @importFrom grDevices chull dev.copy dev.off pdf
#' @importFrom reticulate py_module_available py_install source_python import
#' @importFrom pdftools pdf_text
#' @importFrom fedmatch clean_strings
#' @importFrom gecko outliers.detect
#' @importFrom methods is
#' @importFrom utils txtProgressBar setTxtProgressBar read.csv write.csv adist capture.output
#' @importFrom stats quantile
#' @importFrom kableExtra kable_styling add_header_above kable_paper kable_classic_2 collapse_rows
#' @importFrom jsonlite toJSON
#' 
NULL
#> NULL

############################################################################
################################## PACKAGES - fn_spatial ###################
############################################################################
#' @importFrom ggplot2 aes element_line element_text geom_histogram ggplot labs scale_x_continuous theme theme_minimal
NULL
#> NULL


######################################################################
################################## PACKAGES - qat ####################
######################################################################
#' @importFrom googledrive drive_auth drive_create drive_update drive_ls drive_download as_dribble
#' @importFrom stringr str_locate str_locate_all str_split
#' @importFrom methods is new
#' @importFrom utils read.csv write.csv
NULL
#> NULL 

# Removed:
# ‘biomod2’ ‘kernlab’ ‘predicts’


# Notes
# Assign python sourced functions with something like gazetteer = NULL to avoid
# getting notes while checking/building.

.onAttach <- function(libname, pkgname) {
  path_to_python = system.file("path.txt", package = "arete")
  if (path_to_python == ""){
    packageStartupMessage("Can't find a default virtual environment for arete. If this is your first time loading the package, please run arete_setup().")
  } else {
    packageStartupMessage(paste0("Initalizing python virtual environment: ", readLines(path_to_python)) )

    py_start = tryCatch({
      reticulate::use_virtualenv(readLines(path_to_python))
    }, error = function(e){
      packageStartupMessage("Failed to initalize")
    })
  }
}

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("geopy")
  reticulate::py_require("folium")
  reticulate::py_require("spacy")
  reticulate::py_require("pandas")
  reticulate::py_require("numpy")
  reticulate::py_require("mechanize")
  reticulate::py_require("cookiejar")
  reticulate::py_require("bs4")
  reticulate::py_require("os")
  reticulate::py_require("lxml")
  reticulate::py_require("openai")
  reticulate::py_require("json")
  reticulate::py_require("tiktoken")
  reticulate::py_require("collections")
}
