#' Update python dependencies
#' @description Install python dependencies needed to run arete.
#' @param envname character. Name or path to the Python virtual environment being used for arete.
#' @param verbose logical. Determines if output should be printed.
#' @param ... Other parameters to pass to the installation function as per the documentation of \code{\link[reticulate]{py_install}}
#' @return NULL. If any packages failed to install, a character listing which.
#' @seealso \code{\link{arete_setup}}
#' @examples 
#' \dontrun{
#' install_python_packages("./path/to/venv")
#' }
#' @export
install_python_packages <- function(envname, verbose = TRUE, ...) {
  packages <- c(
    "geopy", "folium", "spacy", "pandas", "numpy", "mechanize", "cookiejar",
    "bs4", "lxml", "openai", "tiktoken"
  )
  failed = c()
  for (i in packages) {
    try_install = tryCatch({
      if (verbose){
        reticulate::py_install(i, envname = envname, method = "auto", ...)
      } else {
        suppressMessages(reticulate::py_install(i, envname = envname, method = "auto", ...))
      }
      try_install = FALSE
    }, error = function(e){
      try_install = TRUE
    })
    if (try_install){
      failed = c(failed, i)
    }
  }
  if (length(failed) != 0){
    if (verbose){
      message("Error: Python package installation failed for: ", paste(failed, collapse = ", "))
    }
    return(failed)
  } else {
    return(NULL)
  }
}
