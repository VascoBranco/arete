#' Setup arete
#' @description arete requires python to use most of its LLM utilities. OCR utilities will by default not be installed as they are not strictly necessary to the usage of the package. If interested please see install_OCR_packages().
#' We recommend that you install Python before running arete_setup. Usually performed through \code{sudo apt install python3-venv python3-pip python3-dev} on linux systems or \code{reticulate::install_python()} for other OS.
#' Some external software might also be needed to successfully install arete's dependencies, including:
#' \itemize{
#' \item GDAL, GEOS, PROJ, netcdf, sqlite3, tbb, see rspatial.github.io/terra
#' \item poppler (rpm: poppler-cpp-devel (Fedora, CentOS, RHEL), brew: poppler (MacOS) )
#' \item kableExtra (fontconfig, deb:libfontconfig1-dev (Debian, Ubuntu, etc), rpm: fontconfig-devel (Fedora, EPEL), csw: fontconfig_dev (Solaris), brew: freetype (OSX) )
#' \item cmake
#' }
#' With some being covered by r-base-dev: \itemize{
#' \item gfortran, libgmp3-dev, 
#' \item harfbuzz freetype2 fribidi (deb:libharfbuzz-dev libfribidi-dev (Debian, Ubuntu, etc), rpm: harfbuzz-devel fribidi-devel (Fedora, EPEL), brew: harfbuzz fribidi (OSX) )
#' } 
#' @param python character. Path to a python virtual environment in which to use arete.
#' @details A custom virtual environment path may be passed to \code{python} but we recommend 
#' leaving it as \code{NULL} and using one of the paths found by \code{reticulate}.
#' It is however useful if arete is already setup and you just wish to update its
#' dependencies.
#' @return character. Python path to the virtual environment created.
#' @examples 
#' \dontrun{
#' arete_setup()
#' }
#' @export
arete_setup <- function(python = NULL){
  if (is.null(python)){
    # venv not supplied
    python_starters = reticulate::virtualenv_starter()
    
    starters_missing = TRUE
    if (!is.null(python_starters)){
      if (length(python_starters) != 0){
        starters_missing = FALSE
      }
    }
    
    if (starters_missing){
      message("No python installation associated as interpreter default. Attempt to install Python through reticulate? Default values will be used.")
      selected = readline("(y/n)")
      
      if (tolower(selected) == "y"){
        
        # Check for git
        
        reticulate::install_python()
      } else if (tolower(selected) == "n"){
        message("Skipping python installation...")
      } else {
        # change to a repeat on future version
        message("Non-valid input. Ending setup.")
        return(NULL)
      }
    } else {
      chosen_python = select_python(python_starters)
    } 
    
    
    if (file.access(chosen_python, 1) != 0){
      message("R does not have permission to execute the given python.")
      return(NULL)
    }
    
    venv_error = tryCatch({
      python = reticulate::virtualenv_create("arete", version = chosen_python) # invisibly returns path
      venv_error = FALSE
    }, error = function(e){
      venv_error = TRUE
    })
    
    if (venv_error){
      message("Failure to create a virtual environment for arete. Make sure Python has been correctly installed and try again. You may need to install it manually. See the documentation for arete::arete_setup() and reticulate::install_python().")
      return(NULL)
    } else {
      message("arete virtual environment successfully created.")
    }
  } else {
    # venv supplied
    if (!is(python, "character")){
      message("Python path must be character string. Ending setup.")
      return(NULL)
    }
  }
  write(as.character(python), paste0(system.file(package = "arete"), "/path.txt"))
  
  # Initialize and install packages
  reticulate::use_virtualenv(python)
  
  if (available_python_packages()){
    message("Required python libraries already installed.")
  } else {
    message("Necessary python libraries needed. Installing.")
    install_python_packages(python)
  }
  message("Setup complete.")
  return(python)
}

select_python = function(envs){
  message("The following pre-existing python installations have been found:")
  print(envs)
  message("Please select one with the corresponding row number. Otherwise (0) to quit setup.")
  selected = readline("--> ")
  
  tryCatch({
    selected = envs[as.numeric(selected)]
  }, error = function(e){
    message("Non-valid input. Ending setup.")
    selected = NULL
  } )
  return(selected)
}

available_python_packages <- function(){
  packages <- c(
    "geopy", "folium", "spacy", "pandas", "numpy", "mechanize", "http.cookiejar",
    "bs4", "os", "lxml", "openai", "json", "tiktoken", "collections"
  )
  for (i in packages) {
    return(all(reticulate::py_module_available(i)))
  }
}
