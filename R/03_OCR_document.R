#' Scan PDF with optical character recognition (OCR)
#' @description Extract text contained under image form in a PDF through the use
#' of optical character recognition software (OCR). Currently two options are
#' available, \code{method = "nougat"} and \code{method = "tesseract"}.
#' @param in_path character. string of a file with species data in either pdf or txt format, e.g: ./folder/file.pdf
#' @param out_path character. Binomial name of the species used with applicable \code{type}.
#' @param method character. Method used for the OCR. Currently it defaults to the only available method, nougatOCR. 
#' @param verbose logical. Print output after finish.
#' @details For now OCR processing of documents is only supported on linux
#' systems.
#' @return character. Containing the extracted information.
#' @seealso \code{\link{arete_setup}}
#' @examples
#' \dontrun{
#' OCR_document("path/to/file.pdf", "path/to/dir")
#' }
OCR_document = function(in_path, out_path, method = "nougat", verbose = TRUE){
  # In order to to call nougat with bash, the Python environment where it is installed must be called.
  if (!is(in_path, "character")){
    warning("in_path must be a character string.", immediate. = TRUE)
    return(NULL)
  }
  
  if (!is(out_path, "character")){
    warning("out_path must be a character string.", immediate. = TRUE)
    return(NULL)
  }
  
  
  # Force the usage of full paths.
  if (substr(in_path, 1, 2) == "./"){
    in_path = paste0(getwd(), substr(in_path, 2, nchar(in_path)))
  }
  
  if (substr(out_path, 1, 2) == "./"){
    out_path = paste0(getwd(), substr(out_path, 2, nchar(out_path)))
  }
  
  bsnm = basename(in_path)
  
  if (method == "nougat"){
    if (substr(bsnm, nchar(bsnm)-3, nchar(bsnm)) != ".pdf"){
      warning("in_path must point to a valid pdf document.", immediate. = TRUE)
      return(NULL)
    } else {
      system2(command = "nougat",
              args = c(
                in_path,
                "-o",
                out_path,
                # "--no-markdown",
                "--recompute"
                #,
                #"--no-skipping"
              )
              
              
      )
      
      system2(command = "pandoc",
              args = c(
                paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".mmd"),
                "-f",
                "markdown",
                "-t",
                "plain",
                "-o",
                paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".txt")
              )
      )
      
      # file.remove(paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".mmd"))
    }
  } else if (method == "tesseract"){
    
    png_folder = paste0(tempdir(), "/arete/tesseract_png")
    txt_folder = paste0(tempdir(), "/arete/tesseract_txt")
    aux_make_directory(txt_folder)
    aux_make_directory(png_folder)
    
    system2(command = "pdftoppm",
            args = c(in_path, paste0(png_folder, "/ocr"), "-png", "-rx 300", "-ry 300")
    )
    
    png_files =  stringr::str_sort(dir(png_folder, full.names = TRUE), numeric = TRUE)
    for (i in png_files){
      ocr_test = system2(command = "tesseract",
              args = c(i, paste0(txt_folder, "/", basename(i)), "--psm 1")
      )
    }
    
    full_text = c()
    txt_files =  stringr::str_sort(dir(txt_folder, full.names = TRUE), numeric = TRUE)
    for (i in txt_files){
      full_text = c(full_text, readLines(i))
    }
    write(full_text, paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".txt"))
    message(paste0("Output written @ ", paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".txt")))
    
    file.remove(png_folder)
    file.remove(txt_folder)
  } else {
    warning("method must be either 'nougat' or 'tesseract'.", immediate. = TRUE)
    return(NULL)
  }
  
  if (verbose){
    return(readLines(paste0(out_path, "/", substr(bsnm, 1, nchar(bsnm)-4), ".txt")))
  } else {
    return(NULL)
  }
  
  
}