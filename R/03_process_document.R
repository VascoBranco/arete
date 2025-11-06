#' Extract and process text from a document 
#' @description This function extracts text embedded in a \code{.pdf} or \code{.txt} file
#' and processes it so it can be safely used by LLM API's.
#' @param path character. Path leading to the desired PDF file.
#' @param extra_measures character. To be implemented. Some documents are 
#' especially difficult for LLM to process due to a variety of 
#' issues such as size and formatting. \code{extra_measures} tries to improve 
#' future performance by cropping the document given to only the central passage
#' mentioning a specific species. \code{"header"} and, by extension, \code{"both"} require an mmd file
#' that is the output of nougatOCR.
#' @return character. Fully processed text.
#' @examples
#' path = arete_data("holzapfelae")
#' process_document(path)
#' 
#' extra_measures = list("mention", "Tricholathys spiralis")
#' @export
process_document = function(path, extra_measures = NULL){ # to process_document
  file_ext = substr(path, nchar(path)-2, nchar(path)) 
  
  if (file_ext == "pdf"){
    # data = paste0(pdftools::pdf_text(path), collapse = "")
    data = c()
    open_pdf = pdftools::pdf_data(path)
    for (x in 1:length(open_pdf)){
      pdf_block = open_pdf[[x]]
      has_space = pdf_block[,5] == TRUE
      block_text = as.matrix(pdf_block[,6])
      block_text[has_space] = paste(block_text[has_space], " ")
      data = c(data, paste0(block_text, collapse = ""))
    }
    data = paste0(data, collapse = "")
    
  } else if (file_ext == "txt"){
    data = paste0(readLines(path), collapse = "")
  } else if (file_ext == "mmd"){
    data = readLines(path)
  } else {
    warning("Currently only pdf, txt and mmd files are supported.")
    return(NULL)
  }
  # Portable packages must use only ASCII characters in their R code
  # stringi::stri_escape_unicode("")
  
  # if (!is.null(extra_measures)){
  #   if (extra_measures[[1]] == "header" || extra_measures[[1]] == "both"){
  #     if (substr(path, nchar(path)-3, nchar(path)) != ".mmd"){
  #       warning("File supplied must be an mmd file created through nougat OCR.")
  #       return(NULL)
  #     }
  #   }
  # }

  clean_data = aux_clean_string(data, extra_measures)
  
  return(clean_data)
}
