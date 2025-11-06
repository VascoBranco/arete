#' Check if text is language-appropriate
#' @description Many, if not all, large language models are biased to English 
#' terms and sentence constructions. This function performs a quick check with
#' \code{\link[cld2:detect_language]{cld2}} over every element of a string of
#' characters and returns whether it is mostly (75% of the document) in English.   
#' @param strings character. Vector of strings containing document sentences.
#' @param detailed bool. If TRUE, the full cld2 report is returned as well. 
#' @return logical. If \code{TRUE} the language of the string is mostly English. 
#' If \code{detailed} is \code{TRUE} a list is instead returned for the full document.
#' @examples
#' # English 
#' check_lang("Species Macrothele calpeiana is found in Alentejo.")
#' 
#' # Portuguese
#' check_lang("A espécie Macrothele calpeiana é encontrada no Alentejo.")
#' @export
check_lang = function(strings, detailed = FALSE){
  if (is(strings, "character")){
    if (detailed){
      strings = paste0(strings, collapse = " ")
      return(cld2::detect_language(strings))
    } else {
      lang_eval = table(cld2::detect_language(strings))
      if (any(names(lang_eval) == "en")){
        if ((lang_eval["en"] / sum(lang_eval)) > 0.75){
          lang_flag = TRUE
        } else {
          lang_flag = FALSE
        }
      } else {
        lang_flag = FALSE
      }
      return(lang_flag)
    }
  } else {
    message("'strings' must be a vector of characters.")
    return(NULL)
  }
}