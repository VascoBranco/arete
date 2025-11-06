#' Call a Large Language Model (LLM) to extract species geographic data
#' @description Send an API request to extract species data from a document. 
#' For now only \code{service = "GPT"} is supported but more are planned including
#' both proprietary and open source models. Uses the API...
#' @param path character. string of a file with species data in either pdf or txt format, e.g: \code{"./folder/file.pdf"}
#' @param tax character. Binomial name of the species to specify extraction to. Most often increases performance of the model. 
#' @param user_key list. Two elements, first element is a character with the user's API key, second element is a logical Bool determining whether the user's account has access to premium features.
#' Both free keys and premium keys are allowed.
#' @param service character. Model to be used. Right now, only requests using OpenAI's \code{chatGPT} are available.
#' @param model character. Model name from given service to be used. You may use any of the models listed on OpenAI's developer platform.
#' If you are unsure which model to use, we recommend picking \code{"gpt-3.5"} (default) or \code{"gpt-4o"}, as these will pick our recommended model from that version.
#' @param outpath Character string of a path to save output to in the format \code{"path/to/file/file_prefix"}.
#' @param outliers logical. Whether or not results should be processed using the methods described in \code{\link[gecko:outliers.detect]{gecko::outliers.detect()}}
#' @param verbose logical determining if output should be printed.
#' @return matrix. Containing the extracted information.
#' @seealso \code{\link{arete_setup}}
#' @examples
#' \dontrun{
#' file_path = arete_data("holzapfelae")
#' 
#' get_geodata(
#'   path = file_path,
#'   user_key = list(key = "your key here", premium = TRUE),
#'   model = "gpt-4o",
#'   outpath = "./out"
#' )}
#' @export
get_geodata = function(path, user_key, service = "GPT", model = "gpt-3.5",
                       tax = NULL, outpath = NULL, outliers = FALSE, verbose = TRUE){

  query = "places"
  species_name = tax
  
  # 1. Parameter Handling ---------------------------------------------------------

  if (!is(user_key, "list")){
    warning("user_key must be a list with a valid API key to a chatGPT account and whether it is premium.", immediate. = TRUE)
    return(NULL)
  } else {
    if (length(user_key) != 2){
      valid = FALSE
    } else {
      if (!is.character(user_key[[1]]) || length(user_key[[1]]) != 1){
        valid = FALSE
      }
      if (!is.logical(user_key[[2]]) || length(user_key[[2]]) != 1){
        valid = FALSE
      }
    }
    valid = aux_check_key_valid(user_key[[1]], "GPT")
    if (!valid){
      warning("API key is invalid or user_key has incorrect structure.")
      return(NULL)
    }
  }
  
  if (!is(model, "character")){
    warning("model must be a character string.", immediate. = TRUE)
    return(NULL)
  }
  
  if (!is(outpath, "character")){
    warning("outpath must be a character string.", immediate. = TRUE)
    return(NULL)
  }
  
  if (!is(verbose, "logical")){
    warning("Error: verbose must be a logical (Boolean) determining if output should be printed.", immediate. = TRUE)
    return(NULL)
  }
  
  # 2. Service is GPT -------------------------------------------------------------
  if (service == "GPT"){
    
    if (file.exists(system.file("python", "3_GPT.py", package = "arete"))){
      reticulate::source_python(system.file("python", "3_GPT.py", package = "arete"))
    } else {
      print("Python scripts not found!")
      return(NULL)
    }
    
    if (model == "gpt-3.5"){
      model = "gpt-3.5-turbo-1106"
    } else if (model == "gpt-4o"){
      model = "gpt-4o-mini-2024-07-18"
    }
    
    if (user_key$premium){
      is_premium = TRUE

      if (query == "places"){
        character_limit = 30000
      } else {
        character_limit = 41666
      }
    } else {
      is_premium = FALSE
      if (verbose){
        message("A free trial chatGPT account using a gpt-3.5-turbo model is capped at 20k
           TPM tokens per minute. At a rate of $0.0015 per 1k tokens a user with 
          just $5 in their account is capable of using gpt-3.5-turbo-instruct-0914,
          allowing for 12x the amount of tokens. This makes it possible to feed more of the PDF per request,
          giving it greater accuracy and speed.", immediate. = TRUE)
      }
      model = "gpt-3.5-turbo" # If free the best model is not available
      character_limit = 5000
    }
    
    doc_extension = substr(path, nchar(path)- 3, nchar(path))
    if (doc_extension == ".pdf" || doc_extension == ".txt"){
      text = process_document(path)
    } else {
      warning("Only .pdf or .txt files are currently supported.", immediate. = TRUE)
      return(NULL)
    }
    
    if (!check_lang(text)){
      warning("Input text is not mostly in English.", immediate. = TRUE)
      return(NULL)
    }
    
    ## Character limit handling? -----------------------------------------------
    # Originally 10k
    if (nchar(text) > character_limit) {
      out <- list()
      times <- c(1:floor(nchar(text) / character_limit)) * character_limit
      vect <- c(0, times, max(times) + nchar(text) %% character_limit)
      
      for (i in 1:(length(vect) - 1)) {
        out[i] <- substr(text, vect[i] + 1, vect[i + 1])
      }
      text <- out
    }
    
    ## Request --------------------------------------------------------------------
    request_text = tryCatch(
      {
        GPT_args = list(document = text, type = query, user_key = user_key,
          is_premium = is_premium, model = model, outpath = outpath
        )
        if (!is.null(species_name)){
          GPT_args["species_name"] = species_name
        }
        if (verbose){
          request_text = do.call(GPTrequest, args = GPT_args) 
        } else {
          request_text = suppressMessages(do.call(GPTrequest, args = GPT_args))
        }
      },
      error = function(e){
        message("Error: ChatGPT request has failed. This could be due to several reasons
              including mismatches in the chosen PDF file (if applicable) and failure
              to connect to the internet and/or ChatGPT.")
        request_text <- NULL
      }
    )

  } else {
    message("As of right now, GPT is the only LLM option. More services such as
            Bard, PaLM, Gemini, etc. will be included in future updates!")
    return(NULL)
  }
  
  # 3. Save results -------------------------------------------------------------
  
  if (!is.null(request_text)){
    if (verbose){
      print(request_text)
    }
    return(request_text)
  }
  
  
  if (outliers){
    gecko::outliers.detect()
  }

}
