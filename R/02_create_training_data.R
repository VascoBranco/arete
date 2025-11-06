#' Create training data for GPT
#' @description Open WebAnnoTSV files following RECODE structure and build training data for large language models in a variety of formats. 
#' @param input character or list. Either a set of paths to WebAnno TSV 3.3 files from which the text and annotated data are taken or a list with two terms,
#' 1) paths to \code{.txt} or \code{.pdf} files e.g: \code{"./folder/file.pdf"} from which text data will be taken from and 2) paths to WebAnno TSV 3.3 files from which to take annotation data.
#' @param prompt character. Custom prompt to be attached to each \code{text} during construction of the training data. Default prompt used otherwise.
#' @param service character. Service to be used. Right now, only \code{GPT} is available.
#' @param aggregate boolean. If TRUE and prompt is \code{"csv"}, a single csv is created.
#' @param export_type character. Either \code{"jsonl"} or \code{"csv"}. If \code{"jsonl"}, a single file is created in which each line is a json specifying the input (\code{prompt} and \code{text}) and expected output (\code{data}).
#' @param out_path character. Path to where the training data will be saved.
#' @return matrix / data.frame
#' @examples
#' example = system.file(paste0("extdata/insecta_annot_1.tsv"), package = "arete")
#' 
#' create_training_data(input = example, service = "GPT", export_type = "jsonl")
#' @export
create_training_data = function(input, prompt = NULL, service = "GPT", aggregate = TRUE, export_type = "jsonl", out_path = NULL){
  if (is.null(out_path)){out_path = paste0(tempdir(), "/train_data") }
  if (export_type == "jsonl"){
    write("", paste0(out_path, ".jsonl"))
  }
  
  input_length = 0
  
  if (is(input, "list")){
    # treat as separate text and annotations

    if (length(input[1]) != length(input[2])){
      message("error. Document files and data files must be a 1 to 1 match.")
      return(NULL)
    }  else {
      input_length = length(input[1])
    }
    
  } else {
    if (is(input, "character")){
      if (all(substr(input, nchar(input)-3,nchar(input)) == ".tsv")){
        input_length = length(input) 
      }
    } else {
      message("error. Input must be a list of paths to .txt and .tsv files or a single vector of paths to .tsv files.")
      return(NULL)
    }
  }
  
  
  
  if (export_type == "csv" && aggregate){
    aggregate_output = data.frame()
  }
  
  for (i in 1:input_length){
    # Prompt ------------------------------------------------------------------
    # If it's for training purposes the following, otherwise...
    
    # Corresponding data ------------------------------------------------------
    match_data = tryCatch(
      {
        
        if (export_type == "jsonl"){
          if (is(input, "character")){
            # text = input[1]
            data = input # data files are the entirety of the input
            open_file = webanno_open(data[[i]])
            document = aux_clean_string(paste0(open_file@text, collapse = " ")) # get the text from the tsv files
          } else {
            data = input[2]
            open_file = webanno_open(data[[i]])
            document = process_document(input[1][[i]], NULL) # character_limit = 30000
          }
          
          match_data = labels(data = open_file, label = "Species", relations = "OCCURS", show_tag = TRUE, show_ID = TRUE, show_type = TRUE)
          match_data = match_data[!is.na(match_data$term),]

          
          if (is.null(prompt)){
            document = paste0(GPT_prompt(), document)
          } else {
            document = paste0(prompt, document)
          }
          
          
        } else if (export_type == "csv"){
          # ... #
        }

        
        test_coords = labels(data = open_file, label = "Loc", relations = "GIVEN COORDS", show_ID = TRUE)
        
        new_terms = c()
        data_amount = c()
        for (r in 1:nrow(match_data)){
          
          pot_terms = test_coords$term[match_data[r,6] == test_coords[,1]]
          
          if (length(pot_terms) == 0){
            new_terms = c(new_terms, NA)
            data_amount = c(data_amount, r)
          } else if (length(pot_terms) > 1){
            new_terms = c(new_terms, pot_terms)
            data_amount = c(data_amount, rep(r, length(pot_terms))) 
          } else {
            new_terms = c(new_terms, pot_terms)
            data_amount = c(data_amount, r)
          }
          
          
        }
        match_data = match_data[data_amount,]
        match_data = cbind(match_data, new_terms = new_terms)
        
        # no_location = match_data[match_data$tag == "Coord",1:3] 
        
        no_location = c(1:nrow(match_data))[match_data[,4] == "Coord"]
        match_data = match_data[,c(2,3,7)]
        
        if (length(no_location) != 0){
          for (k in no_location){
            match_data[k, 2:3] = c(NA, match_data[k,2])
          }
        }

        match_data = match_data
      },
      error = function(e) {
        match_data = NULL
      }
    )
    
    if (!is(match_data, "data.frame")){
      warning(paste0("Could not convert file ", i, "."), immediate. = TRUE)
      next
    } else {
      if (all(is.na(match_data$term))){
        warning(paste0("Could not convert file ", i, "."), immediate. = TRUE)
        next
      }
    }
    
    
    
    colnames(match_data) = c("Species", "Location", "Coordinates")
    # Export -------------------------------------------------------------------
    if (export_type == "csv"){
      if (aggregate){
        end_pos = stringr::str_locate_all(data[i], "/")[[1]]   # to input[i] / input[[1]]
        file_name = substr(data[i], end_pos[nrow(end_pos),1] + 1, nchar(data[i]))
        
        aggregate_output = rbind(
          aggregate_output, 
          cbind(match_data, file = rep(file_name, nrow(match_data))) 
          )
      } else {
        message(paste0("Saving to", paste(out_path, substr(basename(data[i]), 1, nchar(basename(data[i])) - 4), ".csv", sep = "")))
        write.csv(match_data, paste(out_path, substr(basename(data[i]), 1, nchar(basename(data[i])) - 4), ".csv", sep = ""), row.names = FALSE)
      }
      
    } else if (export_type == "jsonl"){
      ## Create JSON -----------------------------------------------------------
      # write.csv(prompt_content, "test.csv")
      # read.csv(text = prompt_content, sep = "|")
      
      make_csv = paste("Species", "Location", "Coordinates", sep = " | ")
      for (j in 1:nrow(match_data)){
        make_csv = paste(
          make_csv, paste(match_data[j,], collapse = " | ")
          , sep = " \n "
        )
      }

      # read.csv(text = make_csv, sep = "|")
      
      out_JSON <- list(
        messages =
          as.data.frame(matrix(
            c(
              "system", "You extract novel species data exactly as it is written in a document.",
              "user", document,
              "assistant", make_csv # used to be jsonlite::toJSON(as.matrix(match_data))
            ),
            byrow = TRUE, ncol = 2, dimnames = list(c(), c("role", "content"))
          ))
      )
      ## Append JSON to JSONl --------------------------------------------------
      out_JSON = jsonlite::toJSON(out_JSON)
      out = readLines(paste0(out_path, ".jsonl"))
      
      if (length(out) == 1){
        if (out == ""){
          out = out_JSON
        } else {
          out = c(out, out_JSON)
        }
      } else {
        out = c(out, out_JSON)
      }
      message(paste0("Saving to ", out_path, ".jsonl"))
      write(out, paste0(out_path, ".jsonl"))
    } else {
      warning("Supported export data types are 'json' or 'csv'.")
      return(NULL)
    }
    
    
    

  }
  # write the file if output is csv and collected
  if (export_type == "csv" && aggregate){
    message(paste0("Saving to ", out_path, "collected_data.csv"))
    write.csv(aggregate_output, paste0(out_path, "collected_data.csv"), row.names = FALSE)
  }
  
}
