# Model metrics ----------------------------------------------------------------
#' Model accuracy
#' @description Calculate accuracy for a binary confusion matrix.
#' @param confusion A binary confusion matrix organized (by row) as TP, FP, FN, TN.
#' @details \url{https://en.wikipedia.org/wiki/F-score}
#' @return A numeric.
#' @examples 
#' example = matrix(c(10, 1, 2, 5), ncol = 2, byrow = TRUE)
#' aux_accuracy(example)
#' @noRd
aux_accuracy = function(confusion){
  if (is.na(confusion[2,2])){
    # TP/(TP + FP + FN)
    out = confusion[1,1] / (confusion[1,1] + confusion[1,2] + confusion[2,1])
  } else {
    # (TP + TN)/(TP + TN + FP + FN)
    out = (confusion[1,1] + confusion[2,2]) / (confusion[1,1] + confusion[1,2] + confusion[2,1] + confusion[2,2])
  }
  return(out)
}

#' Model recall
#' @description Calculate recall for a binary confusion matrix.
#' @param confusion A binary confusion matrix organized (by row) as TP, FP, FN, TN.
#' @details \url{https://en.wikipedia.org/wiki/F-score}
#' @return A numeric.
#' @examples 
#' example = matrix(c(10, 1, 2, 5), ncol = 2, byrow = TRUE)
#' aux_recall(example)
#' @noRd
aux_recall = function(confusion){
  # TP/(TP + FN)
  out = confusion[1,1]/(confusion[1,1] + confusion[2,1])
  return(out)
}

#' Model precision
#' @description Calculate precision for a binary confusion matrix.
#' @param confusion A binary confusion matrix organized (by row) as TP, FP, FN, TN.
#' @details \url{https://en.wikipedia.org/wiki/F-score}
#' @return A numeric.
#' @examples
#' example = matrix(c(10, 1, 2, 5), ncol = 2, byrow = TRUE)
#' aux_precision(example)
#' @noRd
aux_precision = function(confusion){
  # TP/(TP + FP)
  out = confusion[1,1]/(confusion[1,1] + confusion[1,2])
  return(out)
}

#' Model F1 score
#' @description Calculate the F1 score for a binary confusion matrix.
#' @param confusion A binary confusion matrix organized (by row) as TP, FP, FN, TN.
#' @details \url{https://en.wikipedia.org/wiki/F-score}
#' @return A numeric.
#' @examples
#' example = matrix(c(10, 1, 2, 5), ncol = 2, byrow = TRUE)
#' aux_f1(example)
#' @noRd
aux_f1 = function(confusion){
  # 2/((1/Precision) + (1/Recall))
  prec = aux_precision(confusion)
  recall = aux_recall(confusion)
  if ( (prec == 0 || is.nan(prec) ) && (recall == 0 ||   is.nan(recall) )  ){
    out = 0
  } else if (prec == 0){
    out = 2/((1/recall))
  } else if (recall == 0){
    out = 2/((1/prec))
  } else {
    out = 2/((1/prec) + (1/recall))
  }
  return(out)
}

#' Levenshtein distance of two vectors
#' @description Calculate Levenshtein distance between all terms of v_a and all terms of v_b
#' @param v_a, character. vector of strings
#' @param v_b, character. vector of strings
#' @param threshold numeric. replaces end values with bool of values below value.
#' @details \url{https://en.wikipedia.org/wiki/Levenshtein_distance}
#' @return A numeric matrix with rows x columns as v_a x v_b.
#' @examples 
#' example_A = c("apple", "banana", "orange")
#' example_B = c("grapefruit", "orange", "strawberry")
#' aux_levenshtein(example_A, example_B)
#' @noRd
aux_levenshtein = function(v_a, v_b, threshold = NULL){
  # calculate the distance between every member of a and b.
  mat = c()
  for (x in 1:length(v_a)){
    row = c()
    for (y in 1:length(v_b)){
      row = c(row, utils::adist(v_a[x], v_b[y]) )
    }
    mat = rbind(mat, row)
  }
  
  if (!is.null(threshold)){
     mat = mat < threshold
  }
  
  if (is(mat, "matrix")){
    row.names(mat) = 1:nrow(mat)
  }
  
  return(mat)
}

#' Minimum levenshtein distance of a matrix
#' @description Calculate Levenshtein distance between all terms of v_a and all terms of v_b
#' @param v_a, character. vector of strings
#' @param v_b, character. vector of strings
#' @param threshold numeric. replaces end values with bool of values below value.
#' @details \url{https://en.wikipedia.org/wiki/Levenshtein_distance}
#' @return A numeric matrix with rows x columns as v_a x v_b.
#' @examples 
#' example_A = c("apple", "banana", "orange")
#' example_B = c("grapefruit", "orange", "strawberry")
#' aux_lev_matrix(example_A, example_B)
#' @noRd
aux_lev_matrix = function(x, y, method){
  mean_out = c()
  for (z in 1:length(x)){
    # Let's manually enforce the metrics for now.
    if (method %in% c("all", "osa", "lv", "dl", "lcs", "jw")){
      mean_out = c(
        mean_out,
        c(min(sapply(x[z], FUN = utils::adist,
                     y = y)))
      )
    } else {
      message("Error: string_distance must be one of 'all', 'osa', 'lv', 'dl', 'lcs' or 'jw'. See documentation for more information on each method.")
      break
    }
  }
  return(mean_out)
}


# euclidean distance calc, probably to be replaced by the gecko version on a later release
aux_dist_lite = function(x, y){
  # Euclidean distance
  v_inter <- c(0)
  for (i in 1:length(x)) {
    v_inter <- c(v_inter + (as.numeric(x[, i]) - as.numeric(y[i]))^2)
  }
  out <- sqrt(v_inter)
  return(out)
  # Gower distance
  # To be implemented in a future update
}


# calc of EOO and AOO, probably to be replaced by the gecko version on a later release
draw_geom = function(points, surface){
  if (surface == "EOO"){
    # Minimum convex polygon for EOO
    hull = chull(points)
    coords = points[c(hull, hull[1]), ] 
    return(terra::vect(as.matrix(coords), type = "Polygons"))
    
  } else if (surface == "AOO"){
    # AOO 
    # from red::longlat2utm
    if (is(points, "data.frame")){
      longlat = as.matrix(points)
    }
    
    minlong = min(longlat[,1])
    zone = floor((minlong + 180) / 6) + 1
    res = terra::project(longlat,
                         from = "+proj=longlat +datum=WGS84",
                         to = paste0("+proj=utm +zone=", zone," ellps=WGS84"))
    
    spData <- res
    spData = floor(spData/2000)
    ncells = nrow(unique(spData))
    area = ncells * 4
    return(area)
    
  } else {
    warning("Invalid request.")
    return(NULL)
  }
}

# API requests -----------------------------------------------------------------
#' Clean if API key is valid
#' @description Takes a character string intended as an API key and checks if it is valid.
#' @param key character.
#' @param service character.
#' @return logical.
#' @examples 
#' \dontrun{
#' aux_check_key_valid("false_key", "GPT")
#' }
#' @noRd
aux_check_key_valid = function(key, service){
  if (service == "GPT"){
    if (!all(reticulate::py_module_available("openai"))){
      reticulate::py_install(c("openai"))
    }
    gpt_key_check = NULL
    reticulate::source_python(system.file("python", "3_gpt_key_check.py", package = "arete"))
    
    out = gpt_key_check(key)
    
    return(out)
  } else {
    # To implement 
  }
}


#' Perform a GPT API request
#' @description This function extracts text embedded in a PDF and processes it using the chatGPT API.
#' @param document character. string of a file with species data in either pdf or txt format, e.g: ./folder/file.pdf
#' @param type character. Type of chatgpt query. If "species&locations" then all information possible is extracted, if...
#' @param model character. Service to be used. Right now, only \code{GPT} is available.
#' @param species_name character. Binomial name of the species used with applicable \code{type}.   
#' @param user_key character. The user's chatgpt key. Both free keys and premimum keys are allowed. To make full use of premium key functionalities, please set "is_premium" to TRUE.
#' @param is_premium Boolean setting the type of user key.
#' @param outpath Character string of a path to save output to.
#' @return matrix / data.frame
#' @examples
#' \dontrun{
#' GPTrequest(document = text, type = query, species_name = specnam, user_key = user_key,
#' is_premium = is_premium, outpath = outpath)
#' }
#' @noRd
GPTrequest = function(document, type = "species", species_name = NULL, model, user_key,
                      is_premium = FALSE, outpath = NULL){
  # https://community.openai.com/t/chatgpt-4-context-lengths/114919
  run_chatGPT = NULL
  reticulate::source_python(system.file("python", "3_GPT.py",package = "arete"))
  
  if (!is(user_key, "list")){
    warning("user_key must be a character of a valid API key to a chatGPT account!")
    return(NULL)
  }
  
  if (is_premium){
    timeout = 60
  } else {
    timeout = 30
  }
  
  xpected_out = "csv"
  
  
  
  if (type == "places"){
    ## Prompt construction  ----------------------------------------------------
    # Asking GPT to output a JSON array has been tried but is unreliable (GPT 3).
    
    if (is.null(species_name) ){
      main_prompt = paste0("Read the following document then reply only a table ",
                           "consisting of three columns, Species, Location, and Coordinates containing ",
                           "new geographic data and the in-text coordinates of species mentioned in the ",
                           "following document. Include locations as they are written in the text, aside from the special character: |. ",
                           "Include locations without coordinates. Do not include species with neither location nor coordinate data. Do not include those in scientific ",
                           "articles or outside references: ")
    } else {
      main_prompt = paste0("Read the following document then reply only a table ",
                           "consisting of three columns, Species, Location, and Coordinates containing ",
                           "new geographic data and the in-text coordinates for ", species_name, " mentioned in the ",
                           "following document. Include locations as they are written in the text, aside from the special character: |. ",
                           "Include locations without coordinates. Do not include species with neither location nor coordinate data. Do not include those in scientific ",
                           "articles or outside references: ")
      
    }
    
    
    
    xpected_cols = c("Species", "Location", "Coordinates")
    xpected_out = "csv"
    
  }
  
  prompt = list()
  if (is(document, "list")){
    for (i in 1:length(document)){
      prompt[[i]] = paste0(main_prompt, document[[i]])
    }
  } else {
    prompt[[1]] = paste0(main_prompt, document)
  }
  
  # if (debug)
  for (tx in 1:length(prompt)){
    
    debug_files = file(paste0(outpath, "_txt_", tx, ".txt"))
    writeLines(prompt[[tx]], debug_files)
    close(debug_files)
    
    
  }
  
  
  
  ## request data ---------------------------------------------------------------
  if (is(prompt, "list")){
    data = list()
    
    out_table = as.data.frame(matrix(nrow = 0 , ncol = length(xpected_cols),
                                     dimnames = list(c(),xpected_cols) ))
    
    for (i in 1:length(prompt)){
      prompt_content = tryCatch({
        prompt_content = run_chatGPT(prompt[[i]] , user_key$key, model)
      }, error = function(e){
        return(e$message)  
      }
      )
      
      ## Handle API errors -----------------------------------------------------
      if (is(prompt_content, "character")){
        if (grepl("openai.error.ServiceUnavailableError", prompt_content) || grepl("openai.error.APIError", prompt_content)){
          next
        }
        if (grepl("openai.error.InvalidRequestError", prompt_content)){
          warning("OpenAI: InvalidRequestError. This is often due to the length of the message.", immediate. = TRUE)
          next
        }
        
        # if (grepl) "Unable to access object (object is from previous session and is now invalid)"
        
      }
      
      ## Comply with query/minute limits ---------------------------------------
      Sys.sleep(timeout) 
      
      if (prompt_content == "NA"){next} # skip if NA
      
      ## Formatting fixes -------------------------------------------------------
      if (xpected_out == "json"){
        ## JSON ----------------------------------------------------------------
        prompt_content = jsonlite::fromJSON(prompt_content)
        # json_to_table = prompt_content$table$data[[1]]
        colnames(prompt_content) = c("Species", "Location", "Coordinates")
        out_table = rbind(out_table, prompt_content) 
      } else {
        ## CSV -----------------------------------------------------------------
        ### 1 - Check formatting type ------------------------------------------
        if (grepl("\t", prompt_content)){
          separator = "\t"
        } else {
          separator = "|"
        }
        
        ### 1 - (ALT) Determining formatting -----------------------------------
        # stringr::str_locate(prompt_content, "Species")
        # 
        # for (jj in 1:nrow(test)){
        #   print(nrow(stringr::str_locate_all(test[jj,], "\\|")[[1]]))
        # }
        
        
        ### 2 - Broken last line -----------------------------------------------
        brokenLine <- tryCatch(
          {
            read.csv(text = prompt_content, sep = separator)
            brokenLine = FALSE
          },
          error = function(e) {
            if (stringr::str_detect(e$message, "incomplete final line")) brokenLine = TRUE
          }
        )
        
        if (brokenLine){
          edited = sub('\"', "", prompt_content)
          tryCatch(
            {
              read.csv(text = edited, sep = separator)
              prompt_content = edited
            },
            error = function(e) {
              return(e)
            }
          )
        }
        
        data[i] = prompt_content
        
        # Save temps ---------------------------------------------------------
        write(data[[i]], paste0(tempdir(), "/temp.txt"))
        
        # Check for broken table ---------------------------------------------
        brokenTable = tryCatch(
          {
            testtab = read.csv(paste0(tempdir(), "/temp.txt"), sep = separator)
            # Check for broken header, fix if that's the case
            for (tab_col in xpected_cols){
              names(testtab)[grepl(tab_col , names(testtab))] = tab_col
              
            }
            bt_check_table = rbind(out_table, testtab[,xpected_cols])
            brokenTable = FALSE
          }
          ,
          error = function(e){
            brokenTable = TRUE
          }
        )
        unlink(paste0(tempdir(), "/temp.txt"))
        
        if (brokenTable){next}
        
        out_table = bt_check_table
        brokenTable = FALSE
      }
      
      
      # Check for garbage ------------------------------------------------------
      to_delete = c()
      flag = FALSE
      common_mistakes = c("spider", "spiders", "species")
      
      for (r in 1:nrow(out_table)){
        x = out_table[r, 1] # Test strings here
        x = fedmatch::clean_strings(x)
        a = gregexpr(" ", x)
        
        # Correct lens? --------------------------------------------------------
        terms = c()
        if (length(a[[1]]) == 1 && a[[1]][1] != -1 && nchar(x) > 0) {
          terms <- c(terms, substr(x, 1, a[[1]][1] - 1), substr(x, a[[1]][1] + 1, nchar(x)))
        } else if (length(a[[1]]) == 3) { # sp nov exception
          terms <- c(
            terms, substr(x, 1, a[[1]][1] - 1),
            substr(x, a[[1]][1] + 1, a[[1]][2] - 1),
            substr(x, a[[1]][2] + 1, nchar(x))
          )
          
          # This might be redundant given functionalities further ahead
          
          
          if ( !any(substr(x, a[[1]][2] + 1, nchar(x)) == c("sp. nov.", "sp. n.", "n. sp.", "sp nov", "sp n", "n sp")) ) {
            flag <- TRUE
          } else {
            flag <- FALSE
            terms <- terms[-3]
          }
        } else {
          flag <- TRUE
        }
        
        if (flag) {
          flag <- FALSE
          to_delete <- c(to_delete, r)
          next
        }
        
        
        # Correct symbols? -----------------------------------------------------
        for (t in terms){
          match = 0
          for (c in 1:nchar(x)){
            match = match + any(letters %in% substr(t, c, c))
          }
          
          if (nchar(t) != match || any(common_mistakes %in% t)){
            flag = TRUE
          }
        }
        if (flag) {
          flag = FALSE
          to_delete = c(to_delete, r)
        }
      }
      
      if (length(to_delete) != 0){
        out_table = out_table[c(-to_delete),]
      }
      
      
      # End of loop ----------------------------------------------------------
      # print(paste0("At ", Sys.time(), " part ", i, " was skipped."))
      
      
    }
    if (xpected_out == "csv"){
      if (all(data == "NA") || length(data) == 0){
        return(NA) # all(data == "NA") not needed anymore I think
      }
    }
    
    if (is(outpath, "character")){
      write.csv(x = out_table, file = paste0(outpath, ".csv"))
      
    }
    return(out_table)
  } else {
    data = run_chatGPT(prompt, user_key$key, model)
    write(data, paste0(tempdir(), "/temp.txt"))
    tabletest = read.csv(paste0(tempdir(), "/temp.txt"), sep = "|")
    unlink(paste0(tempdir(), "/temp.txt"))
    
    if (is(outpath, "character")){
      write.csv(tabletest, paste0(outpath, ".csv"))
    }
    return(tabletest)
  }
  # process data (e.g.: on fail) -----------------------------------------------
}



# MISC -------------------------------------------------------------------------

aux_error_frame = function(dataset, species, structure, error, id){
  not_found = dataset[,structure$Species] == species
  
  not_found = data.frame(
    Lat = c(dataset[not_found, structure$Lat]),
    Long = c(dataset[not_found, structure$Long]),
    Classification = rep(error, sum(not_found)),
    MinDist = rep(NA, sum(not_found)),
    MeanDist = rep(NA, sum(not_found)), file = rep(id, sum(not_found))
  )
  
  return(not_found)
}


#' Clean character string
#' @description Cleans a string of potential invalid characters and perform additional
#' measures that can reduce model errors, such as restricting text to a window around the first mention of a species.
#' @param data character.
#' @param extra_measures description
#' @return character
#' @examples
#' aux_clean_string(readLines(arete_data("holzapfelae")))
#' @noRd
aux_clean_string = function(data, extra_measures = NULL){
  
  clean_data <- stringr::str_replace_all(
    data,
    c(
      "\n" = " ", "\t" = " ", # " \ " = " ", "\\[|\\]" = " ", # " \ " = "", "\\[|\\]" = "",
      "\\u2642\\u2642" = "males", "\\u2642\\u2640" = "male and female",
      "\\u2640\\u2642" = "female and male", "\\u2640\\u2640" = "females",
      "\\u2642" = "male", "\\u2640" = "female", "\\u00fc" = "u", "\\u011f" = "g",
      "\\u0160" = "S",
      "\\u0161" = "s", "\\u015f" = "s", "\\u00e1" = "a", "\\u00e9" = "e",
      "\\u011b" = "e", "\\u00ed" = "i",
      "\\u00b1" = "plus-minus", ">" = "greater-than", "<" = "less-than",
      "\\u0148" = "n", "\\u0142" = "l", "\\u00df" = "ss", "\\u00f6" = "o",
      "\\u00d6" = "O", "\\u00e4" = "a",
      "\\u0159" = "r", "\\u00a9" = "", "\\u24b8" = "", "@" = "at", "&" = "and"
    )
  )
  
  if (!is.null(extra_measures)){
    # First try through detecting headers
    if (extra_measures[[1]] == "header" || extra_measures[[1]] == "both"){
        headers = c()
        for (x in 1:length(clean_data)){
          pos_headers = regexpr("^[#]+" , clean_data[x])
          if (pos_headers[1] != -1){
            headers = c(headers, x)
          }
        }
        species_mention = grepl(extra_measures[[2]], clean_data[headers])
        if (any(species_mention)){
          # And multiple mentions? Implement soon
          cropped_txt = clean_data[headers[species_mention]:length(clean_data)]
        }
      
      
    }
    
    if (extra_measures[[1]] == "mention" || extra_measures[[1]] == "both"){
      # if nothing is found, something a bit less refined
      
      # A common problem with text derived from OCR is spacing so let's search for
      # combinations with spaces.
      term_treat = c()
      for (j in 1:nchar(extra_measures[[2]])){
        term_treat = c(term_treat, substr(extra_measures[[2]], j, j))
      }
      term_treat = paste(term_treat, collapse = "[[:space:]]?" )
      matches = gregexpr(term_treat, clean_data)[[1]] # problem_situations[[i]][1]
      
      if (length(matches) == 0){
        print("Nothing can be found! File may not contain information or is altered beyond recognition.")
      } else {
        if (length(matches) > 1){
          likely_area = stats::quantile(matches, c(0.15, 0.85))
          likely_area = c(matches < likely_area[1]) + c(matches > likely_area[2])
          if (length(matches[likely_area == 0]) != 0){
            matches = matches[likely_area == 0]
          }
        }
        
        limits = c(matches[1], matches[length(matches)] + round(0.05*nchar(clean_data))) # clean_data used to be species_txt
        limits[limits > nchar(clean_data)] = nchar(clean_data)
        
        clean_data = substr(clean_data, limits[1],  limits[2])
      }
    }
  }

  return(clean_data)
}


#' Mechanical coordinate conversion
#' @description Mechanically convert character strings containing geographic coordinates and convert to sets of numeric values. Useful as most LLM experience better and more consistent results when asked to return a single string instead of two separate values.
#' @param coord_string character. A string containg potential geographic coordinates.  
#' @details Will convert all strings to N, W for consistency's sake.
#' Future updates will probably make it a toggle.
#' @return list. Contain latitude and longitude as the first and second elements,
#' respectively. 
#' @examples
#' example = "19 ° 34 ’ S 29 ° 10 ° E"
#' aux_string_to_coords(example)
#' @export
aux_string_to_coords = function(coord_string){
  if (!stringr::str_detect(coord_string, "[:digit:]")){
    warning("Input string does not have any digits. It does not contain coordinates.")
    return(NULL)
  }
  
  # Unit equivalents
  # All equivalent symbols and expressions should be defined here.
  coord_string = stringr::str_replace_all(coord_string, "[\\u00ba\\u00b0\\u2019'.]",  "\\*")
  coord_string = stringr::str_replace_all(coord_string, "degrees",  "\\*")
  coord_string = tolower(coord_string)
  coord_string = stringr::str_trim(coord_string)
  
  # Determine the directions
  coord_sys = c()
  if (grepl("n", coord_string)){
    coord_sys = c(coord_sys, "n")
  } else if (grepl("s", coord_string)){
    coord_sys = c(coord_sys, "s")
  } else {
    coord_sys = c(coord_sys, NA)
  }
  if (grepl("e", coord_string)){
    coord_sys = c(coord_sys, "e")
  } else if (grepl("w", coord_string)){
    coord_sys = c(coord_sys, "w")
  } else {
    coord_sys = c(coord_sys, NA)
  }
  
  # Spacing
  for (i in coord_sys){
    if (grepl(paste0(i, "[0-9]") , coord_string)){
      coord_string = gsub(i, paste0(i, " "), coord_string)
    }
  }
  
  if (all(!is.na(coord_sys))){
    
    if ( grepl(paste0("^[nesw] [0-9]+"), coord_string) ){
      # they are doing N XXXX E YYYY. separate at E, remove N
      pos_n = gregexpr(coord_sys[1], coord_string)[[1]][1]
      pos_e = gregexpr(coord_sys[2], coord_string)[[1]][1]
      
      if (any(substr(coord_string, 1, 1) == c("n", "s"))){
        out_l = list(substr(coord_string, 1, pos_e - 1),
                     substr(coord_string, pos_e, nchar(coord_string) )
        )
      } else {
        out_l = list(substr(coord_string, pos_n, nchar(coord_string)),
                     substr(coord_string, 1, pos_n - 1 )
        )
      }
      
      
    } else {
      pos_n = gregexpr(coord_sys[1], coord_string)[[1]][1]
      pos_e = gregexpr(coord_sys[2], coord_string)[[1]][1]
      
      if (pos_n < pos_e){
        out_l = list(substr(coord_string, 1, pos_n - 1),
                     substr(coord_string, pos_n, nchar(coord_string) )
        )
      } else {
        out_l = list(substr(coord_string, pos_e, nchar(coord_string)),
                     substr(coord_string, 1, pos_e - 1 )
        )
      }
      
    }
    
  } else {
    # let's assume that there are no cardinals
    out_l = list(
      substr(coord_string, 1, gregexpr(",", coord_string)[[1]][1] - 1),
      substr(coord_string, gregexpr(",", coord_string)[[1]][1] + 1, nchar(coord_string) )
    )
    
  }
  
  
  
  
  # Process each half of the coordinates
  for (i in 1:2){
    # then we can look at where the unit mark is 
    negative_flag = FALSE
    
    # if we have this, then that character is being used at the end
    # instead of our coordinates.
    marks_det = gregexpr("\\*", out_l[i] )
    
    unit_mark = marks_det[[1]][1]
    if (length(marks_det[[1]]) > 1){
      
      marks_len = attributes(marks_det[[1]])[[1]]
      for (j in 2:length(marks_det[[1]])){
        stringr::str_sub(out_l[i], marks_det[[1]][j], (marks_det[[1]][j] + marks_len[j] - 1)) = " "
      }
      
    }
    
    if (any(stringr::str_detect(out_l[[i]], c("-[:digit:]", "- [:digit:]")))){
      negative_flag = TRUE
    } else if (stringr::str_detect(out_l[[i]], "[:digit:]")){
      
    }
    
    if (is.null(unit_mark)){
      decimals = out_l[[i]]
    } else {
      decimals = substr(out_l[[i]], unit_mark - 4, unit_mark - 1)
    }
    
    for (c in 1:nchar(decimals)){
      if( any(letters %in% substr(decimals, c, c)) ){
        substr(decimals, c, c) = " "
      }
    }
    
    # Process decimals
    decimals = fedmatch::clean_strings(decimals)
    
    # decimals = stringr::str_replace_all(decimals, " ",  "") # new line
    decimals = as.numeric(decimals)
    
    if (is.na(decimals)){
      warning("Could not convert input string, please check if valid format.")
      return(NULL)
    }
    
    
    
    if (decimals < 0){
      negative_flag = TRUE
      decimals = abs(decimals)
    }
    
    sub = fedmatch::clean_strings(out_l[[i]])
    
    # sub11 = stringr::str_squish(sub11)
    for (c in 1:nchar(sub)){
      if( any(letters %in% substr(sub, c, c)) ){
        substr(sub, c, c) = " "
      }
    }
    sub = stringr::str_replace_all(sub, " ",  "")
    sub = as.numeric(sub)
    sub = sub/(10^(nchar(sub) - nchar(decimals)))
    if (negative_flag){
      sub = - sub
    }
    
    if (all(!is.na(coord_sys))){
      if (i == 1 && coord_sys[1] == "s"){
        out_l[[i]] = - sub
      } else  {
        out_l[[i]] = sub
      }
    } else {
      out_l[[i]] = sub
    }
    
  }
  
  
  # The resulting output must have the structure of longlat coordinates, if it
  # doesn't the function has failed.
  if (out_l[[1]] < -180 || out_l[[1]] > 180 || out_l[[2]] < -180 || out_l[[2]] > 180){
    warning("Could not convert input string, please check if valid format.")
    return(NULL)
  } else {
    return(out_l)
  }
}

#' Shave file extension
#' @description Quick removal of .csv, .tsv and .txt extensions from files.
#' @param files character. Full path to files you want the extension removed from.
#' @return character
#' @examples
#' example = "path/to/file.txt"
#' @noRd
aux_shave_extension = function(files){
  exts = substr(files, nchar(files)-3, nchar(files))
  need_removal = c()
  for(i in 1:length(exts)){
    if (exts[i] == ".csv" || exts[i] == ".tsv" || exts[i] == ".txt"){
      need_removal = c(need_removal, TRUE)
    } else {
      need_removal = c(need_removal, FALSE)
    }
  }
  # need_removal = substr(files, nchar(files)-3, nchar(files)) == ".csv"
  files = substr(files[need_removal], 1, nchar(files[need_removal])-4)
  return(files)
}

#' Recursive path creation
#' @description Create all folders needed to create a given path
#' @param paths character. Full path to files you want the extension removed from.
#' @return NULL.
#' @examples 
#' \donttest{
#' example = "path/to/file"
#' }
#' @noRd
aux_make_directory = function(paths){
  if (any(!dir.exists(paths))){
    for (r in 1:length(paths)){
      if ((!dir.exists(paths))[r]){
        dir.create(paths[r], recursive = TRUE)
      }
    }
  }
}

aux_check_pdf_chars = function(path){
  return(nchar(pdftools::pdf_text(path)))
}

# POST EXTRACTION PROCESSING - content could be put elsewhere later
# input_mat is a data.frame, outcome of gathering GPT extracted data. 
# species = "arabelia pheidoleicomes"
# i_feel_lucky is a user confidence variable
aux_further_processing = function(input, species, synonyms = NULL, i_feel_lucky = TRUE){
  species = tolower(species)
  to_remove = c()
  
  if (length(species) > 1){
    # remove everything that doesn't involve the species, and synonyms given
    for (i in species){
      to_remove = c(to_remove, row.names(input[stringr::str_trim(tolower(input$Species)) != i,]))
    }
  } else {
    to_remove = c(to_remove, row.names(input[stringr::str_trim(tolower(input$Species)) != species,]))
  }
  
  
  # remove 100% invalid locations
  to_remove = c(to_remove, row.names(input[stringr::str_trim(tolower(input[,2])) %in% c("unknown", "n/a", "not mentioned in the document"), ]))
  
  to_remove = unique(to_remove)
  
  if (i_feel_lucky){
    # Note: Currently there is no way to deal with multiple location mistakes, e.g: location = "Lisbon, Porto"
    to_use_gazetteer = row.names(input)[stringr::str_trim(tolower(input[,3])) %in% c("unknown", "n/a", "not mentioned in the document")]
    to_use_gazetteer = to_use_gazetteer[!to_use_gazetteer %in% to_remove]
    
  } else {
    
  }
  
  output = list("suggested deletions" = to_remove,
                "suggested uses of gazetteer" = to_use_gazetteer)
  
  return(output)
}


# Quick Annotation Tools (QAT) -------------------------------------------------
# this function gets the labels of a list of annotations, used in file_comparison

#' Retrieve element from a list of labels
#' @description Retrieve element from a list of labels
#' @param x character. One of the names of the annotators, of which the elements of y are named after. 
#' @param y list. a collection of annotated terms. e.g.: 
#' $2
#' out_ID       out_labs     lab
#' 38-1;38-2 Hypothyce rayi Species
#' @param z character. A text ID element, e.g. "38-1;38-2"  
#' @return character.
#' @noRd
aux_get_adds = function(x,y,z){
  if (!(x %in% as.numeric(names(y)))){
    return(NULL)
  } else {
    values = y[[x]][,1] == z
    if (any(values)){
      return(y[[x]][values,3])
    } else {
      return(NULL)
    }
  }
}

#' Check for names containing names
#' @description Go over terms in a set of labels and check which are entirely contained inside others.
#' @param labels character. Vector of strings.
#' @param full boolean. If TRUE the entire labels x labels matrix will be returned, not just ones in focus.  
#' @return matrix. A matrix of booleans, interpreted as rows contained in columns.
#' @examples
#' example = c("apple", "nana", "banana")
#' aux_including_names(example, full = TRUE)
#' @noRd
aux_including_names = function(labels, full = FALSE){
  same_mat = matrix(nrow = 0, ncol = length(labels))
  for (b in labels){
    same_mat = rbind(same_mat, stringr::str_detect(labels, b))
  }
  row.names(same_mat) = labels
  colnames(same_mat) = labels
  
  for (b in 1:nrow(same_mat)){
    same_mat[b,b] = FALSE
  }
  
  # in a future version this should be reworked as to not need "full" which exists 
  # due to issues when nrow == 1
  if (!full){
    same_mat = same_mat[rowSums(same_mat) != 0,]
  } 
  return(same_mat)
}


#' Labels for model training
#' @description Extract the labels and relations in a webanno file to an easy, machine readable format ready
#' for machine learning projects.
#' @param x A path to a WebAnno TSV v3.3 file.
#' @param y A path to a WebAnno TSV v3.3 file.
#' @param z A path to a WebAnno TSV v3.3 file.
#' @param rel_type logical. Whether or not you'd like a column with the positional ID of the related terms.
#' @param rel_tag logical. Whether or not you'd like a column with the positional ID of the related terms.
#' @param rel_ID logical. Whether or not you'd like a column with the positional ID of the related terms.
#' @param handle_multiple A path to a WebAnno TSV v3.3 file.
#' @param intermediate The object inter_table created with labels().
#' @details Right now, finds out the total sum of differences between all aspects of a given text.
#' @return A list of dataframes, each named after the corresponding line in the text. 
#' @noRd
aux_append_relations = function(x, y, z, rel_type, rel_tag, rel_ID, handle_multiple, intermediate){
  relational_tab = data.frame(ID = c(NULL), term = c(NULL), tag = c(NULL), relation_tag = c(NULL))
  
  uniq = labels_unique(x)$labels
  
  # If the relation requested is not present
  if (!any(labels_unique(x)$relations == z)){
    intermediate = cbind(intermediate,
                         term = rep(NA, nrow(intermediate)),
                         tag = rep(NA, nrow(intermediate)))
    return(intermediate)
    
  } else {
    for (l in 1:length(x@content)){
      for (m in 1:length(x@content[[l]][,7])){
        if (stringr::str_detect(x@content[[l]][,7][m], pattern = gsub("\\_", "\\\\_", z))){
          # cat(paste0(m,"\n"))
          
          if (x@content[[l]][m,5] %in% uniq){
            work_relation = rep(FALSE, nrow(x@content[[l]]))
            work_relation[m] = TRUE
          } else {
            work_relation = x@content[[l]][,5] == x@content[[l]][m,5]
          }
          
          ## Find intervals -------------------------------------------------------
          # Here we use regex to find all the intervals that relate to the element.
          all_positions = stringr::str_locate_all(x@content[[l]][m,8], "[0-9]+\\-[0-9]+")
          if (is(all_positions, "list")){ # I think this is unnecessary but just in case
            all_positions = all_positions[[1]]
          }
          
          for (w in 1:nrow(all_positions)){ 
            if (nrow(all_positions) == 1){ # this cannot handle all_positions having len == 1 | 86, 7
              brack_pos = all_positions
            } else {
              brack_pos = all_positions[w,]
            }
            
            
            ID_pos = c()
            regex_cases = c(
              paste0("^", substr(x@content[[l]][m, 8], brack_pos[1], brack_pos[2]), "$"),
              paste0("[^0-9]", substr(x@content[[l]][m, 8], brack_pos[1], brack_pos[2]), "$"),
              paste0("^", substr(x@content[[l]][m, 8], brack_pos[1], brack_pos[2]), "[^0-9]"),
              paste0("[^0-9]", substr(x@content[[l]][m, 8], brack_pos[1], brack_pos[2]), "[^0-9]")
            )
            for (y in regex_cases){
              ID_pos = cbind(ID_pos, stringr::str_detect(intermediate[,1], y))
            }
            ID_pos = rowSums(ID_pos) > 0
            
            
            if (any(ID_pos)){
              tag_pos = x@content[[l]][m,5]
              if (stringr::str_detect(tag_pos, "\\[")) {
                tag_pos = substr(tag_pos, 1, stringr::str_locate(tag_pos, "\\[") - 1)
              }
              
              # cat(paste0(l, " ", m))
              # 
              # print(length(
              #   c(intermediate[ID_pos,1],
              #     paste0(x@content[[l]][work_relation, 3], collapse = " "),
              #     tag_pos,
              #     x@content[[l]][,7][m])
              #   
              # ))
              
              data_vector = c(
                ID = intermediate[ID_pos,1],
                term = paste0(x@content[[l]][work_relation, 3], collapse = " ")
              )
              
              if (rel_type == TRUE){
                data_vector = c(data_vector, c(tag = tag_pos))
              }
              if (rel_tag == TRUE){
                data_vector = c(data_vector, c(relation_tag = x@content[[l]][,7][m]))
              }
              if (rel_ID == TRUE){
                data_vector = c(data_vector, c(tag_ID = paste0(x@content[[l]][work_relation,1], collapse = ";")))
              }
              relational_tab = rbind(relational_tab, data_vector)
              
            } else {
              next
            }
            
            
            
            
          }
          
        }
      }
    }
    
    ## Resolving multiples -----------------------------------------------------
    # This next section will handle the association between the data from the main
    # label in inter_table and the relational data in relational_tab. This is 
    # because sometimes there won't be a 1 to 1 association. For example in our
    # project we can expect 1 species value to 1 trait name but the same species
    # name can be linked to multiple locations.
    
    # Check if they exist.
    sums = c()
    rows_to_repeat = c()
    for (n in unique(relational_tab[,1])){
      sums = c(sums, sum(relational_tab[,1] == n)) 
      rows_to_repeat = c(rows_to_repeat, c(1:nrow(intermediate))[intermediate[,1] == n])
    }
    
    # Call a few things useful in all scenarios and future edits
    i_cols = ncol(intermediate)
    i_mains = 2 # number of cols acquired from intermediate
    i_rels = 1 # number of cols we want to add from relational_tab
    heads = c("term")
    
    for (rc in c("rel_type", "rel_tag", "rel_ID")){
      if (get(rc)){
        i_rels = i_rels + 1
        heads = c(heads, rc)
      }
    }
    
    replacement_frame = as.data.frame(matrix(NA, nrow = nrow(intermediate), ncol = i_rels, dimnames = list(c(), heads)))
    
    if (!any(sums > 1)){
      ## No duplicates ------------------------------------------------------------
      # If there are no issues 
      intermediate = cbind(intermediate, replacement_frame) # more options in the future?
      
      for (r in 1:nrow(intermediate)){
        to_annex = relational_tab[,1] == intermediate[r,1]
        if (any(to_annex)){
          intermediate[r, (i_cols+1):(i_cols+i_rels)] = relational_tab[to_annex, 2:(1+i_rels)] # switch to relative numbers
        } else {
          next
        }
      }
      
    } else if (handle_multiple == "duplicate"){
      ## Duplicate ----------------------------------------------------------------
      # if the same label (same entity) has been related to multiple different terms
      # copy that row and associate each different term with each row of that label
      
      # Check the need for rows_to_repeat again
      corr_rows = rows_to_repeat[sums > 1]  
      corr_sums = sums[sums > 1] - 1
      
      # make a table with the duplicates
      new_table = matrix(nrow = 0, ncol = i_cols)
      for (new_row in 1:length(rows_to_repeat[sums > 1])){
        new_table = rbind(new_table, intermediate[rep(corr_rows[new_row], corr_sums[new_row]),]
        )
      }
      
      intermediate = rbind(intermediate, new_table)
      
      intermediate = cbind(intermediate,
                           term = rep(NA, nrow(intermediate)),
                           tag = rep(NA, nrow(intermediate)))
      
      for (val in unique(relational_tab[,1]) ){
        intermediate[intermediate[,1] == val, (i_cols+1):(i_cols+i_rels)] = relational_tab[relational_tab[,1] == val, 2:(1+i_rels)]
      }
      
      
    } else if (handle_multiple == "merge"){
      ## Merge --------------------------------------------------------------------
      # if the same label (same entity) has been related to multiple different terms
      # merge those terms so that they resemble something like "Lisbon || Porto".
      
      # RELATIVE NUMBERS NEEDED
      intermediate = cbind(intermediate,
                           term = rep(NA, nrow(intermediate)),
                           tag = rep(NA, nrow(intermediate)))
      
      for (mer in 1:nrow(intermediate)){
        intermediate[mer, 3:4] = c(
          paste0(relational_tab[relational_tab[,1] == intermediate[mer,1], 2], collapse = " || "),
          paste0(relational_tab[relational_tab[,1] == intermediate[mer,1], 3], collapse = " || ")
        )
      }
    }
    return(intermediate)
  }
}

# Probably to be deleted on a future version
aux_check_folder = function(folder, term){
  check = apply(folder[,1], 2, function(x){
    substr(x, 5, nchar(x)) == term
  })
  annotators = apply(folder[check,1], 2, function(x){substr(x,1,3)})
  return(list(check, annotators))
}

# Probably to be deleted on a future version
aux_logical_clump = function(input_logical){
  clump_matrix = matrix(nrow = 0, ncol = 2)
  is_train = FALSE
  for (x in 1:length(input_logical)){
    y = input_logical[x]
    if (y){
      if (is_train){
        train_length = train_length + 1 
      } else {
        train_start = x
        train_length = 1 
      }
      
      if (x == length(input_logical)){
        if (is_train){
          clump_matrix = rbind(clump_matrix, c(train_start, train_length))
        } else {
          clump_matrix = rbind(clump_matrix, c(x, 1))
        }
        return(clump_matrix)
      }
      
      is_train = TRUE
    } else {
      if (is_train){
        clump_matrix = rbind(clump_matrix, c(train_start, train_length))
        if (x == length(input_logical)) {
          return(clump_matrix)
        } else {
          is_train = FALSE
        }
      }
    }
  }
  return(clump_matrix)
}


