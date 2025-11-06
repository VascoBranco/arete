#' Compare the contents of two WebAnno tsv files.
#' @description Detect differences between two WebAnno files of the same text 
#' for annotation monitoring.
#' @param input list of character or WebAnnoTSV. The contents of WebAnno TSV v3.3 files 
#' as created by \code{\link{webanno_open}} or a set of paths leading to them.
#' @param method character. A choice of "absolute" and "kripp".
#' @param null_category logical. In cases where one annotator labels something and the remaining do not, should it be assigned a category or be set as \code{NA}? 
#' @param log character. Optional path to save the 
#' @param verbose boolean. Print the output of the function at the end.
#' @details Right now, finds out the total sum of differences between all aspects of a given text.
#' Method \code{kripp} calculates the Krippendorf Alpha, "a reliability coefficient 
#' developed to measure the agreement among observers, coders, judges, raters, 
#' or measuring instruments drawing distinctions among typically unstructured 
#' phenomena or assign computable values to them. alpha emerged in content 
#' analysis but is widely applicable wherever two or more methods of generating 
#' data are applied to the same set of objects, units of analysis, or items and 
#' the question is how much the resulting data can be trusted to represent 
#' something real" (Krippendorf, 2011). 
#' @return list. Later should be a dataframe with differences per block in the text. 
#' @references 
#' Klaus Krippendorff (2011). Computing Krippendorff’s Alpha-Reliability. Departmental Papers (ASC). University of Pennsylvania. \url{https://repository.upenn.edu/handle/20.500.14332/2089} 
#' @examples
#' example = arete_data("annotations")
#' 
#' file_comparison(example)
#' @export
file_comparison = function(input, method = "kripp", null_category = TRUE, log = NULL, verbose = TRUE){
  out_l = list()
  # Initial checks -------------------------------------------------------------
  for (i in 1:length(input)){
    if (is(input[[i]], "WebAnnoTSV") || is(input[[i]], "character")){
      if (is(input[[i]], "character")){
        input[[i]] = webanno_open(input[[i]])
      }
    } else {
      warning("Input must be either a WebAnnoTSV object or a path pointing to one.", immediate. = TRUE)
      return(NULL)
    }
  }
  
  
  if (method == "kripp" || method == "light"){
    input_lab = lapply(input, FUN = function(x){labels_unique(x)$labels}) 
    input_lab = unique(Reduce(c, input_lab))
    irr_mat = matrix(rep("0", length(input)+1), ncol = length(input)+1)
    
    for (x in input_lab){
      file_labels = lapply(input, FUN = labels, label = x)
      names(file_labels) = 1:length(file_labels)
      
      file_labels = lapply(file_labels, FUN = function(x,y){if (nrow(x) != 0) cbind(x, lab = y)  }, y = x)
      file_labels = file_labels[!sapply(file_labels, is.null)] # test = sapply(file_labels, is.null)
      
      # list_el = 1:length(file_labels) # should be replaced in the future with as.numeric(names(file_labels))
      list_el = 1:length(input)
      
      for (j in list_el){
        focus_element = file_labels[[as.character(j)]]
        
        if (!is.null(focus_element)){ # If that element is not present, e.g. someone didn't annotate it at all, skip
          for (r in 1:nrow(focus_element)){
          
            prevs = irr_mat[,1] == focus_element[r,1]
            
            if (sum(prevs) == 0){ # not encountered yet then add
              
              out_labels = rep("NULL", length(input))
              
              if (j %in% as.numeric(names(file_labels))){
                if (length(list_el) == 1){
                  out_labels[as.numeric(names(file_labels))] = file_labels[[j]][r,3]
                  add_irr = c(file_labels[[j]][r,1], out_labels)
                } else {
                  app_out = sapply(X = names(file_labels), FUN = aux_get_adds, y = file_labels, z = focus_element[r,1])
                  out_labels[as.numeric(names(file_labels))] = as.character(app_out)
                  add_irr = c(focus_element[r,1], out_labels )
                }
                irr_mat = rbind(irr_mat, add_irr)
              }
              
            } 
          }
        }
      }
    }
    
    irr_mat = irr_mat[-1,]
    
    if (is(log, "character")){
      write.csv(irr_mat, log, row.names = FALSE)
    }
    

    if (!null_category){
      for (x in 1:nrow(irr_mat)){
        irr_mat[x,2:(1+length(input))][irr_mat[x,2:(1+length(input))] == "NULL"] = NA
      }
    }
    
    method_mat = matrix(as.character(irr_mat[,2:ncol(irr_mat)]), ncol = length(input)) # def a better way of doing this
    
    if (method == "kripp"){
      out_mat = irr::kripp.alpha(t(method_mat))
    } else if (method == "light"){
      out_mat = irr::kappam.light(method_mat)
    }
    
    return(
      list(
        mat = irr_mat,
        irr = out_mat
      )
    )
    
  } else {
    
    if (length(input[[1]]) != length(input[[2]])){
      warning("Files have incompatible sizes. Check that they're for the same document.", immediate. = TRUE)
      return(NULL)
    }
    
    # Length check ---------------------------------------------------------------
    for (k in 1:length(input[[1]])){
      if(!all(input[[1]]@content[[k]][,3] == input[[2]]@content[[k]][,3])){
        warning("Files chosen have different content. Check that they're for the same document.", immediate. = TRUE)
        return(NULL)
      }
    }
    
    # Similarity check ---------------------------------------------------------
    file_difs = c()
    file_total = 0
    for (i in 1:length(input[[1]])){
      anot_a = input[[1]]@content[[i]]
      anot_b = input[[2]]@content[[i]]
      
      block_difs = 0
      for (z in 4:8){
        # Check differences
        block_difs = block_difs + (nrow(anot_a) - sum(anot_a[,z] == anot_b[,z]))
        file_total = file_total + 5 * nrow(input[[1]]@content[[i]])
      }
      
      file_difs = c(file_difs, block_difs)
    }
    
    # Output -------------------------------------------------------------------
    if (verbose){
      cat(paste0("A total of ", sum(file_difs), " differences were found in ", file_total, " cells.\n"))
    }
    
    out_l["nmb_differences"] = sum(file_difs) 
    
    if (sum(file_difs) != 0){
      out_l["line_with_most_differences"] = names(out_l[order(file_difs, decreasing = T)[1]])
      if (verbose){
        cat(paste0("The line with the biggest amount of differences was: ", out_l["line_with_most_differences"], ".\n"))
      }
    } 
    return(out_l)
    
  }
  
}
