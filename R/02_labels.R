#' Labels for model training
#' @description Extract the labels and relations in a webanno file to an easy, 
#' machine readable format ready for machine learning projects.
#' @param data character or WebAnnoTSV. The contents of a WebAnno TSV v3.3 file 
#' as created by \code{\link{webanno_open}} or a path leading to it.
#' @param label character. The main label. The relations must go FROM this term.
#' @param relations character. The set of relations you'd like to extract.
#' @param show_type logical. Add a column with the type of relation of the related terms.
#' @param show_tag logical. Add a column with the tags of the related terms.
#' @param show_ID logical. Add a column with the positional ID of the related terms.
#' @param handle_multiple character. If there are multiple \code{relations} connecting to the same \code{label},
#' i.e. multiples locations, show should it be handled? Should \code{duplicate} rows be created or the content be \code{merge}'d?
#' @return A list of dataframes, organized with columns for the corresponding line in the text, label and relations (if \code{relations != NULL}) 
#' @examples
#' example = arete_data("annotations")[[1]]
#' labels(data = example, label = "Species", relations = "OCCURS")
#' 
#' labels(data = example,
#' label = c("TraitVal"), relations = c("meas_Sex")) 
#' 
#' labels(data = example,
#' label = c("TraitVal"), relations = c("meas_trait", "meas_Sex", "meas_Unit"))
#' 
#' labels(data = example,
#' label = c("TraitVal"), relations = c("meas_trait", "meas_Sex", "meas_Unit"),
#'  handle_multiple = "merge")  
#' @export
labels = function(data, label, relations = NULL, show_type = FALSE, show_tag = FALSE, show_ID = FALSE, handle_multiple = "duplicate"){
  ## In the future a check for special characters will be implemented
  
  # Initial checks -------------------------------------------------------------
  if (is(data, "WebAnnoTSV") || is(data, "character")){
    if (is(data, "character")){
      data = webanno_open(data)
    }
  } else {
    warning("Input must be either a WebAnnoTSV object or a path pointing to one.", immediate. = TRUE)
    return(NULL)
  }
  
  
  if (!is.null(relations)){
    relations = gsub("_", "\\\\_", relations) # "_" characters must be transformed to "\\_"
  }
  

  
  out_labs = c() 
  out_ID = c()
  
  # Label extraction -----------------------------------------------------------
  # The next for-loop goes through each section of the document and collects 
  # mentions of "y". Because expressions tagged with "y" can occupy several
  # elements (e.g. locations with multiple names such as "Caldas", "da", "Rainha"
  # or "Las", "Vegas").
  
  for (i in 1:length(data@content)){ 
    ## Check for labels with including names

    same_mat = aux_including_names(labels_unique(data)$labels)
    correct_term = stringr::str_detect(data@content[[i]][, 5], label)
    
    if (any(label == row.names(same_mat))){
      pot_confusion = colnames(same_mat)[same_mat[label == row.names(same_mat),]]
      
      for (j in pot_confusion){
        correct_term[stringr::str_detect(data@content[[i]][, 5], j)] = FALSE
      }
      
      y_table = matrix(data@content[[i]][correct_term,], ncol = 8)
    } else {
      y_table = matrix(data@content[[i]][correct_term,], ncol = 8)
    }
    
  
    is_numbered <- stringr::str_detect(y_table[, 4], paste0("\\*\\["))
    
    if (any(is_numbered)) {
      for (j in unique(y_table[is_numbered, 4])) {
        out_labs <- c(out_labs, paste0(y_table[, 3][y_table[, 4] == j], collapse = " "))
        out_ID <- c(out_ID, paste0(y_table[, 1][y_table[, 4] == j], collapse = ";"))
      }
      
    }
    
    y_table_non = matrix(y_table[!is_numbered,], ncol = 8)
    if (nrow(y_table_non) != 0){
      out_labs <- c(out_labs, y_table_non[, 3])
      out_ID <- c(out_ID, y_table_non[, 1])
    }
    
  } # end of looking for labels
  
  inter_table = data.frame(out_ID, out_labs)
  
  # Relations extraction -------------------------------------------------------
  # Next we need to go through all of the relations that we want, which are on V7 and
  # check if the elements in square brackets on V8 of the relational element matches with
  # those we got for our category. This is handled by aux_append_relations. 
  if (!is.null(relations)){
    for (el in relations){
      inter_table = aux_append_relations(
        x = data, y = label, z = el, rel_type = show_type, rel_ID = show_ID,
        rel_tag = show_tag, handle_multiple = handle_multiple, intermediate = inter_table
        )
    }
  }
  return(inter_table)
}
