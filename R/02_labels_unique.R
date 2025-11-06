#' Get the unique labels of a WebAnno document
#' @description Returns the unique classifications in the document
#' @param data character or WebAnnoTSV. The contents of a WebAnno TSV v3.3 file as
#' created by \code{\link{webanno_open}} or a path leading to it.
#' @param info character. A choice of "labels", "relations" and "all" on what to extract.
#' @return list. With up to two elements, \code{$labels} and \code{$relations}, containing the respective elements. 
#' @examples
#' example = arete_data("annotations")[[1]]
#' labels_unique(example, info = "all")
#' @export
labels_unique = function(data, info = "all"){
  out_list = list()
  
  # Initial checks -------------------------------------------------------------
  if (is(data, "WebAnnoTSV") || is(data, "character")){
    if (is(data, "character")){
      data = webanno_open(data)
    }
  } else {
    warning("Input must be either a WebAnnoTSV object or a path pointing to one.", immediate. = TRUE)
    return(NULL)
  }
  
  if (info == "labels" || info == "all"){
    # Labels -------------------------------------------------------------------
    uniq_labs = c()
    for (i in 1:length(data@content)){
      uniq_labs = c(uniq_labs, unique(data@content[[i]][,5])) 
    }
    uniq_labs = unique(uniq_labs)
    
    uniques = c()
    for (j in 1:length(uniq_labs)){
      # uniq_labs = c(uniq_labs, unique(file_a[[i]][,5])) 
      # loca = stringr::str_locate(uniq_labs[j], "[0-9]")
      loca = substr(uniq_labs[j], 1, stringr::str_locate(uniq_labs[j], "[0-9]")[1] - 2)
      
      if (!is.na(loca)){
        if (loca != "_"){
          uniques = c(uniques, loca)
        }
      } else {
        if (uniq_labs[j] != "_"){
          uniques = c(uniques, uniq_labs[j])
        }
      }
    }
    out_list[["labels"]] = unique(uniques)
  } 
  
  if (info == "relations" || info == "all"){
    # Relations ----------------------------------------------------------------
    uniq_labs = c()
    for (i in 1:length(data@content)){
      uniq_labs = c(uniq_labs, unique(data@content[[i]][,7])) 
    }
    uniq_labs = unique(uniq_labs)
    
    uni_out = c()
    for (i in 1:length(uniq_labs)) {
      if (stringr::str_detect(uniq_labs[i], "\\|")){
        uni_out = c(uni_out, stringr::str_split(uniq_labs[i], "\\|")[[1]])
      } else {
        uni_out = c(uni_out, uniq_labs[i])
      }
      
    }
    uni_out = unique(uni_out)
    uni_out = uni_out[as.numeric(uni_out == "_") + as.numeric(uni_out == "*") == 0]
    out_list[["relations"]] = unique(uni_out)
  } else {
    warning("info needs to be either 'labels', 'relations' or 'all'.", immediate. = TRUE)
    return(NULL)
  }
  return(out_list)
}
