#' Summarize the contents of a group of WebAnno tsv files
#' @description Returns 
#' @param input list of character or WebAnnoTSV. The contents of WebAnno TSV v3.3 files 
#' as created by \code{\link{webanno_open}} or a set of
#' paths leading to them.
#' @param goal character. One of "labels", "relations" or "all" determining what is summarized.
#' @param collected boolean. Organize the summary under a single dataframe, otherwise a list is returned.
#' @param verbose boolean. Print the final output.
#' @return dataframe.
#' @examples
#' example = arete_data("annotations")
#' 
#' webanno_summary(example)
#' @export
webanno_summary = function(input, goal = "relations", collected = TRUE, verbose = TRUE){
  
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

  if (goal == "labels"){
    doc_l = list()
    for (i in 1:length(input)){
      doc_out = matrix(ncol = 2)
      labs = labels_unique(input[[i]])
      for (j in labs$labels){
        doc_out = rbind(doc_out, c(j, nrow(labels(input[[i]], j))))
      }
      doc_out = as.data.frame(doc_out)
      doc_out[,2] = as.numeric(doc_out[,2])
      doc_out = doc_out[-c(1),]
      colnames(doc_out) = c("ID", "Sum")
      doc_l[[i]] = doc_out
    }
  } else if (goal == "relations"){
    doc_l = list()
    for (i in 1:length(input)){
      doc = input[[i]]
      doc_out = data.frame(ID = 0, Sum = 0)
      doc_out$Sum = as.numeric(doc_out$Sum)
      
      for (j in 1:length(doc@content)){ 
        # Label extraction ---------------------------------------------------------
        # Columns 5 and 4 will not always match. Column 4 will always be indicative 
        # of spans and should be the target.
        
        cur_uniq = unique(doc@content[[j]][,7])
        
        cur_uniq_out = list(
          add = c(),
          remove = c()
        )
        
        for (k in 1:length(cur_uniq)){
          sep_pos = stringr::str_locate_all(cur_uniq[k], "\\|")
          sep_pos = c(1, sep_pos[[1]][,1], nchar(cur_uniq[k]))
          sep_out = c()
          if (length(sep_pos) > 2){
            sep_seq = c(1:(length(sep_pos)-1)) # c(1:4)[-c(length(sep_pos))]
            for (h in sep_seq){
              sep_out = c(sep_out, substr(cur_uniq[k], sep_pos[h] + (h!=1), sep_pos[h+1] - (h!=length(sep_seq)) ))
            }
            cur_uniq_out$remove = c(cur_uniq_out$remove, k)
            cur_uniq_out$add = c(cur_uniq_out$add, sep_out)
          }
        }
        
        # end
        if (!is.null(cur_uniq_out$remove)){
          cur_uniq = cur_uniq[-cur_uniq_out$remove]
          # cur_uniq = c(cur_uniq, cur_uniq_out$add)
          cur_uniq = unique(cur_uniq)
          
          # We need to check to see if these terms have already appeared and add
          # them the appropriate amount of times, e.g: "meas\\_trait|meas\\_trait"
          
          
          uniq_tab = table(cur_uniq_out$add)
          
          for (k in 1:nrow(uniq_tab)){
            if (names(uniq_tab[k]) %in% doc_out[,1]){
              doc_out[doc_out[,1] == names(uniq_tab[k]),2] = doc_out[doc_out[,1] == names(uniq_tab[k]),2] + as.numeric(uniq_tab[k])
            } else {
              doc_out = rbind(doc_out, c(names(uniq_tab[k]), as.numeric(uniq_tab[k])))
            }
            doc_out$Sum = as.numeric(doc_out$Sum)
          }
          
          
          
        }
        
        for (k in 1:length(cur_uniq)){
          if (cur_uniq[k] %in% doc_out[,1]){
            doc_out[doc_out[,1] == cur_uniq[k], 2] = doc_out[doc_out[,1] == cur_uniq[k], 2] + sum(doc@content[[j]][,7] == cur_uniq[k])
          } else {
            doc_out = rbind(doc_out, c(cur_uniq[k], sum(doc@content[[j]][,7] == cur_uniq[k])) )
          }
          doc_out$Sum = as.numeric(doc_out$Sum)
        }
        
        
      }
      
      
      doc_out = doc_out[!is.na(doc_out[,1]),]
      doc_out = doc_out[-c(1),]
      doc_l[[i]] = doc_out
    }
    
  }

  if (collected){
    merged = Reduce(rbind, doc_l)
    merged_unique = unique(merged[,"ID"])

    merged_out = sapply(merged_unique, simplify = "matrix", FUN = function(x) {
      c(
        x, # test[x]
        sum(merged[merged[,"ID"] == x, 2]) # $Sum
      )
    })
    
    merged_out = t(merged_out)
    row.names(merged_out) = 1:nrow(merged_out)
    colnames(merged_out) = c("terms", "count")
    
    merged_out[,2] = as.numeric(merged_out[,2])
    return(merged_out)
  } else {
    return(doc_l)
  }
}
