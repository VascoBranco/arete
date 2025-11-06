#' Open a WebAnno TSV v3.3 file.
#' @description Read the contents of a WebAnno TSV 3.3 file, a format for annotated text containing named entities and relations.
#' @param path character. A path to a WebAnno TSV v3.3 file.
#' @param cut_to_content logical. Restrict the output file to those sentences containing
#' annotated labels and relations. 
#' @details One of the format types under use at \url{https://inception-project.github.io/}.
#' @return WebAnnoTSV. A list of dataframes, each named after the corresponding paragraph in the text. 
#' @examples
#' example = system.file(paste0("extdata/insecta_annot_1.tsv"), package = "arete")
#' 
#' webanno_open(example)
#' @export
webanno_open = function(path, cut_to_content = FALSE){
  
  text = readLines(path)
  # lines to skip, flag for found content, output list
  to_skip = c()
  text_flag = FALSE
  ID = 0
  
  output = list(l_cont = list(), l_text = list())
  
  for (x in 1:length(text)){
    
    if (any(to_skip%in%x)){
      next
    }
    
    # Is it a TEXT line?
    if (grepl("#Text=", text[x])){
      line = strsplit(text[x], "#Text=")[[1]][2]
      y = x
      continue = TRUE
      
      while(continue){
        y = y + 1
        if (grepl("#Text=", text[y])){
          line = paste(line, strsplit(text[y], "#Text=")[[1]][2])
        } else {
          continue = FALSE
          y = y - 1
        }
      }
      to_skip = x:y
      text_flag = TRUE
    }
    
    if (any(to_skip%in%x)){
      next
    }
    
    
    
    if (text_flag){
      y = x
      out = matrix(nrow = 0, ncol = 8)
      out = rbind(out, strsplit(text[y], "\t")[[1]])
      continue = TRUE
      
      while(continue){
        if (y == length(text)){
          continue = FALSE
        } else {
          y = y + 1
          out = rbind(out, strsplit(text[y], "\t")[[1]])
          
          if (text[y] == ""){ # Or NA?
            continue = FALSE
          }
        }
        
      }
      # Currently will also skip the empty line
      to_skip = x:y
      text_flag = FALSE
      ID = ID + 1
      output$l_cont[[ID]] = out
      output$l_text[[ID]] = line
    }
    
  }
  
  if (cut_to_content){
    to_keep = sapply(X = 1:length(output$l_cont),
           FUN = function(x, y){
             if (sum(y[[x]][,4:8] == "_") != nrow(y[[x]]) * 5){
               return(x)
             }
           }, y = output$l_cont
    )
    to_keep = Reduce(c, to_keep)

    output$l_text = output$l_text[to_keep]
    output$l_cont = output$l_cont[to_keep]
  }
  
  return(webanno_creator(text = output$l_text, content = output$l_cont))
}
