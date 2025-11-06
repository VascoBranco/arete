#' Process and fix species names
#' @description This function standardizes species names and fixes a number of 
#' common typos and mistakes that commonly occur due to OCR. 
#' @param species character. String with the name of the species to be processed.
#' @param processing character. One of \code{"trim"}, \code{"sp nov"}, \code{"hyphen broken"}, \code{"abbreviate"}, \code{"spacing"} and \code{"all"}. By default set to \code{"all"}, performing every edit.
#' @param enforce boolean. Remove non-conforming entries before returning.
#' @return character. A string with the standardized species name.
#' @examples
#' process_species_names("macro - thele calpeiana sp. nov.")
#' @export
process_species_names = function(species, processing = "all", enforce = FALSE){
  species = stringr::str_trim(tolower(species))
  for (r in 1:length(species)){
    
    if (is.na(species[r])){
      next
    } else {
      continue = TRUE
      step = 1
      work_species = species[r]
      while (continue && step < 10) {
        start_species = work_species
        work_species = tolower(work_species)
        # Fixes ------------------------------------------------------------------
        if (processing == "trim" || processing == "all"){
          work_species = stringr::str_trim(work_species)
        }
        
        if (processing == "sp nov" || processing == "all"){
          initial_pattern = c("sp. nov.", "sp. n.", "n. sp.", "sp nov", "sp n", "n sp")
          for (i in initial_pattern){
            if (grepl(i, work_species)){
              cut_pos = gregexpr(i,  work_species)
              cut_pos = cbind(cut_pos[[1]], cut_pos[[1]] + lapply(cut_pos, attributes)[[1]][[1]] - 1)
              stringr::str_sub(work_species, cut_pos[1,1], cut_pos[1,2]) <- ""
            }
          }
        }
        
        if (processing == "hyphen broken" || processing == "all"){
          initial_pattern = c("^[a-z]+ [a-z]+ - [a-z]+$", "^[a-z]. [a-z]+ - [a-z]+$", "^[a-z]+ - [a-z]+ [a-z]+$")
          for (i in initial_pattern){
            if (grepl(i, work_species)){
              cut_pos = gregexpr("[a-z] - [a-z]",  work_species)
              cut_pos = cbind(cut_pos[[1]], cut_pos[[1]] + lapply(cut_pos, attributes)[[1]][[1]] - 1)
              stringr::str_sub(work_species, cut_pos[1,1], cut_pos[1,2]) <- 
                paste0(substr(work_species, cut_pos[1,1], cut_pos[1,1]),
                       substr(work_species, cut_pos[1,2], cut_pos[1,2])
                ) 
            }
          }
        }
        
        if (processing == "abbreviate" || processing == "all"){
          if (grepl("^[a-z]+ [a-z]+$", work_species)){
            species_format_04 = gregexpr("^[a-z]+ [a-z]",  work_species)
            species_format_04 = cbind(species_format_04[[1]], (lapply(species_format_04, attributes)[[1]][[1]] - 1))
            stringr::str_sub(work_species, species_format_04[1,1], species_format_04[1,2]) <- paste0(stringr::str_sub(work_species, 1,1), ". ") 
          }
        }  
        
        if (processing == "spacing" || processing == "all"){
          if (grepl("[a-z] [.] [a-z]+", work_species)) {
            species_format_02 = gregexpr("[a-z] [.] [a-z]", work_species)
            for (j in 1:length(species_format_02[[1]])){
              species_format_03 = gregexpr("[a-z] [.] [a-z]", work_species)
              species_format_03 = cbind(species_format_03[[1]], 
                                        (species_format_03[[1]] + lapply(species_format_03, attributes)[[1]][[1]]) - 2  ) # species_format_03[[1]] + 
              
              stringr::str_sub(work_species, species_format_03[1,1], species_format_03[1,2]) <- paste0(stringr::str_sub(work_species, 1,1), ". ") 
            }
          }
        }
        
        continue = start_species != work_species
        step = step + 1
      }
    }
    

    if (enforce){
      work_species[!grepl("[a-z][.] [a-z]+", work_species)] = NA
    }
    species[r] = work_species
  }

  return(stringr::str_trim(species)) 
}