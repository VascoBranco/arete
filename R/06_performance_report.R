#' Evaluate the performance of a LLM
#' @description Produce a detailed report on the discrepancies between LLM extracted 
#' data and human annotated data for the same collection of files.
#' @param human_data matrix. Ground truth dataset to compare the data extracted by a LLM.
#' @param model_data matrix. Dataset of location data, following the description under \code{human_data}.
#' @param full_locations character. Defines dataset structure. 
#' If \code{"locations"} then structure follows Species, Location, File. 
#' if \code{"coordinates"} then structure follows Species, Long, Lat, File. 
#' if \code{"both"} then structure follows Species, Location, Long, Lat, File.
#' @param string_distance character. Selects the method through which the proximity
#' between two strings is calculated, from those available under \code{\link[utils:adist]{utils::adist()}}.
#' @param verbose logical. Determines if output should be printed.
#' @param rmds logical. Determines if more extensive R Markdown files should be created at \code{path}.
#' @param path character. Directory to which the output of the function is saved.
#' @return list. A confusion matrix is returned for every species per document, plus
#' one for the entire process.  
#' @details Four main metrics are calculated to report on the performance of the model for coordinates. These are 
#' \itemize{
#' \item Accuracy, \eqn{\frac{TP}{TP + FP + FN}}, here defined as such in a system without True Negatives.
#' \item Recall, \eqn{\frac{TP}{TP + FN}}, Kent \emph{et al.} (1955)
#' \item Precision, \eqn{\frac{TP}{TP + FP}}, Kent \emph{et al.} (1955)
#' \item F1 score, \eqn{\frac{2}{\frac{1}{Precision} + \frac{1}{Sensitivity}}}, van Rijsbergen(1979).
#' }
#' Additional metrics are calculated, including: 1) a distance-weighed confusion matrix where the sum of each type of error (False Negatives and False Positives)
#' is done by weights, calculated to be inverse to the mean euclidean distance of that data point to all others. This way errors that are close to existing data
#' for that species will count less than those further way, i.e. a data point was hallucinated that was close to existing data or, a data point was missed that
#' is already represented in the data. This adjusted confusion matrix is also presented along with versions of the four main metrics calculated with these values.
#' To report on the performance of locations, by default the minimum Levenshtein distance (Levenshtein, 1966) between a term and all other terms is calculated. Which is defined as:
#' \deqn{
#' lev(a,b) = \begin{cases}
#'  |a| & if |b|=0, \\
#'  |b| & if |a|=0, \\
#'  lev(tail(a),tail(b)) & if head(a) = head(b), \\
#'  1 + min 
#'    \begin{cases}
#'      lev (tail(a),b) \\
#'      lev (a,tail(b)) \\
#'      lev (tail(a),tail(b)) \\
#'    \end{cases}
#'    & otherwise
#' \end{cases}
#' }
#' In short, the number of edits needed to turn one string \emph{a} into string \emph{b}.
#' @references
#' \itemize{
#' \item Kent, A. et al. (1955). "Machine literature searching VIII. Operational criteria for designing information retrieval systems", \emph{American Documentation}, 6(2), pp. 93–101. doi:10.1002/asi.5090060209.
#' \item van Rijsbergen, C.J. (1979). "Information Retrieval", Architectural Press. ISBN: 978-0408709293.
#' \item Levenshtein, V.I. (1966). "Binary codes capable of correcting deletions, insertions, and reversals", \emph{Soviet Physics-Doklady}, 10(8), pp. 707–710 [Translated from Russian]. 
#' }
#' @examples
#' trial_data = arete::arete_data("holzapfelae-extract")
#' trial_data = cbind(trial_data[,1:2], arete::string_to_coords(trial_data[,3])[2:1], trial_data[,4:5])
#' 
#' trial_data = list(
#'   GT = trial_data[trial_data$Type == "Ground truth", 1:5],
#'   MD = trial_data[trial_data$Type == "Model", 1:5]
#' )
#' 
#' # make sure you run arete_setup() beforehand!
#' performance_report(
#'   trial_data$GT,
#'   trial_data$MD,
#'   full_locations = "both",
#'   verbose = FALSE,
#'   rmds = FALSE
#' )
#' @export
performance_report = function(human_data, model_data, full_locations = "coordinates", string_distance = "levenshtein", verbose = TRUE, rmds = TRUE, path = NULL){
  #Needs to be updated to call the installation path of the report templates
  fn_out = list()
  fn_out[["exclusive_to_each_set"]] = c()
  
  
  if (string_distance == "levenshtein"){
    string_distance = "lv"
  }
  
  out_global <- list(
    non_intersecting = c(0, 0), accuracy = c(),
    sensitivity = c(), precision = c(), f1 = c(),
    data_used = 0,
    class_dist_global = data.frame(
      Lat = c(NULL), Long = c(NULL),
      Classification = c(NULL), MinDist = c(NULL), MeanDist = c(NULL), file = c(NULL)
    )
  )
  
  # define structure
  if (full_locations == "both"){ 
    s = data.frame(Species = 1, Location = 2, Long = 3, Lat = 4, File = 5)
  } else if (full_locations == "coordinates"){
    s = data.frame(Species = 1, Location = NA, Long = 2, Lat = 3, File = 4)
  } else if (full_locations == "locations"){
    s = data.frame(Species = 1, Location = 2, Long = NA, Lat = NA, File = 3)
  } else {
    message("full_locations must be one of 'coordinates', 'locations' or 'both'.")
    return(NULL)
  }
  
  # Locations ------------------------------------------------------------------
  common_files = intersect(human_data[,s$File], model_data[,s$File])
  
  if (full_locations == "locations" || full_locations == "both"){

    min_lev = matrix(nrow = 0, ncol = 2)
    
    for (i in common_files){
      ## Human -----------------------------------------------------------------
      human_prep = human_data[human_data[,s$File] == i, ] # used to be 4 here and below
      student_species_mentioned = unique(human_prep[,s$Species])
      # Model -----------------------------------------------------------------
      model_prep = model_data[model_data[,s$File] == i, ]
      gpt_species_mentioned = unique(model_prep[,s$Species])
      
      if (nrow(model_prep) == 0 || nrow(human_prep) == 0){
        next
      }
      
      # Let's manually enforce the metrics for now.
      if (string_distance %in% c("all", "osa", "lv", "dl", "lcs", "jw")){
        # min_lev = matrix(nrow = 0, ncol = 2)
        min_lev = rbind(
          min_lev,
          cbind(
            # aux_lev_matrix(model_prep[,s$Location], human_prep[,s$Location], string_distance),
            aux_lev_matrix(human_prep[,s$Location], model_prep[,s$Location], string_distance),
            rep(i, length(aux_lev_matrix(human_prep[,s$Location], model_prep[,s$Location], string_distance)))
          )
          
          # aux_lev_matrix(human_prep_error[,2], model_prep_error[,2], string_distance)
          # aux_lev_matrix(human_prep[,2], model_prep[,2], string_distance)
          
          
          
          
          # aux_lev_matrix(human_prep[,s$Location], model_prep[,s$Location], string_distance)
          # aux_lev_matrix(model_prep[,s$Location], human_prep[,s$Location], string_distance)
        )
      } else {
        message("Error: string_distance must be one of 'all', 'osa', 'lv', 'dl', 'lcs' or 'jw'. See documentation for more information on each method.")
        break
      }
      # number of words in string nrow(stringr::str_locate_all(human_prep[1,2], " ")[[1]])
    }
    min_lev = as.data.frame(min_lev)
    colnames(min_lev) = c("mean_minimum_levenshtein", "file")
    min_lev$mean_minimum_levenshtein = as.integer(min_lev$mean_minimum_levenshtein)
    
    min_lev = cbind(nchar = nchar(human_prep[,s$Location]), min_lev)
    
    if (rmds){
      if (verbose){
        print(paste0(path, "/", "levenshtein_report_", stringr::str_replace_all(Sys.Date(), "-", "_"), ".csv"))
      }
      write.csv(min_lev, paste0(path, "/", "levenshtein_report_", stringr::str_replace_all(Sys.Date(), "-", "_"), ".csv"), row.names = FALSE)
    }
    
    mean_min_lev = c()
    adjusted_lev = c()
    for (j in common_files){
      mean_min_lev = c(mean_min_lev, mean(min_lev$mean_minimum_levenshtein[min_lev$file == j], na.rm = TRUE))
      adjusted_lev = c(adjusted_lev, mean(min_lev$mean_minimum_levenshtein[min_lev$file == j] / min_lev$nchar[min_lev$file == j], na.rm = TRUE))
    }
    names(mean_min_lev) = common_files
    names(adjusted_lev) = common_files
    
    fn_out[["levenshtein"]] = min_lev
    fn_out[["mean_minimum_levenshtein"]] = mean_min_lev
    fn_out[["adjusted_m_m_levenshtein"]] = adjusted_lev
  }
  
  
  
  # Coordinates ------------------------------------------------------------------
  for (i in common_files){
    out_part <- list(
      non_intersecting = c(0, 0), accuracy = c(),
      sensitivity = c(), precision = c(), f1 = c(),
      confusion_matrix = matrix(c(0, 0, 0, NA),
        nrow = 2, ncol = 2, byrow = TRUE,
        dimnames = list(c("TRUE", "FALSE"), c("TRUE", "FALSE"))
      ),
      data_used = 0, euc_dist = 0, 
      contingency_table = matrix(nrow = 2, ncol = 2, c(0,0,0,0)),
      file_name = i, species_name = "placeholder",
      human_data = human_data, model_data = model_data,
      euc_dist_mean = 0
    )
    
    ## Human -------------------------------------------------------------------
    human_prep = human_data[human_data[,s$File] == i, ] # used to be 4 here and below
    student_species_mentioned = unique(human_prep[,s$Species])
    # Model --------------------------------------------------------------------
    model_prep = model_data[model_data[,s$File] == i, ]
    gpt_species_mentioned = unique(model_prep[,s$Species])
    
    if (nrow(model_prep) == 0 || nrow(human_prep) == 0){
      next
    }

    intersecting_species = intersect(student_species_mentioned, gpt_species_mentioned)
    unique_to_students = student_species_mentioned[!(student_species_mentioned %in% intersecting_species)]
    unique_to_gpt = gpt_species_mentioned[!(gpt_species_mentioned %in% intersecting_species)]

    if ((full_locations == "coordinates" || full_locations == "both")){ # Right now the following excludes some missing locations from "location" requests
      
      # further reduce in size
      if (length(unique_to_students) > 0){
        for (spe in unique_to_students){
          
          not_found = aux_error_frame(human_prep, spe, s, "FN", i)
          
          if (is(not_found, "data.frame")){
            nf_r = nrow(not_found)
          } else {
            nf_r = 1
          }
          
          
          fn_out[["exclusive_to_each_set"]] = rbind(
            fn_out[["exclusive_to_each_set"]],
            c(set = rep("human", nf_r), file = rep(i, nf_r),
              species = rep(spe, nf_r), count = nf_r)
          )
          
          out_global$class_dist_global = rbind(out_global$class_dist_global, not_found)
          
        }
      }
      
      if (length(unique_to_gpt) > 0){
        for (spe in unique_to_gpt){
          not_found = aux_error_frame(model_prep, spe, s, "FP", i)
          
          if (is(not_found, "data.frame")){
            nf_r = nrow(not_found)
          } else {
            nf_r = 1
          }
          
          fn_out[["exclusive_to_each_set"]] = rbind(
            fn_out[["exclusive_to_each_set"]],
            c(set = rep("model", nf_r), file = rep(i, nf_r),
              species = rep(spe, nf_r), count = nf_r)
            )
          
          out_global$class_dist_global = rbind(out_global$class_dist_global, not_found)
          
        }
      }
    }
    
    if (length(intersecting_species) == 0){
      message(paste0("WARNING: NO INTERSECTION FOUND IN FILE ", i, "\n"))
      next
    }
    
    if (full_locations == "coordinates" || full_locations == "both"){
      # Save which species appear only in each set.
      out_part[[1]] = c(sum(!(student_species_mentioned %in% intersecting_species)),
                        sum(!(gpt_species_mentioned %in% intersecting_species))) 
      out_global[[1]] = out_global[[1]] + out_part[[1]]
      
      
      for (j in intersecting_species){
        out_part$species_name = j
        
        ### Add locations here ###
          model_inters = dplyr::distinct(model_prep[model_prep[,s$Species] == j, c(s$Long, s$Lat)  ])
          human_inters = dplyr::distinct(human_prep[human_prep[,s$Species] == j, c(s$Long, s$Lat)  ])
          
          
          dists = c()
          for (loc in 1:nrow(human_inters)){
            dists = c(dists, sapply(model_inters[,1], FUN = function(x){utils::adist(x, human_inters[loc,1])}))
            # Just the average isn't going to have good results when model and human are closer.
            # if we have one match among 5
            # 3
          }
          # mean(dists, na.rm = TRUE)
          
          
          out_part$model_data = model_inters
          out_part$human_data = human_inters
          
          # compare_IUCN needs both sets to have three unique points each. This means
          # we can't use it for some papers. Alternative measures should be provided
          # in the future.
          
          model_inters = model_inters[rowSums(is.na(model_inters)) == 0,]
          human_inters = human_inters[rowSums(is.na(human_inters)) == 0,]
          
          if (nrow(model_inters) == 0 || nrow(human_inters) == 0){
            print("No valid data.")
            next
          }
          
          if (nrow(model_inters[rowSums(is.na(model_inters)) == 0,]) >= 3 && nrow(human_inters[rowSums(is.na(human_inters)) == 0,]) >= 3){
            if (verbose){
              intersect_data = compare_IUCN(
                model_inters[rowSums(is.na(model_inters)) == 0,],
                human_inters[rowSums(is.na(human_inters)) == 0,]
              )
            } else {
              intersect_data = compare_IUCN(
                model_inters[rowSums(is.na(model_inters)) == 0,],
                human_inters[rowSums(is.na(human_inters)) == 0,],
              verbose = FALSE, plots = FALSE)
            }

          }
          
          
          
          
          # Contingency table ------------------------------------------------------
          # -- in -->
          # In model set                       | model points that are in human set
          # human points that are in model set | In human set  
          
          gpt_in_stud = 0
          for (r in 1:nrow(model_inters)){
            gpt_in_stud = gpt_in_stud + any(rowSums(human_inters == as.vector(model_inters[r,])) == 2)
          }
          
          stud_in_gpt = 0
          for (r in 1:nrow(human_inters)){
            stud_in_gpt = stud_in_gpt + any(rowSums(model_inters == as.vector(human_inters[r,])) == 2)
          }
          
          #contingency table
          out_part[["contingency_table"]] <- matrix(c(nrow(model_inters),
                                                      gpt_in_stud, stud_in_gpt, nrow(human_inters)),
                                                    nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("GPT", "Student"), c("GPT", "Student"))
          )
          #confusion matrix
          
          class_dist <- data.frame(
            Lat = c(NULL), Long = c(NULL),
            Classification = c(NULL), MinDist = c(NULL), MeanDist = c(NULL)
          )
          
         
          
          
          
          for (r in 1:nrow(model_inters)) {
            if (any(rowSums(human_inters == as.vector(model_inters[r, ])) == 2)) {
              e_dists <- aux_dist_lite(human_inters, model_inters[r, ]) # EXPAND
              class_dist <- rbind(
                class_dist,
                c(
                  model_inters[r, ],
                  Classification = "TP",
                  MinDist = 0,
                  MeanDist = mean(e_dists)
                )
              )
            }
            if (all(rowSums(human_inters == as.vector(model_inters[r, ])) == 0)) {
              e_dists <- aux_dist_lite(human_inters, model_inters[r, ])
              class_dist <- rbind(
                class_dist,
                c(
                  model_inters[r, ],
                  Classification = "FP",
                  MinDist = min(e_dists),
                  MeanDist = mean(e_dists)
                )
              )
            }
          }
          
          for (r in 1:nrow(human_inters)) {
            if (all(rowSums(model_inters == as.vector(human_inters[r, ])) == 0)) {
              e_dists <- aux_dist_lite(model_inters, human_inters[r, ])
              class_dist <- rbind(
                class_dist,
                c(
                  human_inters[r, ],
                  Classification = "FN",
                  MinDist = min(e_dists),
                  MeanDist = mean(e_dists)
                )
              )
            }
          }
          
          # Mean minimum euclidean distance ----------------------------------------
          for (r in 1:nrow(human_inters)){
            out_part$euc_dist = c(out_part$euc_dist, min(aux_dist_lite(model_inters, human_inters[r,])))
            out_part$euc_dist_mean = c(out_part$euc_dist, mean(aux_dist_lite(model_inters, human_inters[r,])))
          }
          
          
          out_part$confusion_matrix <- matrix(
            c(sum(class_dist$Classification == "TP"),
              sum(class_dist$Classification == "FP"),
              sum(class_dist$Classification == "FN"),
              NA),
            nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("TRUE", "FALSE"), c("TRUE", "FALSE"))
          )
          
          if (nrow(class_dist) != 0){
            out_global$class_dist_global = rbind(
              out_global$class_dist_global,
              cbind(class_dist, file = i)
            )
          }
          
          
          ## Accuracy ---------------------------------------------------------------
          # TP/(TP + FP + FN)
          out_part[[2]] = aux_accuracy(out_part$confusion_matrix)
          out_global[[2]] = c(out_global[[2]], out_part[[2]])
          
          ## Recall -----------------------------------------------------------------
          # TP/(TP + FN)
          out_part[[3]] = aux_recall(out_part$confusion_matrix)
          out_global[[3]] = c(out_global[[3]], out_part[[3]])
          
          ## Precision --------------------------------------------------------------
          # TP/(TP + FP)
          out_part[[4]] = aux_precision(out_part$confusion_matrix)
          out_global[[4]] = c(out_global[[4]], out_part[[4]])
          
          ## F1 ---------------------------------------------------------------------
          out_part[[5]] = aux_f1(out_part$confusion_matrix)
          out_global[[5]] = c(out_global[[5]], out_part[[5]])
          
          output_path = substr(i, 1, nchar(i)-4)
          
          if (output_path == ""){
            output_path = i
          }
          
          if (rmds){
            markdown_args = list(
              input = system.file("rmd", "report_template.Rmd", package = "arete"),
              output_file = paste0(path, "/", output_path, "_", j, ".html"),
              params = list(report_data = out_part, global_extra = out_global[[1]])
            )
            if (verbose){
              do.call(rmarkdown::render, args = markdown_args)
            } else {
              markdown_call = utils::capture.output(suppressMessages(do.call(
                rmarkdown::render, args = markdown_args
              ))) 
            }
          }
          
          if (verbose){
            print(paste0("Confusion matrix for file ", i, ":"))
            print(out_part$confusion_matrix)
          }
          
          fn_out[[paste0(i, "_", j)]] = out_part$confusion_matrix # as.character(i)
          
        }
    }
    
      out_global$euc_dist_mean = c(out_global$euc_dist_mean, mean(out_part$euc_dist_mean))
    }
  
  
  # Global report --------------------------------------------------------------
  if (full_locations == "coordinates" || full_locations == "both"){
    if (rmds){
      markdown_args = list(
        input = system.file("rmd", "report_template_global.Rmd", package = "arete"),
        output_file = paste0(path, "/", "global_report_", stringr::str_replace_all(Sys.Date(), "-", "_"), ".html"),
        params = list(report_data = out_global)
      )
      if (verbose){
        do.call(rmarkdown::render, args = markdown_args)
      } else {
        markdown_call = utils::capture.output(suppressMessages(do.call(
          rmarkdown::render, args = markdown_args
        )))
      }
    }
    fn_out[["global"]] = matrix(
      c(sum(out_global$class_dist_global$Classification == "TP"), sum(out_global$class_dist_global$Classification == "FP"),
        sum(out_global$class_dist_global$Classification == "FN"), NA),
      nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("TRUE", "FALSE"), c("TRUE", "FALSE"))
    )
  }
  return(fn_out)
}
