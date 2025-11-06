#' Example data packaged with gecko
#' @description Load data included in the package. This includes \strong{holzapfelae}, a txt file of a paper describing Araneus holzapfelae (Dippenaar-Schoeman & Foord, 2020),
#'  \strong{annotations}, two files of annotated data of the same paper describing two species, Sinlathrobium assingi and Sinlathrobium chenzhilini (Chen \emph{et al.} 2024) and \strong{annotations-highlights} a version of the same two files reduced to just those sections containing annotated data.
#' @param data character. String of one of the data names mentioned in the description, e.g.: \code{"holzapfelae"}.
#' If \code{NULL}, the example files will be returned.
#' @return depending on \code{data} either a character (\code{"holzapfelae"}), a WebAnnoTSV object (\code{"holzapfelae-extract"}) or a list of WebAnnoTSV (\code{"annotations"}, \code{"annotations-highlights"}).
#' @examples
#' arete_data()
#' arete_data("holzapfelae")
#' @references Dippenaar-Schoeman, A. S. & Foord, S. H. (2020a). First record of the orb-web spider Araneus holzapfelae Lessert, 1936 from South Africa (Arachnida: Araneidae). Sansa News 35: 19-21.
#' Chen, X., Ye, J.-P. and Peng, Z. (2024) ‘Two new species and additional records of Sinlathrobium Assing (Coleoptera, Staphylinidae, Paederinae) from southern China’, ZooKeys, 1218, pp. 25–33. doi:10.3897/zookeys.1218.128973. 
#' @source This function is inspired by \code{\link[palmerpenguins:path_to_file]{palmerpanguins::path_to_file()}}
#' which in turn is based on \code{\link[readxl:readxl_example]{readxl::readxl_example()}}.
#' @export
arete_data <- function(data = NULL) {
  if (is.null(data)) {
    return(
      c(
        "holzapfelae", "holzapfelae-extract", "annotations", "annotations-highlights"
      )
    )
  } else {
    if (data == "holzapfelae"){
      path = system.file(paste0("extdata/holzapfelae.txt"), package = "arete")
      out = path
    } else if (data == "holzapfelae-extract"){
      path = system.file(paste0("extdata/holzapfelae_extract.csv"), package = "arete")
      out = read.csv(path)
    } else if (data == "annotations"){
      path = list(
        webanno_open(system.file(paste0("extdata/insecta_annot_1.tsv"), package = "arete")),
        webanno_open(system.file(paste0("extdata/insecta_annot_2.tsv"), package = "arete"))
      )
      out = path
    } else if (data == "annotations-highlights"){
      path = list(
        webanno_open(system.file(paste0("extdata/insecta_annot_1.tsv"), package = "arete"), cut_to_content = TRUE),
        webanno_open(system.file(paste0("extdata/insecta_annot_2.tsv"), package = "arete"), cut_to_content = TRUE)
      )
      out = path
    } else {
      warning("Invalid data name. Run arete_data() for a full list of options.")
      return(NULL)
    }
    return(out)
  }
  
}


GPT_prompt = function(){
  return("Read the following document then reply only a table consisting of three columns, Species, Location, and Coordinates containing new geographic data and the in-text coordinates of species mentioned in the following document. Include locations as they are written in the text, aside from the special character: |. Include locations without coordinates. Do not include species with neither location nor coordinate data. Do not include those in scientific articles or outside references:")
} 
