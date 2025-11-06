#' WebAnno TSV v3.3 class creator.
#' @description Crop raster layers to minimum size possible and uniformize NA values across layers.
webanno_creator <- setClass(
  "WebAnnoTSV",
  slots = c(
    text = "list",
    content = "list"
  )
)