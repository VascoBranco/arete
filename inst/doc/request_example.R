## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = TRUE, results = "hide"-------------------------------------
library(arete)

## ----extraction, eval=FALSE---------------------------------------------------
#  geotest = arete::get_geodata(
#    path = file_path,
#    user_key = list(key = "your key here!", premium = TRUE),
#    model = "gpt-4o",
#    outpath = "/your/path/here"
#    )

## ----pre-run------------------------------------------------------------------
geotest = arete::arete_data("holzapfelae-extract")

kableExtra::kable(geotest)

## ----coords-------------------------------------------------------------------
geocoords = string_to_coords(geotest$Coordinates)

kableExtra::kable(geocoords)

## ----species_1----------------------------------------------------------------
geonames = data.frame(
  human_names = geotest[geotest$ID == 1 & geotest$Type == "Ground truth", "Species"],
  model_names = geotest[geotest$ID == 1 & geotest$Type == "Model", "Species"]
  )

mismatch = c(1:nrow(geonames))[geonames$human_names != geonames$model_names]
geonames = kableExtra::kable(geonames)
geonames = kableExtra::row_spec(geonames, mismatch, color = "red")

geonames

## ----species_2----------------------------------------------------------------
geotest$Species = process_species_names(geotest$Species)

geonames = data.frame(
  human_names = geotest[geotest$ID == 1 & geotest$Type == "Ground truth", "Species"],
  model_names = geotest[geotest$ID == 1 & geotest$Type == "Model", "Species"]
  )
geonames = kableExtra::kable(geonames)
geonames = kableExtra::row_spec(geonames, mismatch, color = "green")

geonames

## ----outliers-----------------------------------------------------------------
geoout = gecko::outliers.detect(geocoords[2:1])

kableExtra::kable(geoout)

## ----reports_1----------------------------------------------------------------
geotest = cbind(geotest[,1:2], geocoords, geotest[,4:5])

geotest = list(
  GT = geotest[geotest$Type == "Ground truth", 1:5],
  MD = geotest[geotest$Type == "Model", 1:5]
)

geo_report = performance_report(geotest$GT, geotest$MD, full_locations = "both", verbose = FALSE, rmds = FALSE)

## ----reports_2----------------------------------------------------------------
geo_report

