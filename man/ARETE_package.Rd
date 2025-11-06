\name{arete_package}

\alias{arete_package}

\alias{arete}

\docType{package}

\title{Summary of methods in the arete package}

\description{

Package \code{arete} seeks to provide easy Automated REtrieval of species data from TExt. To do this it processes user-supplied text, breaks it into API requests to LLM services and processes the output
through a variety of machine learning and rule-based methods to deliever species data in a machine-readable format, ready for analysis. For a short and sweet use case of arete, try our \code{vignette(package = "arete")}.
In broad terms, functions in arete can be placed in 6 different categories: 

---------------------------------------------------------------------------------------------------------------------
}

\section{1. Setting up arete}{

  \tabular{ll}{
    \code{\link{arete_setup}}\tab Define a default virtual environment and install external dependencies\cr
    \code{\link{install_python_packages}} \tab Install or update python dependencies after setup\cr
    \code{\link{install_OCR_packages}} \tab Install or update the dependencies for our optional OCR utilities\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{2. Prepare annotation data}{

  \tabular{ll}{
    \code{\link{labels}}\tab Extract the labels and relations in a Webanno TSV 3.3 file to an easy, machine readable format ready for machine learning projects\cr
    \code{\link{labels_unique}} \tab Extract all unique labels and relations in a Webanno TSV 3.3 file\cr
    \code{\link{webanno_open}} \tab Read the contents of a WebAnno TSV 3.3 file and create a \code{webanno} object, a format for annotated text containing named entities and relations\cr
    \code{\link{webanno_summary}} \tab Summarize the contents of a group of WebAnno tsv files by counting labels and relations present\cr
    \code{\link{create_training_data}} \tab Open files with text and annotated data and build training data for large language models in a variety of formats\cr
    \code{\link{file_comparison}} \tab Detect differences between two WebAnno files of the same text for annotation monitoring\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{3. Prepare text data}{

  \tabular{ll}{
    \code{\link{process_document}} \tab Extract text embedded in a \code{.pdf} or \code{.txt} file and process it so it can be safely used APIs of LLM\cr
    \code{\link{OCR_document}} Optional utilities based on \code{tesseract} and \code{nougatOCR} \tab \cr
    \code{\link{check_lang}}\tab Check if a given string is mostly (75\% of the document) in English\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{4. Clean data}{

  \tabular{ll}{
    \code{\link{string_to_coords}}\tab Rule-based conversion of character strings containing geographic coordinates to sets of numeric values\cr
    \code{\link{process_species_names}} \tab This function standardizes species names and fixes a number of common typos and mistakes that commonly occur due to OCR\cr 

  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{5. Finetune and extract data}{

  \tabular{ll}{
    \code{\link{get_geodata}} \tab Call a Large Language Model (LLM) to extract species geographic data\cr
    \code{\link{gazetteer}} \tab Extract geographic coordinates from strings containing location names, using an online index\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{6. Evaluate model performance}{

  \tabular{ll}{
    \code{\link{performance_report}}\tab Produce a detailed report on the discrepancies between data extracted by a LLM and human annotated data.\cr
    \code{\link{compare_IUCN}} \tab Calculate EOO for two sets of coordinates for a practical assessment of data proximity\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}


\section{Contributors}{
The methods and functions in this package were written by Vasco Branco, with code contribuitions by Vaughn Shirey, Thomas Merrien. Code revision by Pedro Cardoso.
}


\keyword{ package }
\keyword{ large language models }