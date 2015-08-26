# dfciv2corpus(dfciv, export = TRUE)
  #' @title
  #' Creating an ISC corpus from a data frame of class dfciv
  #'
  #' @description
  #' \code{dfciv2corpus} transforms a data frame of class \code{dfciv} to a corpus of Italian Supreme Court
  #' decisions and exports the result to the built-in directory \code{data/corpus}.
  #'
  #' @param dfciv a data frame of class \code{dfciv}.
  #' @param export logical, if \code{TRUE} exports the corpus to txt files. Default is \code{FALSE}.
  #'
  #' @return a ISC \code{corpus} built from a \code{dfciv} data frame with user defined meta attributes (see \code{\link{xml2dfciv}} function).
  #'
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dfciv")
  #' corpus <- dfciv2corpus(dfciv, TRUE)
  #' }
  #'
  #' @import tm
  #'
dfciv2corpus <- function(dfciv, export = FALSE) {

  # Check input.
  if (!is(dfciv, "dfciv"))
    stop("The argument 'dfciv' needs to be a data frame of class 'dfciv'.")

  # Meta data list.
  metaData <- list(content           = "testo",    # default
                   heading           = "materia",  # default
                   Id_doc            = "Id_doc",
                   TipoProvvedimento = "tipoProv",
                   AnnoDecisione     = "annoDec",
                   NumeroDecisione   = "numDec",
                   NumeroSezione     = "numSez",
                   AnnoNrgSic        = "annoNrgSic",
                   NrgSic            = "nrgSic",
                   AnnoProvOriginale = "annoProvOrig",
                   NumProvOriginale  = "numProvOrig",
                   Autorita          = "autorita",
                   Localita          = "localita",
                   Materia           = "materia",
                   Id_materia        = "Id_materia",
                   Dispositivo       = "dispositivo")

  # Ad-hoc reader.
  myReader <- readTabular(mapping = metaData)

  # Corpus.
  corpus <- Corpus(DataframeSource(dfciv), readerControl = list(reader = myReader, language = "it"))

  # Export the corpus.
  if (export) {
    out.dir <- "data/corpus"
    if (!file.exists(out.dir))
      dir.create(out.dir, recursive = TRUE)
    ndocs <- nrow(dfciv)
    writeCorpus(corpus, out.dir, filenames = paste(seq(1, ndocs), ".txt", sep = ""))
  }

  return(corpus)
}
