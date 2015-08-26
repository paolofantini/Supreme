# xml2dfciv(path)
  #' @title
  #' From xml docs to dfciv data frame
  #'
  #' @description
  #' \code{xml2dfciv} transforms the original xml documents (decisions in civil matters from Italian Supreme Court)
  #' to a data frame of class \code{dfciv}.
  #'
  #' @details
  #' Each original document has a \strong{xml} tree structure starting at \code{snintegrale} top element
  #' and branching to \code{testo} and \code{dispositivo} leave elements, respectively containing
  #' the content of the document body and the case decision.
  #' Document attributes are:
  #' \describe{
  #'   \item{\code{tipoProv}}{type of decision (D, I, O, S) - S stands for *Sentenza* (final decision)}
  #'   \item{\code{annoDec}}{year of decision}
  #'   \item{\code{numDec}}{number of decision}
  #'   \item{\code{numSez}}{number of ISC section}
  #'   \item{\code{annoNrgSic}}{year of filing lawsuit}
  #'   \item{\code{nrgSic}}{number of filing lawsuit}
  #'   \item{\code{annoProvOrig}}{year of appealed decision}
  #'   \item{\code{numProvOrig}}{number of appealed decision}
  #'   \item{\code{autorita}}{authority of appealed decision}
  #'   \item{\code{localita}}{location of appealed decision}
  #'   \item{\code{materia}}{subject of appealed decision}
  #' }
  #'
  #' @param path character string containing the path of xml input documents.
  #'
  #' @return \code{dfciv} a data frame of class \code{dfciv}.
  #' Each row is a final decision (\code{tipoProv == "S"}) and
  #' the columns contain attributes and \code{testo} and \code{dispositivo} texts for each document.
  #'
  #' @note
  #' \code{xml2dfciv} transforms the xml tree to a \code{list} which in turn is transformed to a \code{data frame}.
  #' You can obtain the \code{dfciv} data frame loading \href{https://www.dropbox.com/s/t0eiqagdyd67ei7/ISC_Civil_2013.zip?dl=0}{this}
  #' compressed xml file and then running the code in the example.
  #'
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #'
  #' # Unzip and load in memory the xml input file.
  #' xml_input <- unzip("../ISC_Civil_2013.zip")
  #' dfciv <- xml2dfciv(xml_input)
  #' str(dfciv)
  #' }
  #'
  #' @import XML data.table
  #'
xml2dfciv <- function(path) {

  # Extract the top list elements "snintegrale".
  xml.file <- xmlParse(path)
  xml.file <- xmlToList(xml.file)
  xml.file <- xml.file[names(xml.file) == "snintegrale"]

  # Extract the document attributes and build the dataframe dfciv.
  FUN <- function(x) {

    # Event 1: empty documents with empty attribute 'dis' in the document.
    if(is.null(x$dis))
    {
      tipoProv     <- NA
      annoDec      <- NA
      numDec       <- NA
      numSez       <- NA
      testo        <- NA
      dispositivo  <- NA
      annoNrgSic   <- NA
      nrgSic       <- NA
      annoProvOrig <- NA
      numProvOrig  <- NA
      autorita     <- NA
      localita     <- NA
      materia      <- NA

    # Event 2: no empty documents.
    } else {

        # Informations about the case decision.
        tipoProv    <- x$.attrs["tipoprov"]
        annoDec     <- x$.attrs["anno"]
        numDec      <- x$.attrs["numdec"]
        numSez      <- x$.attrs["nsz"]
        testo       <- x$tes
        dispositivo <- x$dis

        # Informations about the case filing.
        if(is.list(x$sic))  # event 1: 'list' of document attributes
        {
          annoNrgSic   <- x$sic$.attrs["anno_nrg"]
          nrgSic       <- x$sic$.attrs["nrg"]
          annoProvOrig <- x$sic$.attrs["anno_prov"]
          numProvOrig  <- x$sic$.attrs["num_provv"]
          autorita     <- x$sic$.attrs["autorita"]
          localita     <- x$sic$.attrs["localita"]
          materia      <- x$sic$.attrs["materia"]

        } else {            # event 2: no 'list' of document attributes

          annoNrgSic   <- x$sic["anno_nrg"]
          nrgSic       <- x$sic["nrg"]
          annoProvOrig <- x$sic["anno_prov"]
          numProvOrig  <- x$sic["num_provv"]
          autorita     <- x$sic["autorita"]
          localita     <- x$sic["localita"]
          materia      <- x$sic["materia"]
          }
       }

    data.frame(tipoProv, annoDec, numDec, numSez, testo, dispositivo, annoNrgSic, nrgSic, annoProvOrig, numProvOrig, autorita, localita, materia, stringsAsFactors = FALSE)

  }

  # Apply FUN to each document.
  dfciv <- lapply(xml.file, FUN)

  # Build data frame dfciv...
  dfciv <- as.data.frame(rbindlist(dfciv))  # rbindlist from pkg data.table

  # ...and keep only the final decisions.
  dfciv <- subset(dfciv, tipoProv == "S")

  # Transform the document attribute 'materia' to factor attribute.
  dfciv$materia <- as.factor(sub("*", "", dfciv$materia, fixed = T))  # character * in 'materia' is deleted

  # Convert the encoding of columns 'testo' and 'dispositivo' from utf8 to latin1.
  ICONV <- function(x) iconv(x, "utf8", "latin1")
  dfciv$testo       <- sapply(dfciv["testo"], ICONV)        # for 'testo'
  dfciv$dispositivo <- sapply(dfciv["dispositivo"], ICONV)  # for 'dispositivo

  # Append to dfciv attribute class "dfciv".
  class(dfciv) <- append(class(dfciv), "dfciv")

  return(dfciv)
}
