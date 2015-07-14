# xmlciv2df(path)
  #' @title
  #' From xml docs to dfciv data frame
  #'
  #' @description
  #' \code{xmlciv2df} transforms the original xml documents (decisions in civil matters from Italian Supreme Court) to a \code{dfciv} data frame.
  #'
  #' @details
  #' Each original document has a \strong{xml} tree structure starting at \code{snintegrale} top element
  #' and branching to \code{testo} and \code{dispositivo} leave elements, respectively containing the document text and the case decision.
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
  #' @return \code{dfciv} a data frame of class \code{dfciv} with added columns \emph{Id_doc} and \emph{Idmateria} (Id subject of appealed decision).
  #'
  #' @note
  #' \code{xmlciv2df} transforms the xml tree to a \code{list} which in turn is transformed to a \code{data frame}.
  #' You can obtain the \code{dfciv} data frame loading \href{https://www.dropbox.com/s/t0eiqagdyd67ei7/ISC_Civil_2013.zip?dl=0}{this}
  #' compressed xml file and then running the code in the example.
  #' Data frame \code{dfciv} contains only final decisions belonging to the 10 larger \emph{Idmateria} classes.
  #'
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #'
  #' # Unzip and load in memory the xml input file.
  #' xml_input <- unzip("../ISC_Civil_2013.zip")
  #' dfciv_original <- xmlciv2df(xml_input)
  #'
  #' # Select the subset of final decisions.
  #' dfciv_final <- subset(dfciv_original, tipoProv == "S")
  #'
  #' # Select only the 10 larger classes.
  #' cl <- names(sort(table(dfciv_final$Idmateria), decreasing = TRUE))[1:10]
  #' dfciv <- subset(dfciv_final, Idmateria %in% cl)
  #'
  #' str(dfciv)
  #'
  #' rm(dfciv_original, dfciv_final)
  #' }
  #'
  #' @import XML data.table
  #'
xmlciv2df <- function(path) {

  # Extract the top list elements "snintegrale".
  xml.file <- xmlParse(path)
  xml.file <- xmlToList(xml.file)
  xml.file <- xml.file[names(xml.file) == "snintegrale"]

  # Extract the document attributes and build dataframe dfciv.
  dfciv <- lapply(xml.file, function(x) {

	# Informations about the case decision.
	tipoProv    <- x$.attrs["tipoprov"]
	annoDec     <- x$.attrs["anno"]
  numDec      <- x$.attrs["numdec"]
	numSez      <- x$.attrs["nsz"]
  testo       <- x$tes
  dispositivo <- x$dis

	# Informations about the case filing.
  annoNrgSic   <- x$sic$.attrs["anno_nrg"]
	nrgSic       <- x$sic$.attrs["nrg"]
	annoProvOrig <- x$sic$.attrs["anno_prov"]
  numProvOrig  <- x$sic$.attrs["num_provv"]
	autorita     <- x$sic$.attrs["autorita"]
	localita     <- x$sic$.attrs["localita"]
	materia      <- x$sic$.attrs["materia"]

	data.frame(tipoProv, annoDec, numDec, numSez, testo, dispositivo, annoNrgSic, nrgSic, annoProvOrig, numProvOrig, autorita, localita, materia, stringsAsFactors = FALSE)
  })

  dfciv <- as.data.frame(rbindlist(dfciv))  # rbindlist from pkg data.table

  # Transform document attribute 'materia' to factor attribute.
  dfciv$materia <- as.factor(sub("*", "", dfciv$materia, fixed = T))  # cleaning character * in column 'materia'

  # If you want to transform document attributes to factor attributes (not recommended for problems in the construction of the corpus).
  #dfciv <- cbind(dfciv[c("testo", "dispositivo")], lapply(dfciv[, !names(dfciv) %in% c("testo", "dispositivo")], function(x) as.factor(x)))

  # Convert the encoding of columns 'testo' and 'dispositivo' from utf8 to latin1.
  ICONV <- function(x) iconv(x, "utf8", "latin1")
  dfciv$testo       <- sapply(dfciv["testo"], ICONV)        # for 'testo'
  dfciv$dispositivo <- sapply(dfciv["dispositivo"], ICONV)  # for 'dispositivo

  # Add Id_doc and Idmateria document attributes.
  dfciv <- cbind(ID_doc = seq(1:nrow(dfciv)), dfciv, Idmateria = as.integer(dfciv$materia))

  # Append to dfciv attribute class "dfciv".
  class(dfciv) <- append(class(dfciv), "dfciv")

  return(dfciv)
}
