% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/xml2dfciv.R
\name{xml2dfciv}
\alias{xml2dfciv}
\title{From xml docs to dfciv data frame}
\usage{
xml2dfciv(path)
}
\arguments{
\item{path}{character string containing the path of xml input documents.}
}
\value{
\code{dfciv} a data frame of class \code{dfciv}.
Each row is a final decision (\code{tipoProv == "S"}) and
the columns contain attributes and \code{testo} and \code{dispositivo} texts for each document.
}
\description{
\code{xml2dfciv} transforms the original xml documents (decisions in civil matters from Italian Supreme Court)
to a data frame of class \code{dfciv}.
}
\details{
Each original document has a \strong{xml} tree structure starting at \code{snintegrale} top element
and branching to \code{testo} and \code{dispositivo} leave elements, respectively containing
the content of the document body and the case decision.
Document attributes are:
\describe{
  \item{\code{tipoProv}}{type of decision (D, I, O, S) - S stands for *Sentenza* (final decision)}
  \item{\code{annoDec}}{year of decision}
  \item{\code{numDec}}{number of decision}
  \item{\code{numSez}}{number of ISC section}
  \item{\code{annoNrgSic}}{year of filing lawsuit}
  \item{\code{nrgSic}}{number of filing lawsuit}
  \item{\code{annoProvOrig}}{year of appealed decision}
  \item{\code{numProvOrig}}{number of appealed decision}
  \item{\code{autorita}}{authority of appealed decision}
  \item{\code{localita}}{location of appealed decision}
  \item{\code{materia}}{subject of appealed decision}
}
}
\note{
\code{xml2dfciv} transforms the xml tree to a \code{list} which in turn is transformed to a \code{data frame}.
You can obtain the \code{dfciv} data frame loading \href{https://www.dropbox.com/s/t0eiqagdyd67ei7/ISC_Civil_2013.zip?dl=0}{this}
compressed xml file and then running the code in the example.
}
\examples{
\dontrun{
library(Supreme)

# Unzip and load in memory the xml input file.
xml_input <- unzip("../ISC_Civil_2013.zip")
dfciv <- xml2dfciv(xml_input)
str(dfciv)
}
}

