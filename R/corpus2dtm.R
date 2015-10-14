# corpus2dtm(corpus, stopwords)
  #' @title
  #' From ISC corpus to a Document-Term Matrix
  #'
  #' @description
  #' \code{corpus2dtm} transforms a corpus of decisions from Italian Supreme Court to a document term matrix.
  #'
  #' @param corpus a corpus of decisions from Italian Supreme Court.
  #' @param stopwords a character vector of stopwords.
  #'
  #' @return \code{dtm} a \emph{raw} document-term matrix with minimum term length 3 and terms appearing at least in 2 documents.
  #'
  #' @export
  #'
  #' @note
  #' Basic text cleaning steps build a \emph{raw} \code{dtm} by selecting only terms (columns)
  #' corresponding to a suitable vocabulary. Typically, this involves converting tokens to lower-case,
  #' removing punctuation characters, removing numbers, stemming, removing stop-words and selecting terms
  #' with a length above a certain minimum and occurring at least in a minimum number of documents.
  #' Package \pkg{tm version >= 0.6} required.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("corpus")
  #' data("italianStopWords")  # for removing italian stop words
  #' dtm <- corpus2dtm(corpus, italianStopWords)
  #' }
  #'
  #' @import tm slam
  #'
corpus2dtm <- function(corpus, stopwords) {

  # Basic text cleaning.
  dtmCorpus <- corpus
  removePunctuation <- function(x) gsub("(['?\n<U+202F><U+2009>]|[[:punct:]]|[[:space:]]|[[:cntrl:]])+", " ", x)
  removeStopWords   <- function(x) removeWords(x, stopwords)

  # List of tm transformations (bottom-up order).
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(tolower))
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(removeNumbers))
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(removePunctuation))
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(removeStopWords))
  dtmCorpus <- tm_map(dtmCorpus, content_transformer(stripWhitespace))

  # dtm: keep only terms with minimum length 3 and appearing at least in 2 documents.
  dtm <- DocumentTermMatrix(dtmCorpus, control = list(wordLengths = c(3, Inf), bounds = list(global = c(2, Inf))))

  # Append to dtm attribute class "raw".
  class(dtm) <- append(class(dtm), "raw")

  return(dtm)
}
