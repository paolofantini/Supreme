# wTfIdf(dtm, normalize = TRUE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' Weight a document-term matrix by term frequency - inverse document frequency.
  #'
  #' @details
  #' In order for the weights to fall in the \code{[0, 1]} interval and
  #' for the documents to be represented by vectors of equal length,
  #' the weights are normalized by cosine normalization.
  #'
  #' @param dtm a document-term matrix in term frequency format.
  #' @param normalize a Boolean value indicating whether the tf-idf scores should be normalized. Default is \code{TRUE}.
  #'
  #' @return The weighted document-term matrix.
  #'
  #' @export
  #'
  #' @note
  #' This function is slightly different from \link{weightTfIdf} in the package \pkg{tm}
  #' because of the different normalization approach.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dtm")
  #' dtm_normalized <- wTfIdf(dtm, normalize = TRUE)
  #' }
  #'
  #' @import tm slam
  #'
wTfIdf <- function(dtm, normalize = TRUE) {

  # Check the input matrix.
  isDTM <- inherits(dtm, "DocumentTermMatrix")
  if (isDTM)
    m <- t(dtm)

  # Compute the term document frequency (number of documents in which term occurs).
  rs <- row_sums(m > 0)

  # Are there any terms occurring in no document?
  if (any(rs == 0))
    warning("unreferenced term(s): ", paste(Terms(m)[rs == 0], collapse = " "))

  # Compute the tf-idf scores.
  lnrs <- log2(nDocs(m)/rs)
  lnrs[!is.finite(lnrs)] <- 0
  m <- m * lnrs  # tf-idf scores (the m to the right is the term frequency)

  # Compute the cosine normalization.
  if (normalize) {
    cs <- col_sums(m ^ 2)
    # Are there any empty docs?
    if (any(cs == 0))
      warning("empty document(s): ", paste(Docs(m)[cs == 0], collapse = " "))
    names(cs) <- seq_len(nDocs(m))
    m$v <- as.numeric(m$v/sqrt(cs[m$j]))
  }

  # Update the weighting attributes.
  attr(m, "weighting") <- c(sprintf("%s%s", "term frequency - inverse document frequency",
                                    if (normalize) " (normalized)" else ""), "tf-idf")
  if (isDTM)
    t(m)
  else m
}
