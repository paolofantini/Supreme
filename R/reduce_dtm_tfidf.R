# reduce_dtm_tfidf(dtm, q = list(inf = 0.25, sup = 0.75), export = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{reduce_dtm_tfidf} selects suitable columns of an \emph{unlabeled} document-term matrix
  #' by deleting terms with tf-idf score out of a range defined by \code{inf} and \code{sup}
  #' quantiles of tf-idf scores distribution.
  #' \code{reduce_dtm_tfidf} is called by \code{\link{reduce_dtm}} function.
  #'
  #' @param dtm a document-term matrix.
  #' @param q a list with \code{inf} and \code{sup} quantiles of tf-idf scores distribution. Default are the first and third quartiles.
  #' @param export logical. If \code{TRUE} exports the discarded terms, the vocabulary and the returned object to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return a list with the \emph{reduced} \code{dtm} and associated \code{term_tfidf}.
  #' Values of quantile thresholds are also returned.
  #'
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dtm")
  #' dtm.tfidf <- reduce_dtm_tfidf(dtm, export = TRUE)
  #' }
  #'
  #' @import slam tm
  #'
reduce_dtm_tfidf <- function(dtm, q = list(inf = 0.25, sup = 0.75), export = FALSE) {

  # Compute tfidf scores.
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))

  # Set up inf and sup thresholds.
  thresh <- list(inf = quantile(term_tfidf, q$inf)[[1]], sup = quantile(term_tfidf, q$sup)[[1]])

  # Reduce dtm.
  dtm.red <- dtm[, ((term_tfidf >= thresh$inf) & (term_tfidf <= thresh$sup))]

  # Returned object and attributes class.
  attributes(dtm.red)$weighting <- c("term frequency", "tf")
  class(dtm.red) <- append(class(dtm.red), "tfidf")
  term_tfidf.red <- term_tfidf[((term_tfidf >= thresh$inf) & (term_tfidf <= thresh$sup))]

  res <- list(reduced = dtm.red, term_tfidf = term_tfidf.red, thresholds = thresh)

  # Save discarded terms, vocabulary and returned object.
  if (export) {

    # Output directory.
    dtm.type <- class(dtm.red)[3]
    dtm.out.dir <- paste("data/dtm/", dtm.type, sep = "")
    if (!file.exists(dtm.out.dir))
      dir.create(dtm.out.dir, recursive = TRUE)

    # Output.
    discardedTerms <- colnames(dtm[, ((term_tfidf < thresh$inf) | (term_tfidf > thresh$sup))])
    write(discardedTerms, paste(dtm.out.dir, "/discardedTerms.txt", sep = ""))
	  write(colnames(dtm.red), paste(dtm.out.dir, "/vocabulary.txt", sep = ""))
    save(res, file = paste(dtm.out.dir, "/dtm_", dtm.type, ".RData", sep = ""))
  }

  return(res)
}
