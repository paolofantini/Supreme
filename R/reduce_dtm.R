# reduce_dtm(dtm, method = c("tfidf", "lognet"), q = list(inf = 0.25, sup = 0.75), target = NULL, export = FALSE)
  #' @title
  #' Reducing the number of columns (terms) of a document-term matrix
  #'
  #' @description
  #' \code{reduce_dtm} reduces the number of columns (terms) of a document-term matrix.
  #'
  #' @details
  #' This function is a wrapper for applying two different methods for dimensionality reduction of a document-term matrix:
  #'
  #' \describe{
  #'   \item{\strong{tfidf}}{It calls the \code{\link{reduce_dtm_tfidf}} function to select suitable columns of an \emph{unlabeled} document-term matrix by deleting
  #'   terms whith tf-idf score out of an user defined range.}
  #'   \item{\strong{lognet}}{It calls the \code{\link{reduce_dtm_lognet}} function to apply \code{lognet}, a logistic classification method from
  #'   package \pkg{glmnet}, to a \emph{labeled} document-term matrix.}
  #' }
  #'
  #' @param dtm a document-term matrix.
  #' @param method the method for selecting the columns.
  #' @param q a list with \code{inf} and \code{sup} quantiles of tf-idf scores distribution. Default are the first and third quartiles.
  #' @param target factor. The labeling variable.
  #' @param export logical. If \code{TRUE} exports the discarded terms, the vocabulary and the returned object to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return
  #' \describe{
  #'   \item{\strong{tfidf}}{A list with the \emph{reduced} \code{dtm} and associated \code{term_tfidf}. Values of quantile thresholds are also returned.}
  #'   \item{\strong{lognet}}{A list with the \emph{reduced} \code{dtm} and train and test misclassification errors \code{err0.train} and \code{err0.test}.
  #'   Confusion matrix is also returned if levels in \code{prediction} and \code{truth} are not different. Otherwise a simple table is returned.}
  #' }
  #'
  #' @export
  #'
  #' @note
  #'
  #' \strong{From Wikipedia}: \href{http://en.wikipedia.org/wiki/Tf-idf}{tfidf}, short for term frequency inverse document frequency,
  #' is a numerical statistic that is intended to reflect how important a word is to a document in a collection or corpus.
  #' It is often used as a weighting factor in information retrieval and text mining.
  #' The \strong{tfidf} value increases proportionally to the number of times a word appears in the document,
  #' but is offset by the frequency of the word in the corpus, which helps to control for the fact that some words are generally
  #' more common than others.
  #'
  #' \code{alpha} and \code{lambda} are the tuning parameters of \strong{lognet} method:
  #' \code{alpha = 1} (default) and the best \code{lambda} value, corresponding to the optimal fit, is associated with the minimum training error.
  #' Dimensionality reduction is performed by selecting only columns (terms) corresponding to \strong{non zero}
  #' \emph{beta} coefficients in the optimal fit of \code{lognet} method.
  #'
  #' \code{discardedTerms.txt} and \code{vocabulary.txt} respectively contain the rejected terms and the vocabulary (i.e. columns) of the \emph{reduced} \code{dtm}.
  #'
  #' @examples
  #' \dontrun{
  #'
  #' ### tfidf method
  #' library(Supreme)
  #' data("dtm")
  #' dtm.tfidf <- reduce_dtm(dtm, method = "tfidf", export = TRUE)
  #'
  #' ### lognet method
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet <- reduce_dtm(dtm, method = "lognet", target = classes, export = TRUE)
  #' }
  #'
reduce_dtm <- function(dtm,
                       method = c("tfidf", "lognet"),
                       q      = list(inf = 0.25, sup = 0.75),
                       target = NULL,
                       export = FALSE) {

  # Check input.
  if (!is(dtm, "DocumentTermMatrix"))
    stop("The argument 'dtm' needs to be a DocumentTermMatrix.")

  method    <- match.arg(method)
  this.call <- match.call()

  if (method == "lognet") {
    if(missing(target))
      stop("Method 'lognet' needs to have a 'target' argument.")
    target <- as.factor(target)
  }

  # Returned object.
  res <- switch(method,
                tfidf  = reduce_dtm_tfidf(dtm, q, export),
                lognet = reduce_dtm_lognet(dtm, target, export))

  res$mycall <- this.call

  return(res)
}
