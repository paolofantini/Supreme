# reduce_dtm(dtm, method = c("tfidf", "lognet", "lognet_cv"), q = list(inf = 0.25, sup = 0.75), classes = NULL,
# lambda = c("lambda.min", "lambda.1se"), SEED = NULL, c_normalize = TRUE, parallel = TRUE, export = FALSE)
  #' @title
  #' Reducing the number of columns (terms) of a document-term matrix
  #'
  #' @description
  #' \code{reduce_dtm} reduces the number of columns (terms) of a document-term matrix.
  #'
  #' @details
  #' This function is a wrapper for applying three different methods for dimensionality reduction of a document-term matrix:
  #'
  #' \describe{
  #'   \item{\strong{tfidf}}{It calls the \code{\link{reduce_dtm_tfidf}} function to select suitable columns of an \emph{unlabeled} document-term matrix by deleting
  #'   terms whith tf-idf score out of an user defined range.}
  #'   \item{\strong{lognet}}{It calls the \code{\link{reduce_dtm_lognet}} function to apply \code{lognet}, a logistic classification method from package \pkg{glmnet},
  #'   to a \emph{labeled} document-term matrix.}
  #'   \item{\strong{lognet_cv}}{It calls the \code{\link{reduce_dtm_lognet_cv}} function to apply the former \code{lognet} method via (parallel) cross-validation.}
  #' }
  #'
  #' @param dtm a document-term matrix in term frequency format.
  #' @param method the method for selecting the columns.
  #' @param q a list with \code{inf} and \code{sup} quantiles of tf-idf scores distribution. Default are the first and third quartiles. Only use for \code{tfidf} method.
  #' @param classes factor. The labeling variable. Only use for \code{lognet} methods.
  #' @param lambda a string with the selection rule of the optimal fit. Only use for \code{lognet} methods.
  #' @param SEED integer, the random seed for selecting train and test set. Only use for \code{lognet} methods.
  #' @param c_normalize a Boolean value indicating whether the \code{dtm} entries should be (cosine) normalized when using the \code{lognet} methods. Default is \code{TRUE}.
  #' @param parallel logical. If \code{TRUE} parallel cross-validation is performed. Default is \code{TRUE}. Only use for \code{lognet_cv} method.
  #' @param export logical. If \code{TRUE} exports the discarded terms, the vocabulary and the returned object to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return
  #' \describe{
  #'   \item{\strong{tfidf}}{A list as in \code{\link{reduce_dtm_tfidf}}.}
  #'   \item{\strong{lognet}}{A list as in \code{\link{reduce_dtm_lognet}}.}
  #'   \item{\strong{lognet_cv}}{A list as in \code{\link{reduce_dtm_lognet_cv}}.}
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
  #' In the optimal fit of the \code{lognet} method the tuning parameters \code{alpha} and \code{lambda}
  #' are respectively set to \code{1} (default) and one out of \code{lambda.min} or \code{lambda.1se}.
  #' The latter follows from the "minimum training error rule" and the former from the more conservative approach of the "one standard error rule".
  #' Full details are given in \emph{"The Elements of Statistical Learnings"} (T. Hastie, R. Tibshirani, J. Friedman) 2nd edition p. 61.
  #' Dimensionality reduction is performed by selecting only columns (terms) corresponding to \strong{non zero}
  #' \emph{beta} coefficients in the optimal fit.
  #'
  #' \code{discardedTerms.txt} and \code{vocabulary.txt} respectively contain the rejected terms and the vocabulary (i.e. columns) of the \emph{reduced} \code{dtm}.
  #'
  #' @examples
  #' \dontrun{
  #'
  #' ### tfidf method
  #' library(Supreme)
  #' data("dtm")
  #' dtm.tfidf <- reduce_dtm(dtm, method = "tfidf")
  #'
  #' ### lognet method
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet <- reduce_dtm(dtm, method = "lognet", classes = classes, SEED = 123)
  #'
  #' ### lognet_cv method
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet.cv <- reduce_dtm(dtm, method = "lognet_cv", classes = classes, lambda = "lambda.1se", SEED = 123)
  #'
  #' }
  #'
reduce_dtm <- function(dtm,
                       method      = c("tfidf", "lognet", "lognet_cv"),
                       q           = list(inf = 0.25, sup = 0.75),
                       classes     = NULL,
                       lambda      = c("lambda.min", "lambda.1se"),
                       SEED        = NULL,
                       c_normalize = TRUE,
                       parallel    = TRUE,
                       export      = FALSE) {

  # Check the input data.
  if (!is(dtm, "DocumentTermMatrix"))
    stop("The argument 'dtm' needs to be a DocumentTermMatrix.")

  method    <- match.arg(method)
  this.call <- match.call()

  if (method %in% c("lognet", "lognet_cv")) {
    if (missing(classes))
      stop("The 'lognet' methods need to have a 'classes' argument.")
  }

  # Returned object.
  res <- switch(method,
                tfidf     = reduce_dtm_tfidf(dtm, q, export),
                lognet    = reduce_dtm_lognet(dtm, classes, SEED, c_normalize, export),
                lognet_cv = reduce_dtm_lognet_cv(dtm, classes, lambda, SEED, c_normalize, parallel, export))

  res$mycall <- this.call

  return(res)
}
