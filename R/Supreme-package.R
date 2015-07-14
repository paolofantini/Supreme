#' @title
#' Make it easier applying LDA topic models to a corpus of Italian Supreme Court decisions
#'
#' @description
#' This package provides tools that make it easier building a corpus of documents starting from the original xml files.
#' It also provides a set of functions for reducing the dimensionality (number of columns) of obtained document-term matrix
#' in both cases of supervised and unsupervised matrix and implements a new strategy for selecting the number of topics
#' based on logistic classification. This strategy can be considered as an alternative to the general criterion of perplexity.
#'
#' @references
#' David M. Blei, Andrew Y. Ng, Michael I. Jordan.
#' Latent Dirichlet Allocation.
#' Journal of Machine Learning Research 3 (2003) 993-1022.
#' \url{http://machinelearning.wustl.edu/mlpapers/paper_files/BleiNJ03.pdf}.
#'
#' @import XML data.table tm slam Matrix caret glmnet topicmodels parallel doParallel foreach iterators
#'
#' @docType package
#'
#' @name Supreme
NULL
