#' @title
#' Hard document-term matrix
#'
#' @description
#' The input data for the LDA model is a document-term matrix. The rows in this matrix
#' correspond to the documents and the columns to the terms. The entry \emph{f_{i,j}} indicates the
#' frequency of jth term in the ith document. The number of rows is equal to the size of the
#' corpus and the number of columns to the size of the vocabulary.
#'
#' @details
#' \code{dtm} has 15,485 rows and 52,504 columns.
#' It describes the original document-term matrix as obtained by applying the \code{corpus2dtm} function to
#' the original \code{corpus} consisting of 15,485 documents relating to final decisions (sentences) in civil matters
#' delivered by the Italian Supreme Court during the year 2013.
#' Each row of this \emph{hard} matrix represents a document as a simple \emph{bag of words}
#' after removing punctuation, numbers, stopwords and white spaces.
#'
#' @name dtm
NULL
