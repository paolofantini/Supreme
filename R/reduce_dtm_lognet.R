# reduce_dtm_lognet(dtm, target, export = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{reduce_dtm_lognet} selects suitable columns of a \emph{labeled} document-term matrix
  #' by applying the \code{lognet} method from package \pkg{glmnet}.
  #' \code{reduce_dtm_lognet} is called by \code{\link{reduce_dtm}} function.
  #'
  #' @details
  #' This function applies \code{lognet}, a logistic classification method from package
  #' \pkg{glmnet}, to a \emph{labeled} document-term matrix.
  #' Reduction of number of terms is performed by selecting only columns (terms) corresponding
  #' to \strong{non zero} \emph{beta} coefficients in the optimal fit of \code{lognet} method
  #' from \code{glmnet()} function in package \pkg{glmnet}.
  #'
  #' @param dtm a document-term matrix.
  #' @param target factor. The labeling variable.
  #' @param export logical. If \code{TRUE} exports the discarded terms, the vocabulary and the returned object to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return a list with the \emph{reduced} \code{dtm} and train and test misclassification errors
  #' \code{err0.train} and \code{err0.test}.
  #' Confusion matrix is also returned if levels in \code{prediction} and \code{truth} are not different.
  #' Otherwise a simple table is returned.
  #'
  #' @export
  #'
  #' @note
  #' \code{alpha} and \code{lambda} are the tuning parameters of \strong{lognet} method:
  #' \code{alpha = 1} (default) and the best \code{lambda} value, corresponding to the
  #' optimal fit, is associated with the minimum training error.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet <- reduce_dtm_lognet(dtm, classes, TRUE)
  #' }
  #'
  #' @import Matrix caret glmnet slam
  #'
reduce_dtm_lognet <- function(dtm, target, export = FALSE) {

  # dtm as sparseMatrix (from package Matrix) for passing to glmnet() function.
  sdtm <- sparseMatrix(dtm$i, dtm$j, x = dtm$v, dimnames = dtm$dimnames)

  # Train and test set for using in glmnet() function.
  # Set random seed only for createDataPartition(): glmnet() doesn't use random seed.
  set.seed(2010)
  inTraining   <- createDataPartition(target, p = 0.75, list = FALSE)  # for balancing the size of target classes in training set
  train.docs   <- sdtm[as.numeric(inTraining), ]
  test.docs    <- sdtm[- as.numeric(inTraining), ]
  train.target <- target[as.numeric(inTraining)]
  test.target  <- target[- as.numeric(inTraining)]

  # Fit lognet model (Method 0): tuning parameters alpha (default = 1) and lambda.
  glmnetFit0 <- glmnet(train.docs, train.target, family = "multinomial")
  prd        <- predict(glmnetFit0, newx = train.docs, type="class")  # prd ...to choose best lambda value

  # Choose lambda value corresponding to minimum training error (the best lambda value).
  s <- which.max(apply(prd == train.target, 2, mean))

  # Minimum training error.
  err0.train <- 1 - max(apply(prd == train.target, 2, mean))

  # Prediction with best lambda value (as picked by 's' value).
  pred0 <- predict(glmnetFit0, newx = test.docs, type="class", s = glmnetFit0$lambda[s[[1]]])

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred0))) == length(levels(as.factor(test.target)))) {
    cm0 <- confusionMatrix(table(pred0 = as.factor(pred0), truth = as.factor(test.target)))
	err0.test <- as.numeric(1 - cm0$overall[1])
  }	else {
      cm0 <- table(pred0 = as.factor(pred0), truth = as.factor(test.target))
	  err0.test <- (sum(as.integer(pred0) == testing$target)/length(testing$target))
    }

  # Select columns in sdtm corresponding to non zero beta coefficients in model glmnetFit0 (as picked by 's' value).
  nzbc <- list()
  num.classes <- length(levels(as.factor(target)))

  for(i in 1:num.classes) {
    bet              <- glmnetFit0$beta[[i]]
    nzbc[[i]]        <- which(bet[, s[[1]]] != 0)
    names(nzbc[[i]]) <- NULL
  }

  nzbc <- unlist(nzbc)
  nzbc <- nzbc[!duplicated(nzbc)]  # no duplicated coefficients

  sdtm.red <- sdtm[, nzbc]

  # dtm.red as simple_triplet_matrix (from package slam) for passing to LDAmodel() function.
  dtm.red <- as.simple_triplet_matrix(sdtm.red)

  # Append to dtm.red suitable attribute class.
  if ("tfidf" %in% class(dtm)) {
    class(dtm.red) <- append(class(dtm.red), c("DocumentTermMatrix", "tfidf_lognet"))
  } else {
    class(dtm.red) <- append(class(dtm.red), c("DocumentTermMatrix", "lognet"))
    }

  attributes(dtm.red)$weighting <- c("term frequency", "tf")

  # Returned object.
  res <- list(reduced = dtm.red, cm0 = cm0, err0.train = err0.train, err0.test = err0.test)

  # Save discarded terms, vocabulary and returned object.
  if (export) {

    # Check dtm class type.
    if ("tfidf" %in% class(dtm)) {
      dtm.type <- paste(class(dtm)[3], "_", class(dtm.red)[3], sep = "")
    } else {
        dtm.type <- class(dtm.red)[3]
      }

    # Output directory.
    dtm.out.dir <- paste("data/dtm/", dtm.type, sep = "")
    if (!file.exists(dtm.out.dir))
      dir.create(dtm.out.dir, recursive = TRUE)

    # Output.
    discardedTerms <- colnames(sdtm[, -nzbc])
    write(discardedTerms, paste(dtm.out.dir, "/discardedTerms.txt", sep = ""))
    write(colnames(dtm.red), paste(dtm.out.dir, "/vocabulary.txt", sep = ""))
    save(res, file = paste(dtm.out.dir, "/dtm_", dtm.type, ".RData", sep = ""))
  }

  return(res)
}
