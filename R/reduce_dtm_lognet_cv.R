# reduce_dtm_lognet_cv(dtm, classes, lambda = c("lambda.min", "lambda.1se"), SEED, c_normalize = TRUE, parallel = TRUE, export = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{reduce_dtm_lognet_cv} reduces the number of terms (columns) of a \emph{labeled} document-term matrix.
  #' \code{reduce_dtm_lognet_cv} is called by the \code{\link{reduce_dtm}} function.
  #'
  #' @details
  #' This function fits a logistic classification model via penalized maximum likelihood
  #' by calling the \code{lognet} function from package \pkg{glmnet}.
  #' The regularization path is only computed for the \strong{lasso} penalty at a grid of values
  #' for the regularization parameter \code{lambda}.
  #' If \code{c_normalize = TRUE} (default) the \code{dtm} is passed
  #' for cosine normalization to the \code{\link{wTfIdf}} function.
  #' Reduction of number of terms is performed by selecting only columns corresponding
  #' to the \strong{non zero} \emph{beta} coefficients in the optimal fit.
  #'
  #' @param dtm a document-term matrix in term frequency format.
  #' @param classes factor, the labeling variable.
  #' @param lambda a string with the selection rule of the optimal fit.
  #' @param SEED integer, the random seed for selecting train and test sets.
  #' @param c_normalize logical. If \code{TRUE} \code{dtm} entries are (cosine) normalized. Default is \code{TRUE}.
  #' @param parallel logical. If \code{TRUE} parallel cross-validation is performed. Default is \code{TRUE}.
  #' @param export logical. If \code{TRUE} export the discarded terms, the vocabulary and the returned object
  #' to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return a list with the \emph{reduced} \code{dtm} (in term frequency format),
  #' the IDs of documents belonging to the training set, the \code{glmnet} fit object,
  #' the position of the best lambda, the selected terms by class, and the train and test
  #' misclassification errors \code{err1.train} and \code{err1.test}.
  #' Confusion matrix is also returned.
  #'
  #' @export
  #'
  #' @note
  #' Tuning parameters \code{alpha} and \code{lambda} are respectively set in the optimal fit
  #' to \code{1} (default) and one out of \code{lambda.min} or \code{lambda.1se}.
  #' The latter follows from the "minimum training error rule" and the former
  #' from the more conservative approach of the "one standard error rule".
  #' Full details are given in \emph{"The Elements of Statistical Learnings"}
  #' (T. Hastie, R. Tibshirani, J. Friedman) 2nd edition p. 61.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet.cv <- reduce_dtm_lognet_cv(dtm, classes, lambda = "lambda.1se", SEED = 123)
  #' }
  #'
  #' @import Matrix caret glmnet slam
  #'
reduce_dtm_lognet_cv <- function(dtm, classes, lambda = c("lambda.min", "lambda.1se"), SEED, c_normalize = TRUE, parallel = TRUE, export = FALSE) {

  # Store the input dtm (in term frequency format) for later use.
  dtm_tf <- dtm

  # Force classes into a factor.
  classes <- as.factor(classes)

  # Should be the input dtm (cosine) normalized?
  if (c_normalize)
    dtm <- wTfIdf(dtm)

  # dtm as sparseMatrix (from package Matrix) to be passed to glmnet() function.
  sdtm <- sparseMatrix(dtm$i, dtm$j, x = dtm$v, dimnames = dtm$dimnames)

  # Set random seed only for createDataPartition(): glmnet() doesn't use random seed.
  set.seed(SEED)  # for reproducible inTraining
  inTraining <- as.integer(createDataPartition(classes, p = 0.75, list = FALSE))  # for balancing the size of target classes in training set

  # Training set and testing set.
  train.docs    <- sdtm[inTraining, ]
  test.docs     <- sdtm[-inTraining, ]
  train.classes <- classes[inTraining]
  test.classes  <- classes[-inTraining]

  #-----------------------------------------------------
  ## Method 1: "cv.glmnet" function from package glmnet.
  #-----------------------------------------------------

  # Set up parallel backend for parallel option in cv.glmnet().
  if (parallel) {
    cores <- detectCores()  # maximum number of available cores
    cl    <- makeCluster(cores)
    registerDoParallel(cl)
  }

  ### Fitting model.
  set.seed(SEED)  # for reproducible cross-validation (SEED is the same as in inTraining)
  glmnetFit1 <- cv.glmnet(train.docs, train.classes, family = "multinomial", type.measure = "class", parallel = parallel)

  # Minimum training error.
  err1.train <- min(glmnetFit1$cvm)

  # Choose the penalty parameter lambda.
  lambda <- match.arg(lambda)

  # Prediction values.
  pred1 <- as.integer(predict(glmnetFit1, newx = test.docs, type = "class", s = lambda))  # as.integer for ordered levels in cm1

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred1))) == length(levels(test.classes))) {
    cm1 <- confusionMatrix(table(pred1 = as.factor(pred1), truth = test.classes))
    err1.test <- as.numeric(1 - cm1$overall[1])
  }	else {
      cm1 <- table(pred1 = as.factor(pred1), truth = test.classes)
      err1.test <- 1 - (sum(pred1 == test.classes)/length(test.classes))
    }

  # Stop the cluster.
  if (parallel)
    stopCluster(cl)

  ### Select columns in sdtm with non zero beta coefficients in the "optimal" fit (as picked by the 's' value).

  # The best model is selected using the "minimum training error rule".
  if (lambda == "lambda.min")
    s <- which(glmnetFit1$lambda == glmnetFit1$lambda.min)

  # The (quasi) best model is selected using the "one standard error rule".
  if (lambda == "lambda.1se")
    s <- which(glmnetFit1$lambda == glmnetFit1$lambda.1se)

  nzbc           <- list()
  terms_by_class <- list()  # columns (with non zero beta coefficients) by class

  num.classes <- length(levels(classes))

  for (i in 1:num.classes) {
    bet <- glmnetFit1$glmnet.fit$beta[[i]]
    nzbc[[i]] <- which(bet[, s] != 0)
    terms_by_class[[levels(classes)[i]]] <- nzbc[[i]]
    names(nzbc[[i]]) <- NULL
  }

  nzbc <- unlist(nzbc)
  nzbc <- nzbc[!duplicated(nzbc)]  # no duplicated coefficients

  sdtm.red <- sdtm[, nzbc]

  # dtm.red as simple_triplet_matrix (from package slam) to be passed to LDA() function (package topicmodels).
  dtm.red <- as.simple_triplet_matrix(sdtm.red)

  # Output dtm.red.
  if (c_normalize) {

    # Select from stored dtm_tf only the columns corresponding to terms in c_normalized dtm.red.
    dtm.red <- dtm_tf[, dtm.red$dimnames$Terms]

    # Append to dtm.red suitable class attributes.
    if ("tfidf" %in% class(dtm_tf))
      class(dtm.red) <- append(class(dtm.red), "tfidf_lognet_cv")
    else
      class(dtm.red) <- append(class(dtm.red), "lognet_cv")

  } else {

      # Idem.
      if ("tfidf" %in% class(dtm_tf))
        class(dtm.red) <- c("DocumentTermMatrix", "simple_triplet_matrix", "tfidf_lognet_cv")
      else
        class(dtm.red) <- c("DocumentTermMatrix", "simple_triplet_matrix", "lognet_cv")

      attr(dtm.red, "weighting") <- c("term frequency", "tf")
    }

  # Returned object.
  res <- list(reduced        = dtm.red,
              inTraining     = inTraining,
              glmnetFit1     = glmnetFit1,
              s              = s,
              terms_by_class = terms_by_class,
              cm1            = cm1,
              err1.train     = err1.train,
              err1.test      = err1.test)

  # Save discarded terms, vocabulary and the returned object.
  if (export) {

    dtm.type <- class(dtm.red)[3]

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
