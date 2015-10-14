# reduce_dtm_lognet(dtm, classes, export = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{reduce_dtm_lognet} reduces the number of terms (columns) of a \emph{labeled} document-term matrix.
  #' \code{reduce_dtm_lognet} is called by the \code{\link{reduce_dtm}} function.
  #'
  #' @details
  #' This function applies \code{lognet} method, a logistic classification method from package
  #' \pkg{glmnet}, to a \emph{labeled} document-term matrix.
  #' Reduction of number of terms is performed by selecting only columns corresponding
  #' to the \strong{non zero} \emph{beta} coefficients in the optimal fit.
  #'
  #' @param dtm a document-term matrix.
  #' @param classes factor, the labeling variable.
  #' @param export logical. If \code{TRUE} export the discarded terms, the vocabulary and the returned object to the built-in directory \code{data/dtm}. Default is \code{FALSE}.
  #'
  #' @return a list with the \emph{reduced} \code{dtm} and train and test misclassification errors
  #' \code{err0.train} and \code{err0.test}. Confusion matrix is also returned.
  #'
  #' @export
  #'
  #' @note
  #' \code{alpha} and \code{lambda} are tuning parameters of the \strong{lognet} method:
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
reduce_dtm_lognet <- function(dtm, classes, export = FALSE) {

  # Force classes into a factor.
  classes <- as.factor(classes)

  # dtm as sparseMatrix (from package Matrix) to be passed to glmnet() function.
  sdtm <- sparseMatrix(dtm$i, dtm$j, x = dtm$v, dimnames = dtm$dimnames)

  # Set random seed only for createDataPartition(): glmnet() doesn't use random seed.
  set.seed(2010)  # for reproducible inTraining
  inTraining    <- as.integer(createDataPartition(classes, p = 0.75, list = FALSE))  # for balancing the size of target classes in training set

  # Training set and testing set.
  train.docs    <- sdtm[inTraining, ]
  test.docs     <- sdtm[-inTraining, ]
  train.classes <- classes[inTraining]
  test.classes  <- classes[-inTraining]

  # Fit lognet model (Method 0): tuning parameters alpha (default = 1) and lambda.
  glmnetFit0 <- glmnet(train.docs, train.classes, family = "multinomial")
  pred       <- predict(glmnetFit0, newx = train.docs, type="class")  # pred ...to choose the best lambda value

  # Choose lambda value corresponding to the minimum training error (the best lambda value).
  s <- which.max(apply(pred == train.classes, 2, mean))

  # Minimum training error.
  err0.train <- 1 - max(apply(pred == train.classes, 2, mean))

  # Prediction with the best lambda value (as picked by 's' value).
  pred0 <- as.integer(predict(glmnetFit0, newx = test.docs, type="class", s = glmnetFit0$lambda[s[[1]]]))  # as.integer for ordered levels in cmo

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred0))) == length(levels(test.classes))) {
    cm0 <- confusionMatrix(table(pred0 = as.factor(pred0), truth = test.classes))
    err0.test <- as.numeric(1 - cm0$overall[1])
  }	else {
      cm0 <- table(pred0 = as.factor(pred0), truth = test.classes)
      err0.test <- 1 - (sum(pred0 == test.classes)/length(test.classes))
    }

  # Select columns in sdtm corresponding to the non zero beta coefficients in the optimal fit (as provided by 's' value).
  nzbc <- list()
  num.classes <- length(levels(classes))

  for(i in 1:num.classes) {
    bet              <- glmnetFit0$beta[[i]]
    nzbc[[i]]        <- which(bet[, s[[1]]] != 0)
    names(nzbc[[i]]) <- NULL
  }

  nzbc <- unlist(nzbc)
  nzbc <- nzbc[!duplicated(nzbc)]  # no duplicated coefficients

  sdtm.red <- sdtm[, nzbc]

  # dtm.red as simple_triplet_matrix (from package slam) to be passed to LDA() function (package topicmodels).
  dtm.red <- as.simple_triplet_matrix(sdtm.red)

  # Append to dtm.red suitable class attributes.
  if ("tfidf" %in% class(dtm)) {
    class(dtm.red) <- append(class(dtm.red), c("DocumentTermMatrix", "tfidf_lognet"))
  } else {
      class(dtm.red) <- append(class(dtm.red), c("DocumentTermMatrix", "lognet"))
    }

  attributes(dtm.red)$weighting <- c("term frequency", "tf")

  # Returned object.
  res <- list(reduced = dtm.red, cm0 = cm0, err0.train = err0.train, err0.test = err0.test)

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
