# compClass(predictors, classes, inTraining, train.glmnet = FALSE, cv.parallel = FALSE, train.parallel = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{compClass} fits a logistic classification model (from packages \pkg{glmnet} and \pkg{caret})
  #' to the posterior topic \code{compositions} of each document as trained by Latent Dirichelet Allocation.
  #' A \code{classes} variable is used as a classification variable.
  #' \code{compClass} is called by the \code{\link{mcLDA}} function.
  #'
  #' @details
  #' This function recognizes the compositional nature of the predictors
  #' and applies the \emph{principle of working on coordinates} when facing with compositional data.
  #' Isometric log-ratio transformed versions of the predictors
  #' (by the \code{ilr} function from package \pkg{compositions}) are provided as input to the classification model.
  #'
  #' We considered three different methods.
  #'
  #' \emph{Method0} and \emph{Method1} are respectively built on the functions \code{glmnet} and \code{cv.glmnet} from package \pkg{glmnet}.
  #' \emph{Method2} refers to the function \code{train.glmnet} from package \pkg{caret}.
  #' \emph{Method0} tends to overfit the training set.
  #' \emph{Method1} and \emph{Method2} try to avoid overfitting problems using cross-validation.
  #' \emph{Method2} uses repeated cross-validation and is more stable than \emph{Method1} but much more time-consuming (parallel computation is allowed).
  #'
  #' @param predictors the matrix of predictors, i. e. the posterior topic compositions of each document.
  #' @param classes factor, the classification variable.
  #' @param inTraining the numeric ids of documents belonging to the training set.
  #' @param train.glmnet logical. If \code{TRUE} run \code{train.glmnet} function from package \pkg{caret}. Default is \code{FALSE}.
  #' @param cv.parallel logical. If \code{TRUE} parallel computation is used in \emph{Method1} with the maximum number of available cores. Default is \code{FALSE}.
  #' @param train.parallel logical. If \code{TRUE} parallel computation is used in \emph{Method2} with the maximum number of available cores. Default is \code{FALSE}.
  #'
  #' @return \code{err} list of misclassification errors (error = 1 - Accuracy) and confusion matrices (from package \pkg{caret}):
  #' \item{e0.train}{train error from method "glmnet"}
  #' \item{e1.train}{train error from method "cv.glmnet"}
  #' \item{e2.train}{train error from method "train.glmnet"}
  #' \item{e0.test}{test error from method "predict.glmnet"}
  #' \item{e1.test}{test error from method "predict.cv.glmnet"}
  #' \item{e2.test}{test error from method "predict.train.glmnet"}
  #' \item{cm0}{confusion matrix for method "glmnet"}
  #' \item{cm1}{confusion matrix for method "cv.glmnet"}
  #' \item{cm1}{confusion matrix for method "train.glmnet"}
  #'
  #' @export
  #'
  #' @note
  #' Tuning parameters are \code{alpha} and \code{lambda}.
  #' \emph{Method0} and \emph{Method1} pick no value for \code{alpha} and it remains at default value \code{alpha = 1}.
  #' \emph{Method2} selects values for \code{alpha} and \code{lambda} using the tuning parameter grid defined by
  #' \code{expand.grid(alpha = seq(0.1, 1, 0.1), lambda = glmnetFit0$lambda)}.
  #' More details can be found \href{http://caret.r-forge.r-project.org/training.html}{here}.
  #' In \emph{Method1} the best model is selected using the "one standard error rule":
  #' default best value of the penalty parameter \code{lambda} is \code{s = "lambda.1se"}, stored on the \code{cv.glmnet} object.
  #' Such a rule takes a conservative approach. Alternatively \code{s = "lambda.min"} can be used.
  #' Full details are given in \emph{"The Elements of Statistical Learnings"} (T. Hastie, R. Tibshirani, J. Friedman) 2nd edition p. 61.
  #' Insights on compositions and their use in R can be found in \emph{"Analyzing compositional data with R"}
  #' (K. Gerald van den Boogaart, Raimon Tolosana-Delgado) Springer-Verlag 2013.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' library(topicmodels)
  #'
  #' # Input data.
  #' data("dtm")
  #' data("classes")
  #'
  #' # Reduced dtm.lognet
  #' dtm.lognet <- reduce_dtm(dtm, method = "lognet", classes = classes, export = TRUE)
  #'
  #' # Run a 35-topic model over the reduced dtm.lognet and compute the topic posteriors.
  #' ldaVEM.mod <- LDA(dtm.lognet$reduced, k = 35, method = "VEM", control = list(seed = 2014))
  #' topic.posteriors <- posterior(ldaVEM.mod)$topics
  #'
  #' # Misclassification errors.
  #' set.seed(2010)  # for inTraining reproducibility
  #' inTraining <- caret::createDataPartition(as.factor(classes), p = 0.75, list = FALSE)  # for balancing the size of target classes in training set
  #' mis.error <- compClass(topic.posteriors, classes, inTraining)
  #' }
  #'
  #' @import compositions caret glmnet doParallel
  #'
compClass <- function(predictors, classes, inTraining, train.glmnet = FALSE, cv.parallel = FALSE, train.parallel = FALSE) {

  # Force classes into a factor.
  classes <- as.factor(classes)

  # Compute the isometric log-ratio transforms of the predictor compositions.
  predictors <- data.frame(ilr(acomp(predictors)))  # ilr and acomp from pkg compositions

  # Training set and testing set.
  train.predictors <- predictors[inTraining, ]
  test.predictors  <- predictors[-inTraining, ]
  train.classes    <- classes[inTraining]
  test.classes     <- classes[-inTraining]

  ################################################
  ## Building the glmnet classification models. ##
  ################################################

  #--------------------------------------------------
  ## Method 0: "glmnet" function from package glmnet.
  #--------------------------------------------------

  # Fitting model.
  glmnetFit0 <- glmnet(as.matrix(train.predictors), train.classes, family = "multinomial")

  # Tuning parameters alpha and lambda: alpha = 1 (default) and the best lambda value corresponds to the minimum training error.
  pred <- predict(glmnetFit0, newx = as.matrix(train.predictors), type="class")  # pred ...to choose the lambda value

  # Finding lambda position.
  s <- which.max(apply(pred == train.classes, 2, mean))  # s: lambda position corresponds to the minimum training error

  # Minimum training error.
  err0.train <- 1 - max(apply(pred == train.classes, 2, mean))

  # Prediction with the best lambda value (as picked by 's').
  pred0 <- as.integer(predict(glmnetFit0, newx = as.matrix(test.predictors), type="class", s = glmnetFit0$lambda[s[[1]]]))  # as.integer for ordered levels in cmo

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred0))) == length(levels(test.classes))) {
    cm0 <- confusionMatrix(table(pred0 = as.factor(pred0), truth = test.classes))
    err0.test <- as.numeric(1 - cm0$overall[1])
  }	else {
      cm0 <- table(pred0 = as.factor(pred0), truth = test.classes)
      err0.test <- 1 - (sum(pred0 == test.classes)/length(test.classes))
    }

  #-----------------------------------------------------
  ## Method 1: "cv.glmnet" function from package glmnet.
  #-----------------------------------------------------

  # Set up parallel backend for parallel option in cv.glmnet().
  if (cv.parallel) {
    cores <- detectCores()  # maximum number of available cores
    cl    <- makeCluster(cores)
    registerDoParallel(cl)
  }

  # Fitting model.
  set.seed(2010)  # for reproducible cross-validation
  glmnetFit1 <- cv.glmnet(as.matrix(train.predictors), train.classes, family = "multinomial", type.measure = "class", cv.parallel = cv.parallel)

  # Minimum training error.
  err1.train <- min(glmnetFit1$cvm)

  pred1 <- as.integer(predict(glmnetFit1, newx = as.matrix(test.predictors), type = "class"))  # as.integer for ordered levels in cm1

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred1))) == length(levels(test.classes))) {
    cm1 <- confusionMatrix(table(pred1 = as.factor(pred1), truth = test.classes))
    err1.test <- as.numeric(1 - cm1$overall[1])
  }	else {
      cm1 <- table(pred1 = as.factor(pred1), truth = test.classes)
      err1.test <- 1 - (sum(pred1 == test.classes)/length(test.classes))
    }

  # Stop the cluster.
  if (cv.parallel) stopCluster(cl)

  #-------------------------------------------------------
  ## Method 2: "train.glmnet" function from package caret.
  #-------------------------------------------------------

  err2.train <- NA
  cm2        <- NA
  err2.test  <- NA

  if (train.glmnet) {

    # Set up parallel backend for parallel option in train().
    if (train.parallel) {
      cores <- detectCores()  # maximum number of available cores
      cl    <- makeCluster(cores)
      registerDoParallel(cl)
    }

    # Fit control.
	  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, allowParallel = train.parallel)  # 10-fold cv repeated 10 times

    # Tuning parameter grid (lambda values grid is the same as in glmnetFit0).
    paramGrid <- expand.grid(alpha = seq(0.1, 1, 0.1), lambda = glmnetFit0$lambda)

    # Fitting model.
    set.seed(2010)  # for reproducible train
    glmnetFit2 <- train(train.predictors, train.classes, method = "glmnet", trControl = fitControl, tuneGrid = paramGrid)

    # Minimum training error.
    err2.train <- 1 - mean(glmnetFit2$resample$Accuracy)

    pred2 <- predict(glmnetFit2, newdata = test.predictors)  # pred2 is factor (with orderd levels for cm2).

    # Check levels in prediction and truth for confusion matrix.
    if (length(levels(pred2)) == length(levels(test.classes))) {
      cm2 <- confusionMatrix(table(pred2 = pred2, truth = test.classes))
      err2.test <- as.numeric(1 - cm2$overall[1])
    } else {
        cm2 <- table(pred2 = pred2, truth = test.classes)
        err2.test <- 1 - (sum(pred2 == test.classes)/length(test.classes))
      }

    # Stop the cluster.
    if (train.parallel) stopCluster(cl)
  }

  # Misclassification error = 1 - Accuracy.
  err <- list(e0.train = err0.train,
              e1.train = err1.train,
              e2.train = err2.train,
              e0.test  = err0.test,
              e1.test  = err1.test,
              e2.test  = err2.test,
              cm0      = cm0,
              cm1      = cm1,
              cm2      = cm2)

  return(err)
}
