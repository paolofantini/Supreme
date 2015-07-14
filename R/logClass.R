# logClass(predictors, target, inTraining, train.glmnet = FALSE)
  #' @title
  #' Internal Supreme function
  #'
  #' @description
  #' \code{logClass} fits logistic classification models from \pkg{glmnet} and \pkg{caret} packages to the
  #' \code{topic.posteriors} matrix of predictors trained by LDA topic model.
  #' A \code{target} variable is used as a supervision variable.
  #' \code{logClass} is called by \code{\link{mcLDA}} function.
  #'
  #' @details
  #' This function applies three different methods.
  #' \emph{Method0} and \emph{Method1} respectively refer to functions \code{glmnet} and \code{cv.glmnet} from package \pkg{glmnet}.
  #' \emph{Method2} refers to function \code{train.glmnet} from package \pkg{caret}.
  #' \emph{Method0} tends to overfit the training set.
  #' \emph{Method1} and \emph{Method2} try to avoid overfitting problems using cross-validation.
  #' \emph{Method2} uses repeated cross-validation and is more stable than \emph{Method1} but much more time-consuming.
  #'
  #' @param predictors the matrix of predictors, i. e. the posterior topic proportions for each doc.
  #' @param target factor, the classification variable.
  #' @param inTraining the numeric ids of documents belonging to the training set.
  #' @param train.glmnet logical. If \code{TRUE} runs \code{train.glmnet} function from package \pkg{caret}. Default is \code{FALSE}.
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
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' library(topicmodels)
  #' library(caret)
  #'
  #' # Input data.
  #' data("dtm")
  #' data("classes")
  #'
  #' # Reduced dtm.lognet
  #' dtm.lognet <- reduce_dtm(dtm, method = "lognet", target = classes, export = TRUE)
  #'
  #' # Run a 35-topic model over the reduced dtm.lognet and compute topic posteriors.
  #' ldaVEM.mod <- LDA(dtm.lognet$reduced, k = 35, method = "VEM", control = list(seed = 2014))
  #' topic.posteriors <- as.data.frame(posterior(ldaVEM.mod)$topics)
  #'
  #' # Misclassification error.
  #' set.seed(2010)  # for inTraining reproducibility
  #' inTraining <- createDataPartition(classes, p = 0.75, list = FALSE)  # for balancing the size of target classes in training set
  #' mis.error <- logClass(topic.posteriors, classes, inTraining)
  #' }
  #'
  #' @import caret glmnet
  #'
logClass <- function(predictors, target, inTraining, train.glmnet = FALSE) {

  # Data to classify and k.
  data.to.classify <- cbind(predictors, target)
  k <- ncol(predictors)

  # Training set and testing set.
  training <- data.to.classify[inTraining, ]
  testing  <- data.to.classify[-inTraining, ]

  ################################################
  ## Building the glmnet classification models. ##
  ################################################

  #--------------------------------------------------
  ## Method 0: "glmnet" function from package glmnet.

  glmnetFit0 <- glmnet(as.matrix(training[c(1:k)]), training$target, family = "multinomial")

  # Tuning parameters alpha and lambda: alpha = 1 (default) and best lambda value corresponding to minimum training error.
  prd <- predict(glmnetFit0, newx = as.matrix(training[c(1:k)]), type="class")  # prd ...to choose lambda value

  # Finding lambda position.
  s <- which.max(apply(prd == training$target, 2, mean))  # s: lambda position corresponding to minimum training error

  # Minimum training error.
  err0.train <- 1 - max(apply(prd == training$target, 2, mean))

  # Prediction with best lambda value (as picked by 's').
  pred0 <- predict(glmnetFit0, newx = as.matrix(testing[c(1:k)]), type="class", s = glmnetFit0$lambda[s[[1]]])

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred0))) == length(levels(as.factor(testing$target)))) {
    cm0 <- confusionMatrix(table(pred0 = as.factor(pred0), truth = as.factor(testing$target)))
	err0.test <- as.numeric(1 - cm0$overall[1])
  }	else {
      cm0 <- table(pred0 = as.factor(pred0), truth = as.factor(testing$target))
	  err0.test <- (sum(as.integer(pred0) == testing$target)/length(testing$target))
    }

  #-----------------------------------------------------
  ## Method 1: "cv.glmnet" function from package glmnet.

  set.seed(2010)
  glmnetFit1 <- cv.glmnet(as.matrix(training[c(1:k)]), training$target, family = "multinomial", type.measure = "class")

  err1.train <- min(glmnetFit1$cvm)  # training error

  pred1 <- predict(glmnetFit1, newx = as.matrix(testing[c(1:k)]), type = "class")

  # Check levels in prediction and truth for confusion matrix.
  if (length(levels(as.factor(pred1))) == length(levels(as.factor(testing$target)))) {
    cm1 <- confusionMatrix(table(pred1 = as.factor(pred1), truth = as.factor(testing$target)))
	err1.test <- as.numeric(1 - cm1$overall[1])
  }	else {
      cm1 <- table(pred1 = as.factor(pred1), truth = as.factor(testing$target))
	  err1.test <- (sum(as.integer(pred1) == testing$target)/length(testing$target))
    }

  #-------------------------------------------------------
  ## Method 2: "train.glmnet" function from package caret.

  err2.train <- NA
  cm2        <- NA
  err2.test  <- NA

  if (train.glmnet) {

	fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, allowParallel = TRUE)  # 10-fold cv repeated 10 times

    # Tuning parameter grid (lambda values grid is the same as in glmnetFit0).
    paramGrid <- expand.grid(alpha = seq(0.1, 1, 0.1), lambda = glmnetFit0$lambda)

    set.seed(2014)
    glmnetFit2 <- train(as.factor(target) ~ ., data = training, method = "glmnet", trControl = fitControl, tuneGrid = paramGrid)

    err2.train <- 1 - mean(glmnetFit2$resample$Accuracy)  # training error

    pred2 <- predict(glmnetFit2, newdata = testing)

	# Check levels in prediction and truth for confusion matrix.
    if (length(levels(pred2)) == length(levels(as.factor(testing$target)))) {
      cm2 <- confusionMatrix(table(pred2, truth = as.factor(testing$target)))
	  err2.test <- as.numeric(1 - cm2$overall[1])
    } else {
        cm2 <- table(pred2, truth = as.factor(testing$target))
		err2.test <- (sum(as.integer(pred2) == testing$target)/length(testing$target))
      }
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
