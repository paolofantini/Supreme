# mcLDA(dtm, lda.method = c("VEM", "VEM_fixed", "Gibbs"), k.runs = list(from = 2, to = 2, steps = 1), target)
  #' @title
  #' Multicore parallel runs of LDA models
  #'
  #' @description
  #' \code{mcLDA} runs multiple parallel LDA models by varying the number of topics \code{k} over a predefined grid of values
  #' and performs model selection based on logistic classification by calling \code{\link{logClass}} function.
  #'
  #' @details
  #' This function runs multiple LDA models and applies internal \code{\link{logClass}} to each model.
  #' A vector of misclassification error on the test set (\code{e1.test}) is returned and the \emph{best} model
  #' is selected with minimum misclassification error.
  #'
  #' @param dtm a document-term matrix.
  #' @param lda.method character. Approximate posterior inference method.
  #' @param k.runs the grid of \code{k} values.
  #' @param target factor. The labeling variable for logistic classification.
  #'
  #' @return a list containing the fitted LDA models and the misclassification errors.
  #' Model with minimum misclassification error, i.e. the \emph{best} model, is also returned.
  #'
  #' @export
  #'
  #' @note
  #' By default the \pkg{doParallel} package uses snow-like functionality. The snow-like functionality should work fine on Unix-like systems.
  #' Output is automatically saved in directory \code{data/ws/output} and a log file is provided in directory \code{log}.
  #'
  #' @examples
  #' \dontrun{
  #' library(Supreme)
  #' data("dtm")
  #' data("classes")
  #' dtm.lognet <- reduce_dtm(dtm, method = "lognet", target = classes)
  #'
  #' # 4 cores: one model for each core.
  #' mc.lda.models <- mcLDA(dtm.lognet$reduced, lda.method = "VEM", k.runs = list(from = 10, to = 25, steps = 5), target = classes)
  #' }
  #'
  #' @import parallel doParallel slam caret topicmodels glmnet
  #'
mcLDA <- function(dtm, lda.method = c("VEM", "VEM_fixed", "Gibbs"), k.runs = list(from = 2, to = 2, steps = 1), target) {

  # Check input.
  if (!is(dtm, "DocumentTermMatrix"))
    stop("The argument 'dtm' needs to be a DocumentTermMatrix.")

  if(missing(target))
    stop("Internal function 'logClass' needs to have a 'target' argument.")

  #--------------------
  # 0. INIZIALIZATION
  #--------------------

  # Set up parallel backend to use the maximum number of available cores.
  cores <- detectCores()
  cl    <- makeCluster(cores)
  registerDoParallel(cl)

  # Get dtm type c("hard", "tfidf", "lognet").
  dtm.type <- class(dtm)[3]

  # Find zero word docs in dtm.
  zeroWordDocs <- as.vector(which(row_sums(dtm) == 0))

  # In LDA model each dtm row needs to be not zero. Target variable also needs to be updated.
  if (length(zeroWordDocs != 0)) {
    dtm        <- dtm[- zeroWordDocs, ]
    class(dtm) <- append(class(dtm), c("DocumentTermMatrix", dtm.type))
    target     <- target[- zeroWordDocs]
  }

  #-----------------------------------------------------------------------------------------------------
  # 1. MULTIPLE LDA MODELS IN PARALLEL
  # Run multiple LDA models in parallel and apply logClass() to each model to select the best k value.
  #-----------------------------------------------------------------------------------------------------

  # Check LDA method.
  lda.method <- match.arg(lda.method)

  # Log file.
  log.dir <- paste("log/", lda.method, sep = "")

  if (!file.exists(log.dir))
    dir.create(log.dir, recursive = TRUE)
  log.file <- paste(log.dir, "/log_", dtm.type, ".txt", sep = "")
  writeLines(c(""), log.file)  # blank log file

  ### PARALLEL LOOP ###

  # Set random seed only for createDataPartition(): glmnet() in logClass function doesn't use random seed.
  set.seed(123)
  inTraining <- as.integer(createDataPartition(as.factor(target), p = 0.75, list = FALSE))  # for balancing the size of target classes in training set

  # Start time.
  ptm0 <- proc.time()

  lda.models <- foreach(k = iter(seq(k.runs$from, k.runs$to, k.runs$steps)), .packages = c("Supreme", "topicmodels", "caret", "glmnet"), .errorhandling = "pass") %dopar% {

    # Log file k-iteration.
    sink(log.file, append = TRUE)
    cat(paste("Number of topics", k, "\n"), file = log.file, append = TRUE)

    # LDA model.
    SEED <- 2014
    lda.mod  <- switch(lda.method,
                       VEM       = LDA(dtm, k = k, method = "VEM", control = list(seed = SEED)),
                       VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
                       Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))

    # Misclassification error.
    mis.error <- logClass(predictors = as.data.frame(posterior(lda.mod)$topics), target = target, inTraining = inTraining, train.glmnet = FALSE)

    # Results.
    lda.models <- list(mod = lda.mod, mis.error = mis.error)

    lda.models
  }

  # Time taken.
  ptm1 <- proc.time() - ptm0

  # Time spent in garbage collection so far in the R session while GC timing was enabled.
  gc_time <- gc.time()

  # Stop the cluster.
  stopCluster(cl)

  # Log file: checking of parallel backend and application time.
  cat(paste("Date:", date(), "\n"), file = log.file, append = TRUE)
  cat(paste("Type of dtm:", dtm.type, "\n"), file = log.file, append = TRUE)
  cat(paste("Number of parallel workers:", getDoParWorkers(), "\n"), file = log.file, append = TRUE)
  cat(paste("Name of parallel backend:", getDoParName(), "\n"), file = log.file, append = TRUE)
  cat(paste("Version of parallel backend:", getDoParVersion() , "\n"), file = log.file, append = TRUE)
  cat(paste("Application time:", ptm1), file = log.file, append = TRUE, fill = TRUE)
  cat(paste("Garbage collection time:", gc_time), file = log.file, append = TRUE, fill = TRUE)

  # Vector of misclassification error.
  miserr1 <- c()
  for(i in 1:length(lda.models))
    miserr1[i] <- lda.models[[i]]$mis.error$e1.test

  # The best model.
  bmod <- lda.models[[which.min(miserr1)]]$mod

  best.model <- list(mod                 = bmod,
                     topic.theMostLikely = topics(bmod, 1),
                     topic.terms         = terms(bmod, 10),
                     topic.posteriors    = as.data.frame(posterior(bmod)$topics),
                     topic.phi           = t(exp(bmod@beta)))

  # List to be returned.
  res <- list(mycall       = match.call(),
              dtm          = dtm,
              target       = target,
              zeroWordDocs = zeroWordDocs,
              inTraining   = inTraining,
              lda.models   = lda.models,
              mis.error    = miserr1,
              time.taken   = ptm1,
              gc_time      = gc_time,
              best.model   = best.model)

  # Save results.
  ws.out.dir <- paste("data/ws/output/", lda.method, sep = "")
  if (!file.exists(ws.out.dir))
    dir.create(ws.out.dir, recursive = TRUE)
  save(res, file = paste(ws.out.dir, "/ws_", dtm.type, ".RData", sep = ""))

  return(res)
}
