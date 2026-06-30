# ============================================================
# icarm: Data utilities
# ============================================================

#' Reproducible train/test split
#'
#' @param data A data.frame or tibble.
#' @param prop Proportion for training (default 0.75).
#' @param seed Integer seed (default 2025).
#' @param stratify Optional column name for stratified split.
#' @return A named list with train, test, seed, prop.
#' @export
#'
#' @examples
#' splits <- icarm_split(iris, prop = 0.8, stratify = "Species")
#' nrow(splits$train)
icarm_split <- function(data, prop = 0.75, seed = 2025L,
                         stratify = NULL) {
  if (!is.data.frame(data))
    rlang::abort("`data` must be a data.frame.")
  if (prop <= 0 || prop >= 1)
    rlang::abort("`prop` must be between 0 and 1.")

  set.seed(seed)
  n <- nrow(data)

  if (!is.null(stratify)) {
    if (!stratify %in% names(data))
      rlang::abort(paste0("Column '", stratify, "' not found."))
    sc <- data[[stratify]]
    if (is.numeric(sc))
      sc <- cut(sc, breaks = 4L, labels = FALSE,
                include.lowest = TRUE)
    idx <- unlist(lapply(
      split(seq_len(n), sc),
      function(i) sample(i, max(1L, floor(length(i)*prop)))
    ))
  } else {
    idx <- sample(seq_len(n), floor(n * prop))
  }

  list(
    train = data[ idx, , drop = FALSE],
    test  = data[-idx, , drop = FALSE],
    seed  = seed,
    prop  = prop
  )
}


#' Compute performance metrics for any task
#'
#' @param y_true True outcome values.
#' @param y_pred Predicted values.
#' @param y_prob Numeric probability for positive class (binary, for AUC).
#' @param positive Positive class label (binary classification).
#' @param type One of `"auto"`, `"binary"`, `"multiclass"`,
#'   `"regression"`.
#' @return A named numeric vector of metrics.
#' @export
#'
#' @examples
#' # Classification
#' y    <- factor(c("yes","no","yes","yes","no"))
#' yhat <- factor(c("yes","no","no","yes","no"))
#' icarm_metrics(y, yhat, positive = "yes")
#'
#' # Regression
#' icarm_metrics(c(1,2,3,4,5), c(1.1,2.2,2.9,4.1,4.8))
#'
#' # Multiclass
#' m <- icarm_fit(Species ~ ., iris)
#' icarm_metrics(iris$Species, predict(m, iris))
icarm_metrics <- function(y_true, y_pred, y_prob = NULL,
                           positive = NULL, type = "auto") {
  if (type == "auto") type <- .detect_task(y_true)

  if (type == "regression") {
    y_true <- as.numeric(y_true)
    y_pred <- as.numeric(y_pred)
    resid  <- y_true - y_pred
    ss_res <- sum(resid^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    return(c(
      mae  = mean(abs(resid)),
      rmse = sqrt(mean(resid^2)),
      r2   = 1 - ss_res / max(ss_tot, .Machine$double.eps)
    ))
  }

  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))
  lvls   <- levels(y_true)

  if (type == "multiclass" || length(lvls) > 2L) {
    acc <- mean(y_true == y_pred)
    pc  <- sapply(lvls, function(cls) {
      tp   <- sum(y_pred == cls & y_true == cls)
      fp   <- sum(y_pred == cls & y_true != cls)
      fn   <- sum(y_pred != cls & y_true == cls)
      prec <- tp / max(tp + fp, 1L)
      rec  <- tp / max(tp + fn, 1L)
      f1   <- 2*prec*rec / max(prec+rec, .Machine$double.eps)
      c(precision=prec, recall=rec, f1=f1)
    })
    return(c(
      accuracy     = acc,
      balanced_acc = mean(pc["recall",]),
      precision    = mean(pc["precision",]),
      recall       = mean(pc["recall",]),
      f1           = mean(pc["f1",])
    ))
  }

  if (is.null(positive)) positive <- lvls[1L]
  cm  <- .confusion(y_true, y_pred, positive)
  f1v <- 2*cm$ppv*cm$tpr /
    max(cm$ppv + cm$tpr, .Machine$double.eps)
  out <- c(
    accuracy     = cm$acc,
    balanced_acc = (cm$tpr + cm$tnr) / 2,
    f1           = f1v,
    precision    = cm$ppv,
    recall       = cm$tpr,
    specificity  = cm$tnr
  )
  if (!is.null(y_prob) &&
      requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- tryCatch(
      pROC::roc(response  = y_true,
                predictor = y_prob,
                levels    = c(setdiff(lvls, positive), positive),
                quiet     = TRUE),
      error = function(e) NULL
    )
    if (!is.null(roc_obj))
      out["auc"] <- as.numeric(pROC::auc(roc_obj))
  }
  out
}


#' Threshold sweep for binary classification
#'
#' @param y_true Factor of true class labels.
#' @param y_prob Numeric probability vector for positive class.
#' @param positive Positive class label.
#' @param thresholds Numeric vector of thresholds to evaluate.
#' @return A tibble with one row per threshold.
#' @export
#'
#' @examples
#' y   <- factor(sample(c("yes","no"), 200, replace = TRUE))
#' p   <- runif(200)
#' thr <- icarm_thresholds(y, p, positive = "yes")
#' icarm_plot_thresholds(thr)
icarm_thresholds <- function(y_true, y_prob,
                              positive   = NULL,
                              thresholds = seq(0.1, 0.9, 0.05)) {
  y_true <- factor(y_true)
  if (is.null(positive)) positive <- levels(y_true)[1L]
  negative <- setdiff(levels(y_true), positive)[1L]

  purrr::map_dfr(thresholds, function(thr) {
    y_hat <- factor(
      ifelse(y_prob >= thr, positive, negative),
      levels = levels(y_true)
    )
    cm  <- .confusion(y_true, y_hat, positive)
    f1v <- 2*cm$ppv*cm$tpr /
      max(cm$ppv + cm$tpr, .Machine$double.eps)
    tibble::tibble(
      threshold     = thr,
      accuracy      = cm$acc,
      balanced_acc  = (cm$tpr + cm$tnr) / 2,
      precision     = cm$ppv,
      recall        = cm$tpr,
      specificity   = cm$tnr,
      f1            = f1v,
      rate_positive = mean(y_hat == positive)
    )
  })
}
