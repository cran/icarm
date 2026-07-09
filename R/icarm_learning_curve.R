# ============================================================
# icarm: Learning Curve Diagnostics
# ============================================================

#' Learning Curve Diagnostics
#'
#' @description
#' Trains models on progressively larger subsets of the data and records
#' training and validation error at each size, repeated over `cv_folds`
#' random splits. The resulting curve diagnoses high bias (train and val
#' errors both high and close) versus high variance (large gap between
#' train and val errors).
#'
#' @param formula A model formula.
#' @param data A data frame.
#' @param model Character model type (default `"auto"`). See [icarm_fit()].
#' @param sizes Numeric vector of training-set fractions (default 0.1 to 0.9).
#' @param cv_folds Integer. Repetitions per size for stability (default 5).
#' @param seed Integer seed (default 2025).
#' @param ... Additional arguments forwarded to [icarm_fit()].
#'
#' @return An `icarm_learning_curve` object with:
#' \describe{
#'   \item{curve}{Tibble: `train_size`, `train_frac`, `train_mean`,
#'     `train_sd`, `val_mean`, `val_sd`.}
#'   \item{metric}{Metric name: RMSE (regression) or error rate (classification).}
#'   \item{task}{Detected task type.}
#' }
#'
#' @export
#'
#' @examples
#' lc <- icarm_learning_curve(Species ~ ., iris,
#'                             model = "cart", cv_folds = 3L,
#'                             sizes = seq(0.2, 0.8, by = 0.2))
#' print(lc)
icarm_learning_curve <- function(formula, data, model = "auto",
                                  sizes    = seq(0.1, 0.9, by = 0.1),
                                  cv_folds = 5L,
                                  seed     = 2025L, ...) {
  set.seed(seed)
  n       <- nrow(data)
  outcome <- all.vars(formula)[1L]

  # Detect task once so it is available in the return object
  y_raw  <- data[[outcome]]
  task   <- .detect_task(y_raw)
  metric <- if (task == "regression") "RMSE" else "Error rate"

  lc_rows <- purrr::map_dfr(sizes, function(s) {
    size <- max(10L, round(s * n))

    fold_res <- purrr::map_dfr(seq_len(cv_folds), function(fold) {
      set.seed(seed + fold)
      train_idx <- sample(n, size, replace = FALSE)
      train     <- data[train_idx,  , drop = FALSE]
      test      <- data[-train_idx, , drop = FALSE]
      if (nrow(test) < 5L) return(NULL)

      m <- tryCatch(
        icarm_fit(formula, train, model = model,
                  seed = seed + fold, ...),
        error = function(e) NULL
      )
      if (is.null(m)) return(NULL)

      tibble::tibble(
        fold      = fold,
        train_err = .lc_error(m, train, outcome),
        val_err   = .lc_error(m, test,  outcome)
      )
    })

    if (is.null(fold_res) || nrow(fold_res) == 0L) return(NULL)
    tibble::tibble(
      train_size = size,
      train_frac = s,
      train_mean = mean(fold_res$train_err, na.rm = TRUE),
      train_sd   = stats::sd(fold_res$train_err,   na.rm = TRUE),
      val_mean   = mean(fold_res$val_err,   na.rm = TRUE),
      val_sd     = stats::sd(fold_res$val_err,     na.rm = TRUE)
    )
  })

  structure(
    list(curve = lc_rows, model = model, task = task,
         metric = metric, formula = formula,
         cv_folds = cv_folds, seed = seed),
    class = "icarm_learning_curve"
  )
}

#' @export
print.icarm_learning_curve <- function(x, ...) {
  cat(.icarm_rule("icarm_learning_curve"), "\n")
  cat(sprintf("  Model    : %s  |  Task: %s\n", x$model, x$task))
  cat(sprintf("  Metric   : %s\n", x$metric))
  cat(sprintf("  CV folds : %d\n", x$cv_folds))
  if (!is.null(x$curve) && nrow(x$curve) > 0L) {
    cat(sprintf("  Sizes    : %d points\n", nrow(x$curve)))
    best <- x$curve[which.min(x$curve$val_mean), ]
    cat(sprintf("  Best val : %.4f  (n = %d, %.0f%% of data)\n",
                best$val_mean, best$train_size, best$train_frac * 100))
  }
  invisible(x)
}

# Internal: error metric for a fitted model on a data split
.lc_error <- function(object, data, outcome) {
  tryCatch({
    y_true <- data[[outcome]]
    if (object$task == "regression") {
      y_hat <- predict.icarm_model(object, data)
      sqrt(mean((as.numeric(y_true) - y_hat)^2, na.rm = TRUE))
    } else {
      y_hat <- predict.icarm_model(object, data, type = "class")
      1 - mean(y_hat == y_true, na.rm = TRUE)
    }
  }, error = function(e) NA_real_)
}
