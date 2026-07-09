# ============================================================
# icarm: SHAP-style local feature attributions
# Marginal (interventional) approximation:
#   SHAP_j(x) ~ E_background[ f(x_j, bg_{-j}) - f(bg_j, bg_{-j}) ]
# ============================================================

#' SHAP-style local feature attributions
#'
#' @description
#' Computes approximate SHAP (SHapley Additive exPlanations) values using a
#' marginal interventional approach. For each observation and each feature,
#' the SHAP value is the average change in prediction when that feature is set
#' to the observed value versus its background distribution.
#'
#' Works with any icarm model type without requiring additional packages.
#'
#' @param object An `icarm_model` from [icarm_fit()].
#' @param data A data frame of observations to explain.
#' @param n_samples Integer. Background sample size (default 50).
#' @param max_obs Integer. Maximum observations to explain (default 100).
#'   A random sample is taken when `nrow(data) > max_obs`.
#' @param seed Integer seed for reproducibility (default 2025).
#'
#' @return An `icarm_shap` object with:
#' \describe{
#'   \item{shap}{Long-format tibble: `obs_id`, `feature`, `shap_value`,
#'     `feature_value`.}
#'   \item{importance}{Tibble ranking features by mean |SHAP|.}
#'   \item{baseline}{Scalar: mean prediction over the background.}
#' }
#'
#' @export
#'
#' @examples
#' m    <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
#' shap <- icarm_shap(m, iris[1:20, ], n_samples = 20L)
#' print(shap)
icarm_shap <- function(object, data, n_samples = 50L,
                        max_obs = 100L, seed = 2025L) {
  .check_icarm(object)
  set.seed(seed)

  features <- object$feature_names
  n_total  <- nrow(data)

  # Subsample observations to explain
  idx      <- if (n_total > max_obs)
    sample(n_total, max_obs) else seq_len(n_total)
  obs_data <- data[idx, , drop = FALSE]
  n_obs    <- nrow(obs_data)

  # Background sample
  n_bg <- min(as.integer(n_samples), n_total)
  bg   <- data[sample(n_total, n_bg, replace = FALSE), , drop = FALSE]

  pred_fn  <- .make_pred_fn(object)
  baseline <- mean(pred_fn(bg), na.rm = TRUE)
  shap_mat <- .shap_marginal(obs_data, bg, pred_fn, features)

  feat_df   <- obs_data[, features, drop = FALSE]
  shap_long <- purrr::imap_dfr(features, function(feat, j) {
    tibble::tibble(
      obs_id        = idx,
      feature       = feat,
      shap_value    = shap_mat[, j],
      feature_value = as.character(feat_df[[feat]])   # always character
    )
  })

  importance <- shap_long |>
    dplyr::group_by(feature) |>
    dplyr::summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(mean_abs_shap))

  structure(
    list(shap = shap_long, importance = importance, baseline = baseline,
         model = object, n_obs = n_obs, features = features, seed = seed),
    class = "icarm_shap"
  )
}

#' @export
print.icarm_shap <- function(x, ...) {
  cat(.icarm_rule("icarm_shap"), "\n")
  cat(sprintf("  Model    : %s / %s\n", x$model$task, x$model$model))
  cat(sprintf("  Obs      : %d explained\n", x$n_obs))
  cat(sprintf("  Baseline : %.4f\n", x$baseline))
  cat("\n  Feature importance (mean |SHAP|):\n")
  mx <- max(x$importance$mean_abs_shap, na.rm = TRUE)
  for (i in seq_len(nrow(x$importance))) {
    r   <- x$importance[i, ]
    bar <- paste(rep("|", round(r$mean_abs_shap / max(mx, 1e-9) * 18L)),
                 collapse = "")
    cat(sprintf("    %-22s %.4f  %s\n", r$feature, r$mean_abs_shap, bar))
  }
  invisible(x)
}

# ── Internals ──────────────────────────────────────────────

.make_pred_fn <- function(object) {
  task <- object$task
  pos  <- object$positive
  if (task == "regression") {
    function(nd) as.numeric(predict.icarm_model(object, nd))
  } else if (task == "binary") {
    function(nd) {
      p <- predict.icarm_model(object, nd, type = "prob")
      if (is.matrix(p)) p[, pos] else as.numeric(p)
    }
  } else {
    function(nd) {
      p <- predict.icarm_model(object, nd, type = "prob")
      if (is.matrix(p)) apply(p, 1L, max) else as.numeric(p)
    }
  }
}

.shap_marginal <- function(obs_data, background, pred_fn, features) {
  n_obs   <- nrow(obs_data)
  pred_bg <- pred_fn(background)           # cached: constant across features

  mat <- matrix(0, nrow = n_obs, ncol = length(features))
  colnames(mat) <- features

  for (j in seq_along(features)) {
    feat <- features[j]
    for (i in seq_len(n_obs)) {
      bg_mod         <- background
      bg_mod[[feat]] <- obs_data[[feat]][i]
      mat[i, j]      <- mean(pred_fn(bg_mod) - pred_bg, na.rm = TRUE)
    }
  }
  mat
}
