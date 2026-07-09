# ============================================================
# icarm: Partial Dependence Profiles
# ============================================================

#' Partial Dependence Profile
#'
#' @description
#' Computes a Partial Dependence Plot (PDP) for a single feature by
#' marginalising over all other features. At each grid point the feature is
#' set to a fixed value and predictions are averaged across all observations.
#' The 10th-90th percentile band shows individual-level variability.
#'
#' @param object An `icarm_model` from [icarm_fit()].
#' @param data A data frame used for marginalisation.
#' @param feature Character. Name of the feature to profile.
#' @param n_grid Integer. Grid resolution for numeric features (default 20).
#' @param seed Integer seed (default 2025).
#'
#' @return An `icarm_pdp` object with:
#' \describe{
#'   \item{pdp}{Tibble with `feature_value`, `mean_pred`, `lower` (10th pct),
#'     `upper` (90th pct).}
#'   \item{feature}{Profiled feature name.}
#'   \item{is_numeric}{Logical: whether the feature is numeric.}
#' }
#'
#' @export
#'
#' @examples
#' m   <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
#' pdp <- icarm_pdp(m, iris, feature = "Petal.Length")
#' print(pdp)
icarm_pdp <- function(object, data, feature, n_grid = 20L, seed = 2025L) {
  .check_icarm(object)
  if (!feature %in% object$feature_names)
    rlang::abort(paste0("'", feature, "' is not a model feature. ",
                        "Available: ",
                        paste(object$feature_names, collapse = ", ")))

  set.seed(seed)
  vals       <- data[[feature]]
  is_numeric <- is.numeric(vals)

  grid <- if (is_numeric)
    seq(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
        length.out = as.integer(n_grid))
  else
    levels(factor(vals))

  pred_fn <- .make_pred_fn(object)

  pdp_tbl <- purrr::map_dfr(grid, function(g) {
    dat_mod          <- data
    dat_mod[[feature]] <- if (is_numeric) as.numeric(g) else g
    preds            <- pred_fn(dat_mod)
    tibble::tibble(
      feature_value = if (is_numeric) as.numeric(g) else as.character(g),
      mean_pred     = mean(preds, na.rm = TRUE),
      lower         = as.numeric(stats::quantile(preds, 0.10, na.rm = TRUE)),
      upper         = as.numeric(stats::quantile(preds, 0.90, na.rm = TRUE))
    )
  })

  structure(
    list(pdp = pdp_tbl, feature = feature, model = object,
         is_numeric = is_numeric),
    class = "icarm_pdp"
  )
}

#' @export
print.icarm_pdp <- function(x, ...) {
  cat(.icarm_rule("icarm_pdp"), "\n")
  cat(sprintf("  Feature : %s (%s)\n", x$feature,
              if (x$is_numeric) "numeric" else "categorical"))
  cat(sprintf("  Model   : %s / %s\n", x$model$task, x$model$model))
  cat(sprintf("  Grid    : %d points\n", nrow(x$pdp)))
  cat(sprintf("  Range   : [%.4f, %.4f]\n",
              min(x$pdp$mean_pred, na.rm = TRUE),
              max(x$pdp$mean_pred, na.rm = TRUE)))
  invisible(x)
}
