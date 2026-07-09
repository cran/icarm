# ============================================================
# icarm: Visualisation functions
# All plot functions return ggplot2 objects.
# ============================================================

# ── Feature importance (lollipop) ─────────────────────────

#' Plot feature importance
#'
#' @param explainer An `icarm_explainer` from [icarm_explain()].
#' @param n_features Max features to display (default 15).
#' @param title Optional plot title.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' m  <- icarm_fit(Species ~ ., iris)
#' ex <- icarm_explain(m)
#' p  <- icarm_plot_importance(ex)
icarm_plot_importance <- function(explainer, n_features = 15L, title = NULL) {
  stopifnot(inherits(explainer, "icarm_explainer"))
  if (is.null(explainer$importance) || nrow(explainer$importance) == 0L)
    rlang::abort("No importance data available.")

  df <- utils::head(
    dplyr::arrange(explainer$importance, dplyr::desc(importance)),
    n_features)
  df$feature <- factor(df$feature, levels = rev(df$feature))

  method_lbl <- switch(explainer$importance_method,
    rpart_impurity  = "Tree impurity (Gini)",
    abs_coefficient = "Absolute coefficient",
    rf_importance   = "Random forest importance",
    xgb_gain        = "XGBoost gain",
    none            = "Not available -- use icarm_shap()",
    explainer$importance_method)

  # Lollipop: segment from 0 + coloured dot
  ggplot2::ggplot(df, ggplot2::aes(x = importance, y = feature,
                                    colour = importance)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = 0, yend = feature),
      linewidth = 0.9, colour = "grey82") +
    ggplot2::geom_point(ggplot2::aes(size = importance_scaled * 4 + 2)) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(importance, 3L)),
      hjust = -0.35, size = 3,
      colour = paste0("#", .icarm_pal["neutral"])) +
    ggplot2::scale_colour_gradient(
      low  = "#AED6F1",
      high = paste0("#", .icarm_pal["primary"]),
      guide = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(
      x       = method_lbl,
      y       = NULL,
      title   = title %||%
        paste0("Feature Importance -- ", explainer$model$model),
      caption = "icarm") +
    .icarm_theme()
}

# ── SHAP beeswarm ─────────────────────────────────────────

#' Beeswarm SHAP plot
#'
#' @description
#' Displays SHAP values as a beeswarm-style scatter: each point is one
#' observation, the x-axis shows the SHAP value (positive = pushes prediction
#' up, negative = down), and colour encodes the feature value
#' (blue = low, red = high).
#'
#' @param shap An `icarm_shap` from [icarm_shap()].
#' @param n_features Max features to show (default 10).
#' @param title Optional plot title.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' m    <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
#' shap <- icarm_shap(m, iris[1:30, ], n_samples = 20L)
#' p    <- icarm_plot_shap(shap)
icarm_plot_shap <- function(shap, n_features = 10L, title = NULL) {
  stopifnot(inherits(shap, "icarm_shap"))

  top_feats <- utils::head(shap$importance$feature, n_features)

  df <- shap$shap |>
    dplyr::filter(feature %in% top_feats) |>
    dplyr::group_by(feature) |>
    dplyr::mutate(
      feat_num   = suppressWarnings(as.numeric(feature_value)),
      feat_min   = min(feat_num, na.rm = TRUE),
      feat_max   = max(feat_num, na.rm = TRUE),
      feat_range = feat_max - feat_min,
      feat_scaled = dplyr::case_when(
        !is.finite(feat_num) | feat_range <= 0 ~ 0.5,
        TRUE ~ (feat_num - feat_min) / feat_range
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(feature = factor(feature, levels = rev(top_feats)))

  ggplot2::ggplot(df, ggplot2::aes(x = shap_value, y = feature,
                                    colour = feat_scaled)) +
    ggplot2::geom_jitter(height = 0.22, width = 0,
                         alpha = 0.75, size = 2.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey55", linewidth = 0.7) +
    ggplot2::scale_colour_gradient2(
      low      = "#2980B9",
      mid      = "#ECF0F1",
      high     = "#C0392B",
      midpoint = 0.5,
      name     = "Feature\nvalue",
      breaks   = c(0, 0.5, 1),
      labels   = c("Low", "Mid", "High"),
      guide    = ggplot2::guide_colourbar(
        barwidth = 7, barheight = 0.5, title.position = "top")) +
    ggplot2::labs(
      x       = "SHAP value  (impact on model output)",
      y       = NULL,
      title   = title %||%
        paste0("SHAP Feature Contributions -- ", shap$model$model),
      caption = "icarm") +
    .icarm_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# ── Partial Dependence Plot ────────────────────────────────

#' Plot a Partial Dependence Profile
#'
#' @param pdp An `icarm_pdp` from [icarm_pdp()].
#' @param title Optional plot title.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' m   <- icarm_fit(Sepal.Length ~ ., iris, model = "linear")
#' pdp <- icarm_pdp(m, iris, feature = "Petal.Length")
#' p   <- icarm_plot_pdp(pdp)
icarm_plot_pdp <- function(pdp, title = NULL) {
  stopifnot(inherits(pdp, "icarm_pdp"))
  df <- pdp$pdp

  if (pdp$is_numeric) {
    df$feature_value <- as.numeric(df$feature_value)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = feature_value)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                           fill  = paste0("#", .icarm_pal["secondary"]),
                           alpha = 0.18) +
      ggplot2::geom_line(ggplot2::aes(y = mean_pred),
                         colour   = paste0("#", .icarm_pal["primary"]),
                         linewidth = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = mean_pred),
                          colour = paste0("#", .icarm_pal["primary"]),
                          size = 2.5, alpha = 0.8)
  } else {
    df$feature_value <- factor(df$feature_value)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = feature_value)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = 0.25,
        colour = paste0("#", .icarm_pal["secondary"]),
        linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(y = mean_pred),
                          colour = paste0("#", .icarm_pal["primary"]),
                          size = 3.5)
  }

  p +
    ggplot2::labs(
      x       = pdp$feature,
      y       = paste0("Average prediction",
                       if (pdp$model$task == "binary")
                         paste0(" (P(", pdp$model$positive, "))") else ""),
      title   = title %||% paste0("Partial Dependence -- ", pdp$feature),
      subtitle = "Shaded band: 10th-90th percentile of individual predictions",
      caption  = "icarm") +
    .icarm_theme()
}

# ── Learning Curve ─────────────────────────────────────────

#' Plot a Learning Curve
#'
#' @param lc An `icarm_learning_curve` from [icarm_learning_curve()].
#' @param title Optional plot title.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' lc <- icarm_learning_curve(Species ~ ., iris, model = "cart",
#'                             cv_folds = 3L,
#'                             sizes = seq(0.2, 0.8, by = 0.2))
#' p  <- icarm_plot_learning_curve(lc)
icarm_plot_learning_curve <- function(lc, title = NULL) {
  stopifnot(inherits(lc, "icarm_learning_curve"))
  df  <- lc$curve
  col_train <- paste0("#", .icarm_pal["secondary"])
  col_val   <- paste0("#", .icarm_pal["accent"])

  ggplot2::ggplot(df, ggplot2::aes(x = train_size)) +
    # Validation band + line
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = val_mean - val_sd, ymax = val_mean + val_sd),
      fill = col_val, alpha = 0.15) +
    ggplot2::geom_line(ggplot2::aes(y = val_mean, colour = "Validation"),
                       linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = val_mean, colour = "Validation"),
                        size = 2.5) +
    # Training band + line
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = train_mean - train_sd, ymax = train_mean + train_sd),
      fill = col_train, alpha = 0.15) +
    ggplot2::geom_line(ggplot2::aes(y = train_mean, colour = "Training"),
                       linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = train_mean, colour = "Training"),
                        size = 2.5) +
    ggplot2::scale_colour_manual(
      values = c(Training = col_train, Validation = col_val),
      name   = NULL) +
    ggplot2::labs(
      x       = "Training set size (observations)",
      y       = lc$metric,
      title   = title %||%
        paste0("Learning Curve -- ", lc$model, "  [", lc$task, "]"),
      subtitle = paste0("Bands: +/- 1 SD over ", lc$cv_folds, " random splits"),
      caption  = "icarm") +
    .icarm_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# ── Confusion matrix (upgraded) ───────────────────────────

#' Plot confusion matrix
#'
#' @param y_true Factor of true outcomes.
#' @param y_pred Factor of predicted outcomes.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' m    <- icarm_fit(Species ~ ., iris)
#' yhat <- predict(m, iris)
#' p    <- icarm_plot_confusion(iris$Species, yhat)
icarm_plot_confusion <- function(y_true, y_pred, title = NULL) {
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))
  df     <- as.data.frame(table(Predicted = y_pred, Actual = y_true))

  # Compute per-row totals for normalisation label
  totals <- tapply(df$Freq, df$Actual, sum)
  df$pct <- sprintf("%.0f%%", df$Freq / totals[as.character(df$Actual)] * 100)
  df$label <- paste0(df$Freq, "\n", df$pct)

  ggplot2::ggplot(df, ggplot2::aes(x = Actual, y = Predicted, fill = Freq)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       size = 4.2, fontface = "bold", colour = "white") +
    ggplot2::scale_fill_gradient(
      low  = paste0("#", .icarm_pal["secondary"]),
      high = paste0("#", .icarm_pal["primary"]),
      name = "Count",
      guide = ggplot2::guide_colourbar(
        barwidth = 7, barheight = 0.5)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title   = title %||% "Confusion Matrix",
      caption = "icarm  |  count + row %") +
    .icarm_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# ── Fairness dot plot (upgraded) ─────────────────────────

#' Plot group-level fairness metric
#'
#' @param fairness An `icarm_fairness` from [icarm_fairness()].
#' @param metric Character. Column to plot.
#' @param title Optional title.
#' @param ref_line Optional numeric reference line.
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' m <- icarm_fit(Species ~ ., iris)
#' iris$size <- factor(ifelse(iris$Sepal.Length > 5.8, "large", "small"))
#' f <- icarm_fairness(m, iris, "Species", "size")
#' p <- icarm_plot_fairness(f, metric = "acc")
icarm_plot_fairness <- function(fairness, metric = "acc",
                                 title = NULL, ref_line = NULL) {
  stopifnot(inherits(fairness, "icarm_fairness"))
  if (!metric %in% names(fairness))
    rlang::abort(paste0("Metric '", metric, "' not found. Available: ",
                        paste(names(fairness), collapse = ", ")))

  protected <- attr(fairness, "protected") %||% "group"
  df        <- as.data.frame(fairness)
  df$value  <- df[[metric]]

  gap_metrics <- c("acc_gap", "tpr_gap", "fpr_gap",
                   "mae_gap", "rmse_gap", "eo_gap")
  is_gap  <- metric %in% gap_metrics
  good    <- paste0("#", .icarm_pal["fair"])
  bad     <- paste0("#", .icarm_pal["unfair"])
  neutral <- paste0("#", .icarm_pal["secondary"])

  dot_col <- if (is_gap)
    ifelse(abs(df$value) < 0.05, good, bad)
  else
    rep(neutral, nrow(df))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = grp)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = 0, yend = grp),
      colour = "grey82", linewidth = 0.9) +
    ggplot2::geom_point(colour = dot_col, size = 5) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(value, 3L)),
      hjust = -0.4, size = 3.5,
      colour = paste0("#", .icarm_pal["primary"])) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
    ggplot2::labs(
      x       = metric,
      y       = protected,
      title   = title %||% paste0("Fairness: ", metric),
      caption = "icarm") +
    .icarm_theme()

  if (!is.null(ref_line))
    p <- p + ggplot2::geom_vline(
      xintercept = ref_line, linetype = "dashed",
      colour = paste0("#", .icarm_pal["accent"]), linewidth = 0.8)
  p
}

# ── Calibration ───────────────────────────────────────────

#' Plot calibration curve
#'
#' @param calibration An `icarm_calibration` from [icarm_calibrate()].
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
icarm_plot_calibration <- function(calibration, title = NULL) {
  stopifnot(inherits(calibration, "icarm_calibration"))
  bins <- calibration$bins
  ggplot2::ggplot(bins, ggplot2::aes(x = mean_pred, y = obs_freq)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = paste0("#", .icarm_pal["neutral"]),
                         linewidth = 0.8) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = obs_freq * 0.85,
                                       ymax = obs_freq * 1.15),
                         fill = paste0("#", .icarm_pal["secondary"]),
                         alpha = 0.12) +
    ggplot2::geom_line(colour = paste0("#", .icarm_pal["secondary"]),
                       linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(size = n),
                        colour = paste0("#", .icarm_pal["primary"]),
                        alpha = 0.85) +
    ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x        = "Mean predicted probability",
      y        = "Observed frequency",
      title    = title %||%
        paste0("Calibration -- ", calibration$model$model),
      subtitle = sprintf("Brier = %.4f  |  ECE = %.4f",
                         calibration$brier_score, calibration$ece),
      caption  = "icarm") +
    .icarm_theme()
}

# ── Threshold curves ──────────────────────────────────────

#' Plot threshold performance curves
#'
#' @param thresholds_tbl A tibble from [icarm_thresholds()].
#' @param metrics Character vector of metric columns.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
icarm_plot_thresholds <- function(thresholds_tbl,
    metrics = c("accuracy", "recall", "precision", "f1"),
    title   = NULL) {
  avail <- intersect(metrics, names(thresholds_tbl))
  long  <- tidyr::pivot_longer(
    thresholds_tbl,
    cols      = dplyr::all_of(avail),
    names_to  = "metric",
    values_to = "value")
  ggplot2::ggplot(long, ggplot2::aes(x = threshold, y = value,
                                      colour = metric)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.8, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                        colour = "grey55", linewidth = 0.7) +
    ggplot2::scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    ggplot2::scale_colour_brewer(palette = "Set2", name = "Metric") +
    ggplot2::labs(x = "Threshold", y = "Metric value",
                  title   = title %||% "Performance vs Threshold",
                  caption = "icarm") +
    .icarm_theme()
}

# ── Multi-model comparison ────────────────────────────────

#' Plot multi-model comparison
#'
#' @param comparison An `icarm_comparison` from [icarm_compare()].
#' @param metrics Character vector of metric columns.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
icarm_plot_comparison <- function(comparison,
    metrics = c("accuracy", "f1", "max_tpr_gap", "min_dp_ratio"),
    title   = NULL) {
  stopifnot(inherits(comparison, "icarm_comparison"))
  avail <- intersect(metrics, names(comparison))
  avail <- avail[sapply(comparison[avail], is.numeric)]
  long  <- tidyr::pivot_longer(
    comparison[, c("model_name", avail)],
    cols      = dplyr::all_of(avail),
    names_to  = "metric",
    values_to = "value") |>
    dplyr::filter(!is.na(value))
  ggplot2::ggplot(long, ggplot2::aes(x = model_name, y = value,
                                      fill = model_name)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3L)),
                       vjust = -0.4, size = 3,
                       colour = paste0("#", .icarm_pal["neutral"])) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(x = NULL, y = NULL,
                  title   = title %||% "Model Comparison",
                  caption = "icarm") +
    .icarm_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))
}

# ── Per-group ROC curves ──────────────────────────────────

#' Plot per-group ROC curves
#'
#' @param eoc_tbl A tibble from [icarm_equalized_odds_curve()].
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
icarm_plot_roc_groups <- function(eoc_tbl, title = NULL) {
  ggplot2::ggplot(eoc_tbl, ggplot2::aes(x = fpr, y = tpr,
                                          colour = group, group = group)) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", colour = "grey70") +
    ggplot2::geom_path(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.8, alpha = 0.7) +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "Group") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate",
                  title   = title %||% "Group ROC Curves",
                  caption = "icarm") +
    .icarm_theme()
}
