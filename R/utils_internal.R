# ============================================================
# icarm: Internal utilities
# ============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Suppress R CMD check NOTEs for column names used in dplyr / ggplot2 NSE
utils::globalVariables(c(
  # icarm_shap
  "shap_value", "mean_abs_shap", "feature_value",
  "feat_num", "feat_min", "feat_max", "feat_range",
  "feat_scaled", "feat_finite",
  # icarm_plot_confusion
  "label", "Actual", "Predicted", "Freq",
  # icarm_plot_learning_curve
  "train_size", "train_mean", "train_sd", "val_mean", "val_sd",
  # icarm_plot_pdp
  "lower", "upper", "mean_pred",
  # icarm_plot_importance
  "importance", "importance_scaled",
  # icarm_plot_fairness
  "grp", "value",
  # icarm_plot_roc_groups
  "fpr", "tpr", "group",
  # icarm_plot_comparison / icarm_plot_thresholds
  "model_name", "metric", "threshold"
))

.icarm_pal <- c(
  primary   = "2C3E50",
  secondary = "2980B9",
  accent    = "E67E22",
  fair      = "27AE60",
  unfair    = "C0392B",
  neutral   = "7F8C8D",
  light     = "EBF5FB"
)

.icarm_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Titles
      plot.title    = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["primary"]),
        face = "bold", size = base_size + 2,
        margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["neutral"]),
        size = base_size - 1,
        margin = ggplot2::margin(b = 8)),
      plot.caption  = ggplot2::element_text(
        colour = "grey75", size = base_size - 3, hjust = 1),
      plot.margin = ggplot2::margin(12, 14, 8, 10),
      # Axes
      axis.title    = ggplot2::element_text(
        colour = "grey35", size = base_size - 0.5),
      axis.text     = ggplot2::element_text(colour = "grey45"),
      # Gridlines: only horizontal, very light
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey93", linewidth = 0.45),
      panel.grid.minor   = ggplot2::element_blank(),
      # Backgrounds
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      # Legend
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(colour = "grey35",
                                               size = base_size - 1),
      legend.title     = ggplot2::element_text(colour = "grey35",
                                               size = base_size - 1),
      legend.key.size  = ggplot2::unit(0.9, "lines"),
      # Facets
      strip.text = ggplot2::element_text(
        face = "bold", colour = paste0("#", .icarm_pal["primary"]),
        size = base_size - 0.5),
      strip.background = ggplot2::element_rect(
        fill = paste0("#", .icarm_pal["light"]), colour = NA)
    )
}

.icarm_rule <- function(label) {
  w <- min(getOption("width", 80L), 80L)
  dashes <- paste(rep("-", max(w - nchar(label) - 4L, 4L)),
                  collapse = "")
  paste0("-- ", label, " ", dashes)
}

.check_icarm <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "icarm_model"))
    rlang::abort(
      c("Expected an `icarm_model` object.",
        i = "Use `icarm_fit()` to create one."),
      call = call
    )
  invisible(x)
}

.detect_task <- function(y) {
  if (is.numeric(y) || is.integer(y)) return("regression")
  y <- as.factor(y)
  if (nlevels(y) == 2L) return("binary")
  return("multiclass")
}

.prepare_y <- function(y, task) {
  if (task %in% c("binary", "multiclass")) {
    if (!is.factor(y)) as.factor(y) else y
  } else {
    if (!is.numeric(y)) as.numeric(y) else y
  }
}

.confusion <- function(y_true, y_pred, positive) {
  tp <- sum(y_pred == positive & y_true == positive)
  tn <- sum(y_pred != positive & y_true != positive)
  fp <- sum(y_pred == positive & y_true != positive)
  fn <- sum(y_pred != positive & y_true == positive)
  n  <- length(y_true)
  list(
    tp = tp, tn = tn, fp = fp, fn = fn, n = n,
    tpr = tp / max(tp + fn, 1L),
    tnr = tn / max(tn + fp, 1L),
    fpr = fp / max(fp + tn, 1L),
    fnr = fn / max(fn + tp, 1L),
    ppv = tp / max(tp + fp, 1L),
    acc = (tp + tn) / max(n, 1L)
  )
}

# Interpretability rating — extended for black-box models
.interp_label <- function(model_str) {
  switch(model_str,
    cart          = "HIGH   - decision tree (fully inspectable)",
    logistic      = "HIGH   - logistic regression (readable coefficients)",
    logistic_l1   = "HIGH   - sparse logistic (L1 penalised)",
    linear        = "HIGH   - linear regression (readable coefficients)",
    gam           = "MEDIUM - GAM (smooth terms)",
    multinomial   = "MEDIUM - multinomial logistic",
    random_forest = "LOW    - random forest (post-hoc explanation needed)",
    xgboost       = "LOW    - XGBoost (post-hoc explanation needed)",
    svm           = "LOW    - SVM (post-hoc explanation needed)",
    "UNKNOWN"
  )
}

# Is the model interpretable (used for ICARM compliance check)
.is_interpretable <- function(model_str) {
  model_str %in% c("cart", "logistic", "logistic_l1",
                   "linear", "gam", "multinomial")
}
