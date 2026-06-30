# ============================================================
# icarm: Internal utilities
# ============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
      plot.title    = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["primary"]),
        face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["neutral"]),
        size = base_size - 1),
      plot.caption  = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["neutral"]),
        size = base_size - 2, hjust = 0),
      axis.title    = ggplot2::element_text(
        colour = paste0("#", .icarm_pal["primary"])),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
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
