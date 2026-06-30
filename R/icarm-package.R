#' icarm: Interpretable Contextual-Accountable and Responsible Machine Learning
#'
#' @description
#' A general-purpose framework for Interpretable Contextual-Accountable
#' and Responsible Machine Learning (ICARM) that works with any clean
#' tabular data across any application domain.
#'
#' The contextual accountability framing captures a core principle:
#' what counts as fair, interpretable, and responsible depends on the
#' deployment context. A model predicting hospital readmission requires
#' different accountability standards than one predicting loan default
#' or civic participation. icarm operationalises this principle through
#' domain-agnostic tools that adapt to any context.
#'
#' ## Quickstart
#' \code{
#' library(icarm)
#' m    <- icarm_fit(outcome ~ ., data = your_data)
#' ex   <- icarm_explain(m, data = your_data)
#' fair <- icarm_fairness(m, your_data, "outcome", "protected_col")
#' icarm_scorecard(m, test_data, outcome = "outcome")
#' }
#'
#' ## ICARM definition
#' \strong{I}nterpretable: model decisions can be explained
#' to affected stakeholders.
#'
#' \strong{C}ontextual-Accountable: accountability standards
#' are evaluated relative to the deployment domain and
#' the specific groups affected.
#'
#' \strong{R}esponsible: models are audited for fairness,
#' calibration, and reproducibility before deployment.
#'
#' \strong{M}achine Learning: statistical learning methods
#' applied to structured tabular data.
#'
#' @references
#' Awe OO (2025) civic.icarm: A Unified R Framework for
#' Interpretable, Civic-Accountable and Responsible Machine Learning.
#' \url{https://cran.r-project.org/package=civic.icarm}
#'
#' Breiman L (2001) Random Forests.
#' \doi{10.1023/A:1010933404324}
#'
#' Chen T, Guestrin C (2016) XGBoost.
#' \doi{10.1145/2939672.2939785}
#'
#' @keywords internal
#' @name icarm-package
"_PACKAGE"

utils::globalVariables(c(
  "grp", "y", "y_hat", "ppos", "group",
  "acc", "acc_gap", "tpr", "tpr_gap", "fpr", "fpr_gap",
  "fnr", "tnr", "ppv", "rate_pos", "mean_prob",
  "dp_ratio", "eo_gap", "reference_group",
  "err", "se", "mae", "rmse", "mae_gap", "rmse_gap",
  "feature", "importance", "importance_scaled",
  "coefficient", "contribution", "variable",
  "model_name", "interpretability", "metric",
  "metric_label", "value", "accuracy", "balanced_acc",
  "f1", "precision", "recall", "auc", "r2",
  "max_acc_gap", "max_tpr_gap", "max_fpr_gap",
  "min_dp_ratio", "max_eo_gap", "di_pass", "eo_pass",
  "n_train", "threshold", "rate_positive",
  "mean_pred", "obs_freq", "bin", "bin_lower", "bin_upper",
  "Actual", "Predicted", "Freq", "n", "where", ".data"
))

