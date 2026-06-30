# ============================================================
# icarm: Unified model fitting
# ============================================================

#' Fit an ICARM model on any tabular data
#'
#' @description
#' Single unified entry point for all icarm modelling. Automatically
#' detects the prediction task from your target variable and supports
#' both interpretable and extended (black-box) model families.
#'
#' **Task auto-detection:**
#' | Target type | Task |
#' |---|---|
#' | numeric / integer | regression |
#' | factor / character, 2 levels | binary classification |
#' | factor / character, 3+ levels | multi-class classification |
#'
#' **Interpretable models (ICARM-compliant):**
#' - `"cart"` — Classification/Regression Tree (rpart)
#' - `"logistic"` — Logistic regression (binary)
#' - `"logistic_l1"` — L1-penalised logistic (glmnet)
#' - `"linear"` — Linear regression
#' - `"gam"` — Generalised Additive Model (mgcv)
#' - `"multinomial"` — Multinomial logistic (nnet)
#'
#' **Extended models (requires post-hoc explanation):**
#' - `"random_forest"` — Random forest (randomForest)
#' - `"xgboost"` — Gradient boosting (xgboost)
#' - `"svm"` — Support vector machine (e1071)
#'
#' @param formula A model formula, e.g. `outcome ~ .` or
#'   `outcome ~ x1 + x2`.
#' @param data A `data.frame` or `tibble`.
#' @param task One of `"auto"` (default), `"binary"`,
#'   `"multiclass"`, or `"regression"`.
#' @param model Character. Model type. Use `"auto"` for CART
#'   (default), or specify any model from the list above.
#' @param seed Integer random seed for reproducibility (default 2025).
#' @param positive Positive class label for binary classification.
#' @param cart_control Optional [rpart::rpart.control()] for CART.
#' @param ... Additional arguments passed to the underlying fitter.
#'
#' @return An S3 object of class `icarm_model` with full provenance.
#'
#' @export
#'
#' @examples
#' # Works on any data — task auto-detected
#' m1 <- icarm_fit(Species ~ ., iris)              # multiclass
#' m2 <- icarm_fit(Sepal.Length ~ ., iris)         # regression
#'
#' # Extended models
#' m3 <- icarm_fit(Species ~ ., iris,
#'                 model = "random_forest")
#'
#' # Built-in datasets
#' data(icarm_medical)
#' m4 <- icarm_fit(readmitted ~ ., icarm_medical,
#'                 model = "cart")
icarm_fit <- function(formula,
                      data,
                      task         = "auto",
                      model        = "auto",
                      seed         = 2025L,
                      positive     = NULL,
                      cart_control = NULL,
                      ...) {

  if (!is.data.frame(data))
    rlang::abort("`data` must be a data.frame or tibble.")
  if (nrow(data) < 10L)
    rlang::abort("`data` must have at least 10 rows.")

  set.seed(seed)
  outcome <- all.vars(formula)[1L]

  if (!outcome %in% names(data))
    rlang::abort(paste0("Outcome '", outcome, "' not found in data."))

  y_raw <- data[[outcome]]

  # ── Task detection ────────────────────────────────────────
  if (task == "auto") {
    task <- .detect_task(y_raw)
    rlang::inform(paste0("Task auto-detected: ", task,
                         " (target = '", outcome, "')"))
  }
  task <- match.arg(task,
                    c("binary", "multiclass", "regression"))

  data[[outcome]] <- .prepare_y(y_raw, task)
  y <- data[[outcome]]

  # ── Model selection ───────────────────────────────────────
  if (model == "auto") {
    model <- "cart"
    rlang::inform("Model auto-selected: cart")
  }

  valid_models <- switch(task,
    binary     = c("cart", "logistic", "logistic_l1",
                   "random_forest", "xgboost", "svm"),
    multiclass = c("cart", "multinomial",
                   "random_forest", "xgboost"),
    regression = c("cart", "linear", "gam",
                   "random_forest", "xgboost", "svm")
  )

  if (!model %in% valid_models)
    rlang::abort(paste0(
      "model = '", model, "' not available for task = '", task,
      "'.\nValid: ", paste(valid_models, collapse = ", ")))

  # ── ICARM compliance warning ──────────────────────────────
  if (!.is_interpretable(model)) {
    rlang::warn(c(
      paste0("Model '", model, "' is not inherently interpretable."),
      i = "Use icarm_explain() with DALEX for post-hoc explanation.",
      i = "Consider an interpretable model for ICARM compliance."
    ))
  }

  # ── Feature metadata ──────────────────────────────────────
  mf           <- stats::model.frame(formula, data = data)
  feature_names <- setdiff(names(mf), outcome)
  n_features   <- length(feature_names)

  ctrl <- cart_control %||%
    rpart::rpart.control(cp = 0.01, minsplit = 20L)

  # ── Fit ───────────────────────────────────────────────────
  fit <- switch(paste(task, model, sep = "|"),

    # ── Interpretable: Binary ───────────────────────────────
    "binary|cart" =
      rpart::rpart(formula, data = data,
                   method = "class", control = ctrl, ...),
    "binary|logistic" =
      stats::glm(formula, data = data,
                 family = stats::binomial(), ...),
    "binary|logistic_l1" = {
      if (!requireNamespace("glmnet", quietly = TRUE))
        rlang::abort("Install glmnet: install.packages('glmnet')")
      X <- stats::model.matrix(formula, data = mf)[,-1L, drop = FALSE]
      glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1L, ...)
    },

    # ── Interpretable: Multiclass ───────────────────────────
    "multiclass|cart" =
      rpart::rpart(formula, data = data,
                   method = "class", control = ctrl, ...),
    "multiclass|multinomial" = {
      if (!requireNamespace("nnet", quietly = TRUE))
        rlang::abort("Install nnet: install.packages('nnet')")
      nnet::multinom(formula, data = data, trace = FALSE, ...)
    },

    # ── Interpretable: Regression ───────────────────────────
    "regression|cart" =
      rpart::rpart(formula, data = data,
                   method = "anova", control = ctrl, ...),
    "regression|linear" =
      stats::lm(formula, data = data, ...),
    "regression|gam" = {
      if (!requireNamespace("mgcv", quietly = TRUE))
        rlang::abort("Install mgcv: install.packages('mgcv')")
      mgcv::gam(formula, data = data, ...)
    },

    # ── Extended: Random Forest ─────────────────────────────
    "binary|random_forest" = ,
    "multiclass|random_forest" = ,
    "regression|random_forest" = {
      if (!requireNamespace("randomForest", quietly = TRUE))
        rlang::abort("Install randomForest: install.packages('randomForest')")
      randomForest::randomForest(formula, data = data, ...)
    },

    # ── Extended: XGBoost ───────────────────────────────────
    "binary|xgboost" = ,
    "multiclass|xgboost" = ,
    "regression|xgboost" = {
      if (!requireNamespace("xgboost", quietly = TRUE))
        rlang::abort("Install xgboost: install.packages('xgboost')")
      X    <- stats::model.matrix(formula, data = mf)[,-1L, drop=FALSE]
      y_xg <- if (task == "regression") as.numeric(y)
               else as.numeric(y) - 1L
      obj  <- if (task == "regression") "reg:squarederror"
               else if (task == "binary") "binary:logistic"
               else "multi:softprob"
      params <- list(objective = obj, eta = 0.1,
                     max_depth = 6L, nrounds = 100L)
      if (task == "multiclass")
        params$num_class <- nlevels(y)
      xgboost::xgboost(
        data  = xgboost::xgb.DMatrix(X, label = y_xg),
        params = params,
        nrounds = 100L,
        verbose = 0L, ...
      )
    },

    # ── Extended: SVM ───────────────────────────────────────
    "binary|svm" = ,
    "multiclass|svm" = ,
    "regression|svm" = {
      if (!requireNamespace("e1071", quietly = TRUE))
        rlang::abort("Install e1071: install.packages('e1071')")
      e1071::svm(formula, data = data,
                 probability = (task != "regression"), ...)
    },

    rlang::abort(paste0("Unknown: task='", task,
                        "', model='", model, "'"))
  )

  # ── Positive class ────────────────────────────────────────
  lvls <- if (task %in% c("binary","multiclass")) levels(y) else NULL
  if (task == "binary") {
    positive <- positive %||% lvls[1L]
  }

  structure(
    list(
      fit           = fit,
      task          = task,
      model         = model,
      formula       = formula,
      outcome       = outcome,
      levels        = lvls,
      positive      = positive,
      seed          = seed,
      n_train       = nrow(data),
      n_features    = n_features,
      feature_names = feature_names,
      data_hash     = digest::digest(data, algo = "sha256"),
      trained_at    = Sys.time(),
      interpretable = .is_interpretable(model),
      call          = match.call()
    ),
    class = "icarm_model"
  )
}

# ── S3 methods ────────────────────────────────────────────────

#' Print an icarm_model
#' @param x An icarm_model object.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the icarm_model object x. Called for
#'   its side effect of printing a formatted summary to the console.
#' @export
print.icarm_model <- function(x, ...) {
  cat(.icarm_rule("icarm_model"), "\n")
  cat(sprintf("  Task           : %s\n", x$task))
  cat(sprintf("  Model          : %s\n", x$model))
  cat(sprintf("  Outcome        : %s\n", x$outcome))
  if (!is.null(x$levels))
    cat(sprintf("  Classes        : %s\n",
                paste(x$levels, collapse = ", ")))
  if (!is.null(x$positive))
    cat(sprintf("  Positive class : %s\n", x$positive))
  cat(sprintf("  Features       : %d\n", x$n_features))
  cat(sprintf("  N train        : %d\n", x$n_train))
  cat(sprintf("  Seed           : %d\n", x$seed))
  cat(sprintf("  Trained        : %s\n",
              format(x$trained_at, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("  Data hash      : %s\n",
              substr(x$data_hash, 1L, 20L)))
  cat(sprintf("  Interpretable  : %s\n",
              ifelse(x$interpretable, "YES (ICARM)", "NO (post-hoc needed)")))
  cat(sprintf("  Rating         : %s\n", .interp_label(x$model)))
  invisible(x)
}

#' Summary of an icarm_model
#' @param object An icarm_model object.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the summary of the underlying fitted
#'   model object. Called for its side effect of printing a detailed
#'   model summary to the console.
#' @export
summary.icarm_model <- function(object, ...) {
  cat(sprintf("\nicarm_model [%s / %s]\n\n",
              object$task, object$model))
  summary(object$fit, ...)
}

#' Predict from an icarm_model
#'
#' @param object An `icarm_model`.
#' @param newdata A data frame for prediction.
#' @param type For classification: `"class"` or `"prob"`.
#'   For regression: ignored.
#' @param threshold Decision threshold for binary (default 0.5).
#' @param ... Ignored.
#' @return Factor vector, probability matrix, or numeric vector.
#' @export
#'
#' @examples
#' m <- icarm_fit(Species ~ ., iris)
#' predict(m, iris[1:5, ], type = "class")
predict.icarm_model <- function(object, newdata,
                                type      = c("class", "prob"),
                                threshold = 0.5, ...) {
  .check_icarm(object)
  type <- match.arg(type)

  # ── Regression ────────────────────────────────────────────
  if (object$task == "regression") {
    if (object$model == "xgboost") {
      mf <- stats::model.frame(object$formula, data = newdata)
      X  <- stats::model.matrix(object$formula, data = mf)[,-1L, drop=FALSE]
      return(as.numeric(stats::predict(
        object$fit, xgboost::xgb.DMatrix(X))))
    }
    return(as.numeric(stats::predict(object$fit, newdata = newdata)))
  }

  # ── CART ──────────────────────────────────────────────────
  if (object$model == "cart") {
    probs <- stats::predict(object$fit, newdata = newdata,
                            type = "prob")
    if (!is.matrix(probs)) probs <- as.matrix(probs)
    if (type == "prob") return(probs)
    if (object$task == "binary" && !is.null(object$positive)) {
      pos <- object$positive
      neg <- setdiff(object$levels, pos)[1L]
      cls <- ifelse(probs[, pos] >= threshold, pos, neg)
    } else {
      cls <- colnames(probs)[max.col(probs, ties.method="first")]
    }
    return(factor(cls, levels = object$levels))
  }

  # ── Logistic ──────────────────────────────────────────────
  if (object$model == "logistic") {
    p   <- stats::predict(object$fit, newdata = newdata,
                          type = "response")
    pos <- object$positive
    neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1-p, p); colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    return(factor(ifelse(p >= threshold, pos, neg),
                  levels = object$levels))
  }

  # ── L1 logistic ───────────────────────────────────────────
  if (object$model == "logistic_l1") {
    if (!requireNamespace("glmnet", quietly=TRUE))
      rlang::abort("glmnet required")
    mf <- stats::model.frame(object$formula, data = newdata)
    X  <- stats::model.matrix(object$formula, data = mf)[,-1L, drop=FALSE]
    p  <- as.numeric(stats::predict(object$fit, newx = X,
                                    type="response", s="lambda.min"))
    pos <- object$positive; neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1-p, p); colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    return(factor(ifelse(p >= threshold, pos, neg),
                  levels = object$levels))
  }

  # ── Multinomial ───────────────────────────────────────────
  if (object$model == "multinomial") {
    if (!requireNamespace("nnet", quietly=TRUE))
      rlang::abort("nnet required")
    probs <- stats::predict(object$fit, newdata=newdata,
                            type="probs")
    if (is.vector(probs)) probs <- matrix(probs, nrow=1L)
    if (type == "prob") return(probs)
    cls <- object$levels[max.col(probs, ties.method="first")]
    return(factor(cls, levels=object$levels))
  }

  # ── Random Forest ─────────────────────────────────────────
  if (object$model == "random_forest") {
    if (!requireNamespace("randomForest", quietly=TRUE))
      rlang::abort("randomForest required")
    if (type == "prob" && object$task == "binary") {
      probs <- stats::predict(object$fit, newdata=newdata,
                              type="prob")
      return(probs)
    }
    preds <- stats::predict(object$fit, newdata=newdata)
    if (object$task == "binary")
      return(factor(as.character(preds), levels=object$levels))
    return(preds)
  }

  # ── XGBoost ───────────────────────────────────────────────
  if (object$model == "xgboost") {
    if (!requireNamespace("xgboost", quietly=TRUE))
      rlang::abort("xgboost required")
    mf <- stats::model.frame(object$formula, data=newdata)
    X  <- stats::model.matrix(object$formula, data=mf)[,-1L, drop=FALSE]
    raw <- stats::predict(object$fit,
                          xgboost::xgb.DMatrix(X))
    if (object$task == "binary") {
      pos <- object$positive; neg <- setdiff(object$levels, pos)[1L]
      probs <- cbind(1-raw, raw); colnames(probs) <- c(neg, pos)
      if (type == "prob") return(probs)
      return(factor(ifelse(raw >= threshold, pos, neg),
                    levels=object$levels))
    }
    if (object$task == "multiclass") {
      nc    <- nlevels(factor(object$levels))
      probs <- matrix(raw, ncol=nc, byrow=TRUE)
      colnames(probs) <- object$levels
      if (type == "prob") return(probs)
      cls <- object$levels[max.col(probs, ties.method="first")]
      return(factor(cls, levels=object$levels))
    }
  }

  # ── SVM ───────────────────────────────────────────────────
  if (object$model == "svm") {
    if (!requireNamespace("e1071", quietly=TRUE))
      rlang::abort("e1071 required")
    if (type == "prob" && object$task == "binary") {
      probs <- attr(stats::predict(object$fit, newdata=newdata,
                                   probability=TRUE), "probabilities")
      return(probs)
    }
    preds <- stats::predict(object$fit, newdata=newdata)
    if (object$task %in% c("binary","multiclass"))
      return(factor(as.character(preds), levels=object$levels))
    return(as.numeric(preds))
  }

  rlang::abort(paste0("predict not implemented for model: ",
                      object$model))
}
