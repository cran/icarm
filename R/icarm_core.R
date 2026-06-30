# ============================================================
# icarm: Explanation, Fairness, Audit, Scorecard
# ============================================================

#' Generate global model explanations
#'
#' @param object An `icarm_model` from [icarm_fit()].
#' @param data Optional data frame for DALEX explainer.
#' @param label Optional label for DALEX explainer.
#' @return An object of class `icarm_explainer`.
#' @export
#'
#' @examples
#' m  <- icarm_fit(Species ~ ., iris)
#' ex <- icarm_explain(m)
#' print(ex)
#' icarm_plot_importance(ex)
icarm_explain <- function(object, data = NULL, label = NULL) {
  .check_icarm(object)
  fit <- object$fit
  out <- list(model=object, importance=NULL,
              importance_method="none", dalex=NULL)

  # ── Importance by model type ──────────────────────────────
  if (inherits(fit, "rpart")) {
    imp <- fit$variable.importance
    if (!is.null(imp) && length(imp) > 0L) {
      out$importance <- tibble::tibble(
        feature           = names(imp),
        importance        = as.numeric(imp),
        importance_scaled = as.numeric(imp) / max(as.numeric(imp))
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "rpart_impurity"
    }
  } else if (inherits(fit, "glm") || inherits(fit, "lm")) {
    coefs <- stats::coef(fit)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    out$importance <- tibble::tibble(
      feature           = names(coefs),
      importance        = abs(as.numeric(coefs)),
      importance_scaled = abs(as.numeric(coefs)) /
        max(abs(as.numeric(coefs)) + .Machine$double.eps)
    ) |> dplyr::arrange(dplyr::desc(importance))
    out$importance_method <- "abs_coefficient"
  } else if (inherits(fit, "randomForest")) {
    if (requireNamespace("randomForest", quietly=TRUE)) {
      imp <- randomForest::importance(fit)
      imp_vals <- imp[, ncol(imp)]
      out$importance <- tibble::tibble(
        feature           = names(imp_vals),
        importance        = as.numeric(imp_vals),
        importance_scaled = as.numeric(imp_vals) /
          max(as.numeric(imp_vals) + .Machine$double.eps)
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "rf_importance"
    }
  } else if (inherits(fit, "xgb.Booster")) {
    if (requireNamespace("xgboost", quietly=TRUE)) {
      imp <- xgboost::xgb.importance(model = fit)
      if (!is.null(imp) && nrow(imp) > 0L) {
        out$importance <- tibble::tibble(
          feature           = imp$Feature,
          importance        = as.numeric(imp$Gain),
          importance_scaled = as.numeric(imp$Gain) /
            max(as.numeric(imp$Gain) + .Machine$double.eps)
        ) |> dplyr::arrange(dplyr::desc(importance))
        out$importance_method <- "xgb_gain"
      }
    }
  } else if (requireNamespace("vip", quietly=TRUE)) {
    tryCatch({
      vi <- vip::vi(fit)
      out$importance <- tibble::tibble(
        feature           = vi$Variable,
        importance        = as.numeric(vi$Importance),
        importance_scaled = as.numeric(vi$Importance) /
          max(as.numeric(vi$Importance) + .Machine$double.eps)
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "vip"
    }, error = function(e) NULL)
  }

  # ── DALEX explainer ───────────────────────────────────────
  if (!is.null(data) && requireNamespace("DALEX", quietly=TRUE)) {
    tryCatch({
      y_vec     <- data[[object$outcome]]
      feat_data <- data[, setdiff(names(data), object$outcome),
                        drop=FALSE]
      predict_fn <- if (object$task == "regression") {
        function(m, nd) as.numeric(predict.icarm_model(m, nd))
      } else {
        function(m, nd) {
          p <- predict.icarm_model(m, nd, type="prob")
          if (is.matrix(p)) p[, ncol(p)] else as.numeric(p)
        }
      }
      y_num <- if (is.factor(y_vec)) as.numeric(y_vec)-1L
               else as.numeric(y_vec)
      out$dalex <- DALEX::explain(
        model=object, data=feat_data, y=y_num,
        predict_function=predict_fn,
        label=label %||% paste0("icarm_", object$model),
        verbose=FALSE)
    }, error=function(e)
      rlang::warn(conditionMessage(e)))
  }

  class(out) <- "icarm_explainer"
  out
}

#' @export
print.icarm_explainer <- function(x, ...) {
  cat(.icarm_rule("icarm_explainer"), "\n")
  cat(sprintf("  Task   : %s\n", x$model$task))
  cat(sprintf("  Model  : %s\n", x$model$model))
  cat(sprintf("  Method : %s\n", x$importance_method))
  cat(sprintf("  DALEX  : %s\n",
              if(!is.null(x$dalex)) "available" else "not available"))
  if (!is.null(x$importance) && nrow(x$importance) > 0L) {
    cat("\n  Top features:\n")
    top <- utils::head(x$importance, 5L)
    for (i in seq_len(nrow(top))) {
      bar <- paste(rep("|", round(top$importance_scaled[i]*20L)),
                   collapse="")
      cat(sprintf("    %-22s %s\n", top$feature[i], bar))
    }
  }
  invisible(x)
}

#' Local explanation for individual observations
#'
#' @param explainer An `icarm_explainer` from [icarm_explain()].
#' @param newdata A data frame of observations to explain.
#' @param n_features Max features to show (default 10).
#' @return A list of tibbles, one per row of newdata.
#' @export
#'
#' @examples
#' m  <- icarm_fit(Species ~ ., iris)
#' ex <- icarm_explain(m)
#' icarm_explain_local(ex, iris[1:2, ])
icarm_explain_local <- function(explainer, newdata,
                                 n_features = 10L) {
  stopifnot(inherits(explainer, "icarm_explainer"))

  if (!is.null(explainer$dalex) &&
      requireNamespace("DALEX", quietly=TRUE)) {
    return(lapply(seq_len(nrow(newdata)), function(i) {
      obs <- newdata[i, , drop=FALSE]
      bd  <- tryCatch(
        DALEX::predict_parts(explainer$dalex,
                             new_observation=obs,
                             type="break_down"),
        error=function(e) NULL)
      if (is.null(bd)) return(tibble::tibble())
      tibble::as_tibble(bd) |>
        dplyr::select(dplyr::any_of(
          c("variable","contribution","cumulative",
            "variable_name","variable_value"))) |>
        utils::head(n_features)
    }))
  }

  fit <- explainer$model$fit
  if (inherits(fit, c("glm","lm"))) {
    return(lapply(seq_len(nrow(newdata)), function(i) {
      obs <- newdata[i, , drop=FALSE]
      mf  <- tryCatch(
        stats::model.matrix(explainer$model$formula, obs)[1L,],
        error=function(e) NULL)
      if (is.null(mf)) return(tibble::tibble())
      coefs <- stats::coef(fit)[names(mf)]
      tibble::tibble(
        variable=names(mf),
        coefficient=as.numeric(coefs),
        value=as.numeric(mf),
        contribution=as.numeric(coefs)*as.numeric(mf)
      ) |>
        dplyr::filter(variable != "(Intercept)") |>
        dplyr::arrange(dplyr::desc(abs(contribution))) |>
        utils::head(n_features)
    }))
  }
  list()
}


# ============================================================
# Fairness
# ============================================================

#' Group-level fairness audit
#'
#' @param object An `icarm_model`.
#' @param data A data frame with outcome and protected column.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column name.
#' @param positive Positive class (binary).
#' @param threshold Decision threshold (binary, default 0.5).
#' @return A tibble of class `icarm_fairness`.
#' @export
#'
#' @examples
#' m <- icarm_fit(Species ~ ., iris)
#' iris$size <- factor(ifelse(iris$Sepal.Length > 5.8,
#'                            "large","small"))
#' icarm_fairness(m, iris, "Species", "size")
icarm_fairness <- function(object, data, outcome, protected,
                            positive=NULL, threshold=0.5) {
  .check_icarm(object)
  if (!outcome   %in% names(data))
    rlang::abort(paste0("'", outcome, "' not found."))
  if (!protected %in% names(data))
    rlang::abort(paste0("'", protected, "' not found."))

  y   <- data[[outcome]]
  grp <- factor(data[[protected]])
  pos <- positive %||% object$positive

  if (object$task == "regression") {
    preds <- as.numeric(predict.icarm_model(object, data))
    err   <- abs(as.numeric(y) - preds)
    se    <- (as.numeric(y) - preds)^2
    tab <- dplyr::group_by(
      tibble::tibble(grp=grp, err=err, se=se), grp
    ) |>
      dplyr::summarise(n=dplyr::n(), mae=mean(err),
                       rmse=sqrt(mean(se)), .groups="drop")
    ref <- tab[which.min(tab$mae),]
    tab <- dplyr::mutate(tab,
      mae_gap  = mae  - ref$mae[1L],
      rmse_gap = rmse - ref$rmse[1L],
      reference_group = as.character(ref$grp[1L])
    )
    attr(tab,"task") <- "regression"
    attr(tab,"protected") <- protected
    attr(tab,"outcome") <- outcome
    class(tab) <- c("icarm_fairness", class(tab))
    return(tab)
  }

  y <- factor(y)
  probs <- tryCatch(
    predict.icarm_model(object, data, type="prob"),
    error=function(e) rlang::abort(conditionMessage(e)))

  if (object$task == "binary") {
    if (is.null(pos)) pos <- levels(y)[1L]
    neg <- setdiff(levels(y), pos)[1L]
    ppos <- if (is.matrix(probs)) {
      if (pos %in% colnames(probs)) probs[,pos]
      else probs[,ncol(probs)]
    } else as.numeric(probs)

    y_hat <- factor(ifelse(ppos >= threshold, pos, neg),
                    levels=levels(y))
    tab <- dplyr::group_by(
      tibble::tibble(grp=grp, y=y, y_hat=y_hat, ppos=ppos), grp
    ) |>
      dplyr::summarise(
        n=dplyr::n(),
        acc=mean(y_hat==y),
        tpr=sum(y_hat==pos & y==pos)/max(sum(y==pos),1L),
        tnr=sum(y_hat==neg & y==neg)/max(sum(y==neg),1L),
        fpr=sum(y_hat==pos & y==neg)/max(sum(y==neg),1L),
        fnr=sum(y_hat==neg & y==pos)/max(sum(y==pos),1L),
        ppv=sum(y==pos & y_hat==pos)/max(sum(y_hat==pos),1L),
        rate_pos=mean(y_hat==pos),
        mean_prob=mean(ppos),
        .groups="drop")
    ref <- tab[which.max(tab$acc),]
    tab <- dplyr::mutate(tab,
      acc_gap=acc-ref$acc[1L],
      tpr_gap=tpr-ref$tpr[1L],
      fpr_gap=fpr-ref$fpr[1L],
      dp_ratio=rate_pos/max(ref$rate_pos[1L],.Machine$double.eps),
      eo_gap=pmax(abs(tpr_gap),abs(fpr_gap)),
      reference_group=as.character(ref$grp[1L]))
    attr(tab,"task") <- "binary"
    attr(tab,"positive") <- pos
    attr(tab,"threshold") <- threshold
    attr(tab,"protected") <- protected
    attr(tab,"outcome") <- outcome
    class(tab) <- c("icarm_fairness", class(tab))
    return(tab)
  }

  # Multiclass
  y_hat <- factor(
    object$levels[max.col(probs, ties.method="first")],
    levels=object$levels)
  tab <- dplyr::group_by(
    tibble::tibble(grp=grp, y=y, y_hat=y_hat), grp
  ) |>
    dplyr::summarise(
      n=dplyr::n(), acc=mean(y_hat==y),
      balanced_acc={
        lvls <- levels(y)
        mean(sapply(lvls, function(cls)
          sum(y_hat==cls & y==cls)/max(sum(y==cls),1L)))
      }, .groups="drop")
  ref <- tab[which.max(tab$acc),]
  tab <- dplyr::mutate(tab,
    acc_gap=acc-ref$acc[1L],
    reference_group=as.character(ref$grp[1L]))
  attr(tab,"task") <- "multiclass"
  attr(tab,"protected") <- protected
  attr(tab,"outcome") <- outcome
  class(tab) <- c("icarm_fairness", class(tab))
  tab
}

#' @export
print.icarm_fairness <- function(x, ...) {
  cat(.icarm_rule("icarm_fairness"), "\n")
  cat(sprintf("  Protected : %s\n", attr(x,"protected") %||% "?"))
  cat(sprintf("  Outcome   : %s\n", attr(x,"outcome")   %||% "?"))
  cat(sprintf("  Task      : %s\n", attr(x,"task")      %||% "?"))
  cat("\n")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' Equity summary from a fairness report
#'
#' @param fairness An `icarm_fairness` from [icarm_fairness()].
#' @return A named list of scalar equity indicators.
#' @export
icarm_equity_summary <- function(fairness) {
  stopifnot(inherits(fairness, "icarm_fairness"))
  task <- attr(fairness,"task") %||% "binary"
  if (task == "binary") {
    list(
      n_groups              = nrow(fairness),
      max_acc_gap           = max(abs(fairness$acc_gap)),
      max_tpr_gap           = max(abs(fairness$tpr_gap)),
      max_fpr_gap           = max(abs(fairness$fpr_gap)),
      min_dp_ratio          = min(fairness$dp_ratio),
      max_eo_gap            = max(fairness$eo_gap),
      disparate_impact_pass = all(fairness$dp_ratio >= 0.8),
      equal_opp_pass        = max(abs(fairness$tpr_gap)) < 0.1
    )
  } else if (task == "multiclass") {
    list(n_groups=nrow(fairness),
         max_acc_gap=max(abs(fairness$acc_gap)))
  } else {
    list(n_groups=nrow(fairness),
         max_mae_gap=max(abs(fairness$mae_gap)),
         max_rmse_gap=max(abs(fairness$rmse_gap)))
  }
}

#' Equalized odds curves across thresholds
#'
#' @param object An `icarm_model` (binary only).
#' @param data A data frame.
#' @param outcome Character outcome column.
#' @param protected Character protected attribute column.
#' @param positive Positive class label.
#' @param thresholds Numeric threshold vector.
#' @return A tibble with threshold, group, tpr, fpr, tnr.
#' @export
icarm_equalized_odds_curve <- function(object, data, outcome,
                                        protected, positive=NULL,
                                        thresholds=seq(0.05,0.95,0.05)) {
  .check_icarm(object)
  if (object$task != "binary")
    rlang::abort("Requires binary classification model.")
  y   <- factor(data[[outcome]])
  grp <- factor(data[[protected]])
  pos <- positive %||% object$positive %||% levels(y)[1L]
  neg <- setdiff(levels(y), pos)[1L]
  probs <- predict.icarm_model(object, data, type="prob")
  ppos  <- if (is.matrix(probs)) {
    if (pos %in% colnames(probs)) probs[,pos] else probs[,ncol(probs)]
  } else as.numeric(probs)

  purrr::map_dfr(levels(grp), function(g) {
    idx <- grp==g; y_g <- y[idx]; p_g <- ppos[idx]
    purrr::map_dfr(thresholds, function(thr) {
      yhat_g <- factor(ifelse(p_g>=thr,pos,neg), levels=levels(y))
      cm <- .confusion(y_g, yhat_g, pos)
      tibble::tibble(threshold=thr, group=g,
                     tpr=cm$tpr, fpr=cm$fpr, tnr=cm$tnr)
    })
  })
}


# ============================================================
# Calibration
# ============================================================

#' Probability calibration diagnostics
#'
#' @param object An `icarm_model` (binary only).
#' @param data A data frame.
#' @param outcome Character outcome column.
#' @param positive Positive class label.
#' @param n_bins Number of bins (default 10).
#' @return An object of class `icarm_calibration`.
#' @export
#'
#' @examples
#' m   <- icarm_fit(
#'   Petal.Width ~ Sepal.Length + Sepal.Width, iris,
#'   model = "linear")
#' # calibration only for binary:
#' data(icarm_medical)
#' m2  <- icarm_fit(readmitted ~ ., icarm_medical)
#' cal <- icarm_calibrate(m2, icarm_medical, "readmitted", "Yes")
#' print(cal)
icarm_calibrate <- function(object, data, outcome,
                              positive=NULL, n_bins=10L) {
  .check_icarm(object)
  if (object$task != "binary")
    rlang::abort("Calibration requires binary classification.")
  y   <- factor(data[[outcome]])
  pos <- positive %||% object$positive %||% levels(y)[1L]
  probs <- predict.icarm_model(object, data, type="prob")
  ppos  <- if (is.matrix(probs)) {
    if (pos %in% colnames(probs)) probs[,pos] else probs[,ncol(probs)]
  } else as.numeric(probs)

  y_bin  <- as.integer(y == pos)
  brier  <- mean((ppos - y_bin)^2)
  breaks <- seq(0, 1, length.out=n_bins+1L)
  b_idx  <- pmin(findInterval(ppos, breaks, rightmost.closed=TRUE),
                 n_bins)

  bins <- purrr::map_dfr(seq_len(n_bins), function(b) {
    idx <- b_idx==b; n_b <- sum(idx)
    tibble::tibble(
      bin=b, bin_lower=breaks[b], bin_upper=breaks[b+1L],
      bin_mid=(breaks[b]+breaks[b+1L])/2, n=n_b,
      mean_pred=if(n_b>0L) mean(ppos[idx]) else NA_real_,
      obs_freq =if(n_b>0L) mean(y_bin[idx]) else NA_real_
    )
  }) |> dplyr::filter(!is.na(mean_pred))

  ece <- with(bins,
    sum(n*abs(mean_pred-obs_freq), na.rm=TRUE)/sum(n))

  out <- list(bins=bins, brier_score=round(brier,4L),
              ece=round(ece,4L), positive=pos,
              outcome=outcome, model=object)
  class(out) <- "icarm_calibration"
  out
}

#' @export
print.icarm_calibration <- function(x, ...) {
  cat(.icarm_rule("icarm_calibration"), "\n")
  cat(sprintf("  Model       : %s / %s\n",
              x$model$task, x$model$model))
  cat(sprintf("  Brier score : %.4f\n", x$brier_score))
  cat(sprintf("  ECE         : %.4f  %s\n", x$ece,
              if(x$ece<0.05) "(GOOD)"
              else if(x$ece<0.10) "(MODERATE)" else "(POOR)"))
  invisible(x)
}


# ============================================================
# Model comparison
# ============================================================

#' Compare multiple icarm_models
#'
#' @param models A named list of `icarm_model` objects.
#' @param test_data A data frame for evaluation.
#' @param outcome Character outcome column.
#' @param protected Optional protected attribute for fairness.
#' @param positive Positive class (binary).
#' @param threshold Decision threshold (binary, default 0.5).
#' @return A tibble of class `icarm_comparison`.
#' @export
#'
#' @examples
#' sp <- icarm_split(iris, stratify = "Species")
#' m1 <- icarm_fit(Species ~ ., sp$train, model = "cart")
#' m2 <- icarm_fit(Species ~ ., sp$train, model = "multinomial")
#' cmp <- icarm_compare(list(CART=m1, Multinom=m2),
#'                      sp$test, outcome="Species")
#' print(cmp)
icarm_compare <- function(models, test_data, outcome,
                           protected=NULL, positive=NULL,
                           threshold=0.5) {
  if (!is.list(models) || is.null(names(models)))
    rlang::abort("'models' must be a named list.")

  purrr::imap_dfr(models, function(obj, nm) {
    .check_icarm(obj)
    y_true <- test_data[[outcome]]

    if (obj$task %in% c("binary","multiclass")) {
      y_hat <- tryCatch(
        predict.icarm_model(obj, test_data, type="class",
                            threshold=threshold),
        error=function(e) rep(NA, nrow(test_data)))
      y_prob <- if (obj$task=="binary") tryCatch({
        p   <- predict.icarm_model(obj, test_data, type="prob")
        pos <- positive %||% obj$positive %||%
               levels(factor(y_true))[1L]
        if (is.matrix(p)) p[,pos] else as.numeric(p)
      }, error=function(e) NULL) else NULL
      perf <- icarm_metrics(y_true, y_hat, y_prob=y_prob,
                            positive=positive, type=obj$task)
    } else {
      y_hat <- tryCatch(
        predict.icarm_model(obj, test_data),
        error=function(e) rep(NA_real_, nrow(test_data)))
      perf <- icarm_metrics(y_true, y_hat, type="regression")
    }

    perf_row <- tibble::tibble(
      accuracy=perf["accuracy"] %||% NA_real_,
      balanced_acc=perf["balanced_acc"] %||% NA_real_,
      f1=perf["f1"] %||% NA_real_,
      precision=perf["precision"] %||% NA_real_,
      recall=perf["recall"] %||% NA_real_,
      auc=perf["auc"] %||% NA_real_,
      mae=perf["mae"] %||% NA_real_,
      rmse=perf["rmse"] %||% NA_real_,
      r2=perf["r2"] %||% NA_real_
    )

    fair_row <- tibble::tibble(
      max_acc_gap=NA_real_, max_tpr_gap=NA_real_,
      min_dp_ratio=NA_real_, di_pass=NA, eo_pass=NA
    )
    if (!is.null(protected)) {
      tryCatch({
        fair <- icarm_fairness(obj, test_data, outcome=outcome,
                               protected=protected,
                               positive=positive,
                               threshold=threshold)
        eq <- icarm_equity_summary(fair)
        fair_row <- tibble::tibble(
          max_acc_gap  = eq$max_acc_gap  %||% NA_real_,
          max_tpr_gap  = eq$max_tpr_gap  %||% NA_real_,
          min_dp_ratio = eq$min_dp_ratio %||% NA_real_,
          di_pass      = eq$disparate_impact_pass %||% NA,
          eo_pass      = eq$equal_opp_pass        %||% NA
        )
      }, error=function(e) NULL)
    }

    dplyr::bind_cols(
      tibble::tibble(
        model_name=nm, learner=obj$model,
        interpretability=.interp_label(obj$model),
        icarm_compliant=obj$interpretable,
        n_train=obj$n_train
      ),
      perf_row, fair_row
    )
  }) |> (\(x) {class(x)<-c("icarm_comparison",class(x));x})()
}

#' @export
print.icarm_comparison <- function(x, digits=3L, ...) {
  cat(.icarm_rule("icarm_comparison"), "\n\n")
  num_cols <- names(x)[sapply(x, is.numeric)]
  xp <- x; xp[num_cols] <- lapply(xp[num_cols], round, digits)
  print(tibble::as_tibble(xp), n=Inf, ...)
  invisible(x)
}


# ============================================================
# Audit and Scorecard
# ============================================================

#' Generate a JSON audit trail
#'
#' @param object An `icarm_model`.
#' @param metrics Named numeric vector from [icarm_metrics()].
#' @param fairness An `icarm_fairness` from [icarm_fairness()].
#' @param notes Character analyst notes.
#' @param analyst Character analyst name.
#' @param path File path to write JSON (optional).
#' @return Invisibly, the JSON string.
#' @export
#'
#' @examples
#' m <- icarm_fit(Species ~ ., iris)
#' trail <- icarm_audit(m, analyst = "O. O. Awe")
#' cat(trail)
icarm_audit <- function(object, metrics=NULL, fairness=NULL,
                         notes=NULL, analyst=NULL, path=NULL) {
  .check_icarm(object)
  eq <- if (!is.null(fairness) && inherits(fairness,"icarm_fairness"))
    tryCatch(icarm_equity_summary(fairness), error=function(e) NULL)
  else NULL

  meta <- list(
    icarm_version = tryCatch(
      as.character(utils::packageVersion("icarm")),
      error=function(e) "dev"),
    timestamp     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    analyst       = analyst %||% Sys.info()[["user"]],
    task          = object$task,
    model         = object$model,
    interpretable = object$interpretable,
    outcome       = object$outcome,
    formula       = deparse(object$formula),
    seed          = object$seed,
    n_train       = object$n_train,
    n_features    = object$n_features,
    feature_names = object$feature_names,
    data_hash     = object$data_hash,
    trained_at    = format(object$trained_at,
                           "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    metrics       = if (!is.null(metrics)) as.list(metrics) else NULL,
    equity        = eq,
    notes         = notes
  )

  json <- jsonlite::toJSON(meta, pretty=TRUE,
                           auto_unbox=TRUE, null="null")
  if (!is.null(path)) {
    writeLines(json, path)
    rlang::inform(paste0("Audit trail saved to: ", path))
  }
  invisible(json)
}


#' Generate a full accountability scorecard
#'
#' @param object An `icarm_model`.
#' @param test_data Data frame of test data.
#' @param outcome Character outcome column.
#' @param protected Optional protected attribute column.
#' @param positive Positive class (binary).
#' @param analyst Character analyst name.
#' @param project Character project name.
#' @param path Optional JSON output path.
#' @return Invisibly, the scorecard list.
#' @export
#'
#' @examples
#' sp <- icarm_split(iris, stratify = "Species")
#' m  <- icarm_fit(Species ~ ., sp$train)
#' iris_test <- sp$test
#' iris_test$size <- factor(
#'   ifelse(iris_test$Sepal.Length > 5.8, "large","small"))
#' icarm_scorecard(m, iris_test, outcome="Species",
#'                 protected="size", project="Iris Demo")
icarm_scorecard <- function(object, test_data, outcome,
                             protected=NULL, positive=NULL,
                             analyst=NULL, project="icarm",
                             path=NULL) {
  .check_icarm(object)
  y_true <- test_data[[outcome]]

  if (object$task %in% c("binary","multiclass")) {
    y_hat  <- predict.icarm_model(object, test_data, type="class")
    y_prob <- if (object$task=="binary") tryCatch({
      p   <- predict.icarm_model(object, test_data, type="prob")
      pos <- positive %||% object$positive
      if (is.matrix(p) && !is.null(pos) && pos %in% colnames(p))
        p[,pos] else if (is.matrix(p)) p[,ncol(p)]
      else as.numeric(p)
    }, error=function(e) NULL) else NULL
    perf <- icarm_metrics(y_true, y_hat, y_prob=y_prob,
                          positive=positive, type=object$task)
  } else {
    y_hat <- predict.icarm_model(object, test_data)
    perf  <- icarm_metrics(y_true, y_hat, type="regression")
  }

  fair <- eq <- NULL
  if (!is.null(protected)) {
    tryCatch({
      fair <- icarm_fairness(object, test_data, outcome=outcome,
                             protected=protected, positive=positive)
      eq   <- icarm_equity_summary(fair)
    }, error=function(e) NULL)
  }

  cat(.icarm_rule(paste0("ICARM SCORECARD -- ",toupper(project))),
      "\n\n")
  cat(sprintf("  Analyst   : %s\n",
              analyst %||% Sys.info()[["user"]]))
  cat(sprintf("  Generated : %s\n",
              format(Sys.time(),"%Y-%m-%d %H:%M UTC",tz="UTC")))
  cat(sprintf("  Task      : %s | Model: %s\n",
              object$task, object$model))
  cat(sprintf("  Formula   : %s\n", deparse(object$formula)))
  cat(sprintf("  Interpretable: %s\n",
              ifelse(object$interpretable,"YES","NO (post-hoc needed)")))
  cat(sprintf("  N train/test : %d / %d\n",
              object$n_train, nrow(test_data)))
  cat("\n  [Performance]\n")
  for (nm in names(perf))
    cat(sprintf("    %-20s: %.4f\n", nm, perf[nm]))
  if (!is.null(eq)) {
    cat("\n  [Equity]\n")
    for (nm in names(eq)) {
      val <- eq[[nm]]
      cat(sprintf("    %-32s: %s\n", nm,
                  if(is.logical(val))
                    ifelse(isTRUE(val),"PASS","FAIL")
                  else round(as.numeric(val),4L)))
    }
  }
  cat("\n", .icarm_rule("end scorecard"), "\n")

  sc <- list(project=project,
             analyst=analyst %||% Sys.info()[["user"]],
             model=list(task=object$task,learner=object$model,
                        interpretable=object$interpretable),
             performance=as.list(round(perf,4L)),
             equity=eq)
  if (!is.null(path)) {
    writeLines(jsonlite::toJSON(sc, pretty=TRUE,
                                auto_unbox=TRUE, null="null"), path)
    rlang::inform(paste0("Scorecard saved to: ", path))
  }
  invisible(sc)
}
