library(testthat)
library(icarm)

# ── icarm_fit auto-detection ──────────────────────────────────
test_that("icarm_fit detects binary from factor (2 levels)", {
  df <- tibble::tibble(
    y=factor(sample(c("yes","no"),150,replace=TRUE)),
    x1=rnorm(150), x2=rnorm(150))
  m <- icarm_fit(y ~ x1+x2, df)
  expect_s3_class(m, "icarm_model")
  expect_equal(m$task, "binary")
  expect_equal(m$model, "cart")
})

test_that("icarm_fit detects multiclass from iris", {
  m <- icarm_fit(Species ~ ., iris)
  expect_s3_class(m, "icarm_model")
  expect_equal(m$task, "multiclass")
})

test_that("icarm_fit detects regression from numeric", {
  m <- icarm_fit(Sepal.Length ~ ., iris)
  expect_s3_class(m, "icarm_model")
  expect_equal(m$task, "regression")
})

test_that("icarm_fit works with mtcars regression", {
  m <- icarm_fit(mpg ~ cyl+wt+hp, mtcars, model="linear")
  expect_equal(m$model, "linear")
  expect_equal(m$task, "regression")
})

test_that("icarm_fit stores provenance metadata", {
  m <- icarm_fit(Species ~ ., iris, seed=42L)
  expect_equal(m$seed, 42L)
  expect_equal(m$n_train, 150L)
  expect_true(nchar(m$data_hash) > 10L)
  expect_true(inherits(m$trained_at, "POSIXct"))
  expect_true(m$interpretable)
})

test_that("icarm_fit marks random forest as non-interpretable", {
  skip_if_not_installed("randomForest")
  suppressWarnings(
    m <- icarm_fit(Species ~ ., iris, model="random_forest")
  )
  expect_false(m$interpretable)
})

test_that("icarm_fit rejects invalid model for task", {
  expect_error(
    icarm_fit(Species ~ ., iris, model="logistic"),
    regexp="not available"
  )
})

# ── predict.icarm_model ───────────────────────────────────────
test_that("predict returns factor for binary CART", {
  df <- tibble::tibble(
    y=factor(sample(c("A","B"),100,replace=TRUE)),
    x1=rnorm(100))
  m    <- icarm_fit(y ~ x1, df)
  yhat <- predict(m, df, type="class")
  expect_true(is.factor(yhat))
  expect_true(all(yhat %in% c("A","B")))
})

test_that("predict returns prob matrix for binary", {
  df <- tibble::tibble(
    y=factor(sample(c("A","B"),100,replace=TRUE)),
    x1=rnorm(100))
  m    <- icarm_fit(y ~ x1, df)
  prob <- predict(m, df, type="prob")
  expect_true(is.matrix(prob))
  expect_equal(ncol(prob), 2L)
  expect_true(all(prob >= 0 & prob <= 1))
})

test_that("predict returns numeric for regression", {
  m    <- icarm_fit(Sepal.Length ~ ., iris)
  yhat <- predict(m, iris)
  expect_true(is.numeric(yhat))
  expect_equal(length(yhat), 150L)
})

test_that("predict works for multiclass", {
  m    <- icarm_fit(Species ~ ., iris)
  yhat <- predict(m, iris, type="class")
  expect_true(is.factor(yhat))
  expect_true(all(levels(yhat) %in% levels(iris$Species)))
})

# ── icarm_split ───────────────────────────────────────────────
test_that("icarm_split returns correct proportions", {
  sp <- icarm_split(iris, prop=0.8, seed=1L)
  expect_equal(nrow(sp$train)+nrow(sp$test), 150L)
  expect_true(nrow(sp$train) >= 115L)
})

test_that("icarm_split stratifies correctly", {
  sp <- icarm_split(iris, prop=0.75, stratify="Species")
  expect_equal(nrow(sp$train)+nrow(sp$test), 150L)
})

# ── icarm_metrics ─────────────────────────────────────────────
test_that("icarm_metrics binary returns named vector", {
  y    <- factor(c("yes","no","yes","yes","no"))
  yhat <- factor(c("yes","no","no","yes","no"))
  m    <- icarm_metrics(y, yhat, positive="yes")
  expect_true(all(c("accuracy","f1","recall") %in% names(m)))
  expect_true(all(m >= 0 & m <= 1))
})

test_that("icarm_metrics regression returns mae/rmse/r2", {
  y    <- rnorm(100, 50, 10)
  yhat <- y + rnorm(100, 0, 5)
  m    <- icarm_metrics(y, yhat)
  expect_named(m, c("mae","rmse","r2"))
})

test_that("icarm_metrics multiclass returns macro metrics", {
  m    <- icarm_fit(Species ~ ., iris)
  yhat <- predict(m, iris, type="class")
  met  <- icarm_metrics(iris$Species, yhat)
  expect_true("accuracy" %in% names(met))
  expect_true("f1" %in% names(met))
})

# ── icarm_explain ─────────────────────────────────────────────
test_that("icarm_explain returns importance for CART", {
  m  <- icarm_fit(Species ~ ., iris)
  ex <- icarm_explain(m)
  expect_s3_class(ex, "icarm_explainer")
  expect_false(is.null(ex$importance))
  expect_true("feature" %in% names(ex$importance))
})

test_that("icarm_explain returns importance for linear", {
  m  <- icarm_fit(mpg ~ cyl+wt+hp, mtcars, model="linear")
  ex <- icarm_explain(m)
  expect_equal(ex$importance_method, "abs_coefficient")
})

# ── icarm_fairness ────────────────────────────────────────────
test_that("icarm_fairness works for regression", {
  m <- icarm_fit(Sepal.Length ~ Sepal.Width+Petal.Length, iris)
  f <- icarm_fairness(m, iris, "Sepal.Length", "Species")
  expect_s3_class(f, "icarm_fairness")
  expect_true("mae" %in% names(f))
  expect_equal(nrow(f), 3L)
})

test_that("icarm_fairness works for multiclass", {
  m  <- icarm_fit(Species ~ ., iris)
  df <- iris
  df$grp <- factor(ifelse(df$Sepal.Length>5.8,"large","small"))
  f  <- icarm_fairness(m, df, "Species", "grp")
  expect_s3_class(f, "icarm_fairness")
  expect_true("acc" %in% names(f))
})

# ── icarm_audit ───────────────────────────────────────────────
test_that("icarm_audit returns valid JSON", {
  m     <- icarm_fit(Species ~ ., iris)
  trail <- icarm_audit(m, analyst="tester", notes="test")
  expect_type(trail, "character")
  parsed <- jsonlite::fromJSON(trail)
  expect_equal(parsed$analyst, "tester")
  expect_equal(parsed$task, "multiclass")
})

# ── Plots ─────────────────────────────────────────────────────
test_that("icarm_plot_importance returns ggplot", {
  m  <- icarm_fit(Species ~ ., iris)
  ex <- icarm_explain(m)
  p  <- icarm_plot_importance(ex)
  expect_s3_class(p, "ggplot")
})

test_that("icarm_plot_confusion returns ggplot", {
  y <- factor(c("a","b","a","b","a"))
  p <- icarm_plot_confusion(y, y)
  expect_s3_class(p, "ggplot")
})

test_that("icarm_plot_fairness returns ggplot", {
  m  <- icarm_fit(Sepal.Length ~ Sepal.Width, iris)
  f  <- icarm_fairness(m, iris, "Sepal.Length", "Species")
  p  <- icarm_plot_fairness(f, metric="mae")
  expect_s3_class(p, "ggplot")
})
